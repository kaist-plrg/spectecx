(** P4 Typechecker target - Implements TARGET and TASK for P4 typechecking *)

module Builtins = Builtins_p4
module Frontend = Frontend_p4

(* Files that are includes, not test inputs *)
let skip_dirs = [ "include"; "includes" ]

(* Simple substring check *)
let contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

(* Recursively collect files with given suffix from directory *)
let collect_files_recursive ~suffix dir =
  let rec gather acc path =
    if Sys.file_exists path && Sys.is_directory path then (
      let entries = Sys.readdir path in
      Array.sort String.compare entries;
      Array.fold_left
        (fun acc name ->
          let full_path = Filename.concat path name in
          if List.mem name skip_dirs then acc else gather acc full_path)
        acc entries)
    else if Filename.check_suffix path suffix then path :: acc
    else acc
  in
  if Sys.file_exists dir then gather [] dir |> List.rev else []

(* Upstream .exclude lines carry a [p4c/testdata/] prefix the local paths lack. *)
let normalize_exclude pattern =
  let upstream_prefix = "p4c/testdata/" in
  if String.starts_with ~prefix:upstream_prefix pattern then
    String.sub pattern
      (String.length upstream_prefix)
      (String.length pattern - String.length upstream_prefix)
  else pattern

(* Load exclude patterns from .exclude files recursively *)
let load_excludes dir =
  let exclude_files = collect_files_recursive ~suffix:".exclude" dir in
  List.concat_map
    (fun path ->
      let ic = open_in path in
      let rec read_lines acc =
        try
          let line = input_line ic |> String.trim in
          if String.length line = 0 || line.[0] = '#' then read_lines acc
          else read_lines (normalize_exclude line :: acc)
        with End_of_file ->
          close_in ic;
          acc
      in
      read_lines [])
    exclude_files

(* Check if a path should be excluded - matches against test-relative path *)
let is_excluded excludes path =
  List.exists (fun pattern -> contains_substring path pattern) excludes

module Target : Spectec.Target.S = struct
  let name = "p4"

  let spec_dir =
    match P4_sites.Sites.specs with dir :: _ -> dir | [] -> "spectec/specs/p4"

  let builtins = Builtins.builtins @ Builtins.Ebpf.builtins
  let opaque_types = Builtins.Ebpf.opaque_types

  (* The cache drops the result of a call that advanced [tid_counter], unless
     [is_impure_func] or [is_impure_rel] accepts the callee's name. *)
  let tid_counter = ref 0
  let state_version = tid_counter

  let with_state f =
    let previous_tid_counter = !tid_counter in
    tid_counter := 0;
    let fresh_tid () =
      let tid = "FRESH__" ^ string_of_int !tid_counter in
      incr tid_counter;
      tid
    in
    Fun.protect
      (fun () -> Builtins.Fresh.GlobalTidProvider.with_provider fresh_tid f)
      ~finally:(fun () -> tid_counter := previous_tid_counter)

  (* The cache stores the result of a call to one of these functions or
     relations even when the call advanced [tid_counter]. *)
  let is_impure_func = function
    | "bound" | "merge_constraint" | "merge_constraint'" | "nestable_struct"
    | "nestable_struct_in_header" | "find_map" ->
        true
    | _ -> false

  let is_impure_rel = function
    | "Cast_expl" | "Cast_impl" | "Type_wf" | "Type_alpha" -> true
    | _ -> false
end

(* P4 Typechecker task - extends TASK with make function for CLI *)
module Typecheck = struct
  let name = "typechecker"

  module Target = Target

  type input = {
    includes : string list;
    filename : string;
    expect : Spectec.Task.expectation;
  }

  let collect ?dir () =
    match dir with
    | None -> []
    | Some base ->
        (* includes live under the corpus base; excludes sit beside it *)
        let includes_dir = Filename.concat base "includes" in
        let excludes_dir = Filename.concat (Filename.dirname base) "excludes" in
        let excludes = load_excludes excludes_dir in
        collect_files_recursive ~suffix:".p4" base
        |> List.filter (fun filename -> not (is_excluded excludes filename))
        |> List.map (fun filename ->
               let expect =
                 if contains_substring filename "_errors" then
                   Spectec.Task.Negative
                 else Spectec.Task.Positive
               in
               { includes = [ includes_dir ]; filename; expect })

  let unparse = Frontend.unparse
  let parse_string = Frontend.parse_string

  let parse_input ~spec:_ { includes; filename; _ } =
    Frontend.parse_file includes filename
    |> Result.map (fun v -> Spectec.Task.Relation ("Program_ok", [ v ]))

  let source { filename; _ } = filename
  let expectation { expect; _ } = expect
  let format_output _values = "Typechecker succeeded"
  let save_output _filename _values = ()
end

module Instantiate = struct
  include Typecheck

  let name = "instantiator"

  let parse_input ~spec:_ { includes; filename; _ } =
    Frontend.parse_file includes filename
    |> Result.map (fun v -> Spectec.Task.Relation ("Program_inst", [ v ]))

  let format_output _values = "Instantiation succeeded"
end

(* Runs a P4 program on the packets of an STF file with the eBPF architecture. *)
module Sim = struct
  let name = "simulator"

  module Target = Target

  type input = { includes : string list; filename : string; stf : string }

  let uses_ebpf filename =
    contains_substring
      (In_channel.with_open_text filename In_channel.input_all)
      "ebpf_model.p4"

  let collect ?dir () =
    match dir with
    | None -> []
    | Some base ->
        let includes_dir = Filename.concat base "includes" in
        collect_files_recursive ~suffix:".p4" base
        |> List.filter_map (fun filename ->
               let stf = Filename.remove_extension filename ^ ".stf" in
               if Sys.file_exists stf && uses_ebpf filename then
                 Some { includes = [ includes_dir ]; filename; stf }
               else None)

  let unparse = Frontend.unparse
  let parse_string = Frontend.parse_string

  let parse_input ~spec:_ { includes; filename; stf; _ } =
    let ( let* ) = Result.bind in
    let* program = Frontend.parse_file includes filename in
    let* statements = Stf.parse_file stf in
    Ok (Spectec.Task.Driver (Ebpf_sim.drive program ~statements))

  let source { filename; _ } = filename
  let expectation _ = Spectec.Task.Positive

  let format_output = function
    | [] -> "No packets transmitted"
    | values ->
        values |> List.map Ebpf_sim.string_of_transmitted |> String.concat "\n"

  let save_output _filename _values = ()
end

let program_flags =
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map includes = flag "-i" (listed string) ~doc:"DIR P4 include paths"
  and filename = flag "-p" (required string) ~doc:"FILE P4 file to process" in
  (includes, filename)

let cli_flags =
  let open Core.Command.Let_syntax in
  let%map includes, filename = program_flags in
  { Typecheck.includes; filename; expect = Spectec.Task.Positive }

module Typecheck_cli : Cli.Task_cli.S = struct
  module Task = Typecheck

  let flags = cli_flags
end

module Instantiate_cli : Cli.Task_cli.S = struct
  module Task = Instantiate

  let flags = cli_flags
end

module Sim_cli : Cli.Task_cli.S = struct
  module Task = Sim

  let flags =
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map includes, filename = program_flags
    and stf =
      flag "-stf" (required string)
        ~doc:"FILE STF file with input packets and expected output packets"
    in
    { Sim.includes; filename; stf }
end

let target = (module Target : Spectec.Target.S)

module Cli : Cli.Target_cli.S = struct
  module Target = Target

  let command =
    let module Subcommand = Cli.Subcommand in
    Core.Command.group ~summary:"P4 commands"
      [
        Subcommand.make_task target ~name:"typecheck"
          ~summary:"Run P4 typechecker"
          (module Typecheck_cli);
        Subcommand.make_task target ~name:"instantiate"
          ~summary:"Instantiate a P4 program with eBPF extern objects"
          (module Instantiate_cli);
        Subcommand.make_task target ~name:"sim"
          ~summary:"Run a P4 program on STF packets with the eBPF architecture"
          (module Sim_cli);
        Subcommand.make_parse target ~name:"parse"
          ~summary:"parse a P4 program to an IL value"
          (module Typecheck_cli);
        Subcommand.make_batch target ~name:"batch" [ (module Typecheck_cli) ];
        Subcommand.make_checkpoint target ~name:"checkpoint";
      ]
end
