(** The Rust target: the static semantics of `rust-type-semantics` at v0.2.

    - the surface grammar of Figs. 2.1-2.3 ({!Lexer}, {!Parse}, {!Resolve});
    - the IL value of `pgm` ({!Value}), shaped by [GRAMMAR.md];
    - the round-trip printer ({!Unparse});
    - two tasks: [typecheck] runs the relation [Pgm_ok], [parse] converts and,
      with [-r], re-parses its own output and compares. *)

module Builtins = Builtins_rust

let collect_files_recursive ~suffix dir =
  let rec gather acc path =
    if Sys.file_exists path && Sys.is_directory path then (
      let entries = Sys.readdir path in
      Array.sort String.compare entries;
      Array.fold_left
        (fun acc name -> gather acc (Filename.concat path name))
        acc entries)
    else if Filename.check_suffix path suffix then path :: acc
    else acc
  in
  if Sys.file_exists dir then gather [] dir |> List.rev else []

let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* ------------------------------------------------------------------ *)
(* Interpreter state                                                   *)
(* ------------------------------------------------------------------ *)

(* One counter serves `$fresh_rgid` and `$fresh_tyid`, so that the ids of a run
   are distinct and reproducible. *)
let fresh_counter = ref 0

let with_state f =
  let vid_counter = ref 0 in
  let previous = !fresh_counter in
  fresh_counter := 0;
  let fresh_vid () =
    let vid = !vid_counter in
    incr vid_counter;
    vid
  in
  let fresh_id prefix =
    let n = !fresh_counter in
    incr fresh_counter;
    Printf.sprintf "?%s%d" prefix n
  in
  Fun.protect
    (fun () ->
      Lang.Il.Value.GlobalVidProvider.with_provider fresh_vid @@ fun () ->
      Builtins.Fresh.GlobalFreshProvider.with_provider fresh_id f)
    ~finally:(fun () -> fresh_counter := previous)

module Target : Spectec.Target.S = struct
  let name = "rust"

  let spec_dir =
    let exists d = Sys.file_exists d && Sys.is_directory d in
    let candidates = Rust_sites.Sites.specs @ [ "spectec/specs/rust" ] in
    match List.find_opt exists candidates with
    | Some dir -> dir
    | None -> (
        match Rust_sites.Sites.specs with
        | dir :: _ -> dir
        | [] -> "spectec/specs/rust")

  let builtins = Builtins.builtins
  let with_state = with_state

  (* No relation of the encoding is written yet (Tasks 38-44); both guards are
     revisited when the chapters that call $fresh_rgid/$fresh_tyid land. *)
  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = fresh_counter
end

(* ------------------------------------------------------------------ *)
(* Inputs                                                              *)
(* ------------------------------------------------------------------ *)

type input = {
  filename : string;
  expect : Spectec.Task.expectation;
  edition : Ast.edition;
}

(* A test's expectation is the DOCUMENT's verdict, so it can never be read off
   the file name.  `run_encoding.sync_testdata` writes it beside the copied
   programs as `expect.yaml`, one `<file>: positive|negative` line per program
   (Phase 4 design §6). *)
let load_expectations dir : (string * Spectec.Task.expectation) list =
  let path = Filename.concat dir "expect.yaml" in
  if not (Sys.file_exists path) then []
  else
    read_file path |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           let line =
             match String.index_opt line '#' with
             | Some i -> String.sub line 0 i
             | None -> line
           in
           match String.index_opt line ':' with
           | None -> None
           | Some i ->
               let key = String.trim (String.sub line 0 i) in
               let value =
                 String.trim
                   (String.sub line (i + 1) (String.length line - i - 1))
               in
               if key = "" || value = "" then None
               else
                 Some
                   ( key,
                     match value with
                     | "negative" -> Spectec.Task.Negative
                     | _ -> Spectec.Task.Positive ))

let collect ?dir () =
  match dir with
  | None -> []
  | Some test_dir ->
      let expects = load_expectations test_dir in
      collect_files_recursive ~suffix:".rs" test_dir
      |> List.map (fun filename ->
             let expect =
               match List.assoc_opt (Filename.basename filename) expects with
               | Some e -> e
               | None -> Spectec.Task.Positive
             in
             { filename; expect; edition = Ast.E2021 })

(* ------------------------------------------------------------------ *)
(* Parsing facade                                                      *)
(* ------------------------------------------------------------------ *)

let to_result (f : unit -> Lang.Il.Value.t) : Lang.Il.Value.t Spectec.Task.result
    =
  try Ok (f ()) with
  | Error.RustParseError (at, msg) ->
      Stdlib.Error (Spectec.Error.TaskParseError (at, msg))
  | Value.Bad msg ->
      Stdlib.Error (Spectec.Error.TaskParseError (Common.Source.no_region, msg))

let parse_file ~edition filename =
  to_result (fun () ->
      Value.of_pgm (Parse.parse_ast ~filename ~edition (read_file filename)))

(* `parse -r` hands the printer's output back here.  Unparse always emits the
   crate directive, so the crate label round-trips with the items and the
   default below is only used on a file that has none. *)
let parse_string ~spec:_ ~filename content =
  to_result (fun () ->
      Value.of_pgm
        (Parse.parse_ast ~filename ~edition:Ast.E2021 content))
  |> Result.map (fun v -> [ v ])

(* ------------------------------------------------------------------ *)
(* Tasks                                                               *)
(* ------------------------------------------------------------------ *)

module Task_common = struct
  module Target = Target

  type nonrec input = input

  let unparse = Unparse.unparse
  let parse_string = parse_string
  let source ({ filename; _ } : input) = filename
  let expectation ({ expect; _ } : input) = expect
  let save_output _ _ = ()
  let collect = collect
end

module Typecheck = struct
  include Task_common

  let name = "typechecker"

  let parse_input ~spec:_ { filename; edition; _ } =
    parse_file ~edition filename |> Result.map (fun v -> ("Pgm_ok", [ v ]))

  let format_output _ = "Typecheck succeeded"
end

let cli_flags =
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filename = flag "-p" (required string) ~doc:"FILE Rust file"
  and edition =
    flag "-e" (optional string)
      ~doc:"EDITION crate edition of the implicit crate: 2021 (default) or 2024"
  in
  let edition =
    match edition with
    | None | Some "2021" -> Ast.E2021
    | Some "2024" -> Ast.E2024
    | Some e -> failwith (Printf.sprintf "unknown edition %S" e)
  in
  { filename; expect = Spectec.Task.Positive; edition }

module Typecheck_cli : Cli.Task_cli.S = struct
  module Task = Typecheck

  let flags = cli_flags
end

module Cli : Cli.Target_cli.S = struct
  module Target = Target

  let command =
    let target = (module Target : Spectec.Target.S) in
    let module Subcommand = Cli.Subcommand in
    Core.Command.group ~summary:"Rust commands"
      [
        Subcommand.make_task target ~name:"typecheck"
          ~summary:"Run the Rust type system on a program"
          (module Typecheck_cli);
        Subcommand.make_parse target ~name:"parse"
          ~summary:"parse a Rust program to an IL value"
          (module Typecheck_cli);
        Subcommand.make_batch target ~name:"batch" [ (module Typecheck_cli) ];
        Subcommand.make_checkpoint target ~name:"checkpoint";
      ]
end
