(** P4 Typechecker target - Implements TARGET and TASK for P4 typechecking *)

(* Paths are relative to the repo root (where the binary runs from) *)
let includes_dir = "tests/interp/p4-tests/includes"
let excludes_dir = "tests/interp/p4-tests/excludes"
let test_base_dir = "tests/interp/p4-tests/tests"

(* Directories to skip during file collection *)
let skip_dirs = [ "include" ]

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
          else read_lines (line :: acc)
        with End_of_file ->
          close_in ic;
          acc
      in
      read_lines [])
    exclude_files

(* Check if a path should be excluded - matches against test-relative path *)
let is_excluded excludes path =
  List.exists (fun pattern -> contains_substring path pattern) excludes

(* P4 target specification *)
module Target : Runner.Target.S = struct
  let name = "p4"
  let spec_dir = "examples/p4-concrete"
  let test_dir = test_base_dir
end

(* P4 Typechecker task - extends TASK with make function for CLI *)
module Typecheck = struct
  let name = "typechecker"

  module Target = Target

  type input = {
    includes : string list;
    filename : string;
    expect : Runner.Task.expectation;
  }

  (* Create an input with optional expectation *)
  let make ?(expect = Runner.Task.Positive) ~includes ~filename () =
    { includes; filename; expect }

  (* Collect inputs from directory, uses Target.test_dir if not specified *)
  let collect ?dir () =
    let test_dir = Option.value dir ~default:Target.test_dir in
    let excludes = load_excludes excludes_dir in
    collect_files_recursive ~suffix:".p4" test_dir
    |> List.filter (fun filename -> not (is_excluded excludes filename))
    |> List.map (fun filename ->
           let expect =
             if contains_substring filename "_errors" then Runner.Task.Negative
             else Runner.Task.Positive
           in
           { includes = [ includes_dir ]; filename; expect })

  let parse ~spec:_ { includes; filename; _ } =
    Runner.parse_p4_file includes filename
    |> Result.map (fun v -> ("Program_ok", [ v ]))

  let source { filename; _ } = filename
  let expectation { expect; _ } = expect
  let format_output _values = "Typechecker succeeded"
  let save_output _filename _values = ()
end
