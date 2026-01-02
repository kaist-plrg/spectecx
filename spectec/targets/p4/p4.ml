(** P4 Typechecker target - Implements TASK for P4 typechecking *)

(* Directories to skip during file collection *)
let skip_dirs = [ "include" ]

(* Recursively collect files with given suffix from directory *)
let collect_files ~suffix dir =
  let rec gather acc path =
    if Sys.is_directory path then (
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
  gather [] dir |> List.rev

(* P4 Typechecker task - implements TASK with extra make function *)
module Typecheck = struct
  let name = "typechecker"

  type input = {
    includes : string list;
    filename : string;
    expect : Runner.Task.expectation;
  }

  let make ?(expect = Runner.Task.Positive) ~includes ~filename () =
    { includes; filename; expect }

  let parse ~spec:_ { includes; filename; _ } =
    Runner.parse_p4_file includes filename
    |> Result.map (fun v -> ("Program_ok", [ v ]))

  let source { filename; _ } = filename
  let expectation { expect; _ } = expect

  let collect dir =
    collect_files ~suffix:".p4" dir
    |> List.map (fun filename ->
           { includes = []; filename; expect = Runner.Task.Positive })

  let format_output _values = "Typecheck succeeded"
  let save_output _filename _values = ()
end

(* P4 target specification *)
module Target = struct
  let name = "p4"
  let spec_dir = "examples/p4-concrete"
  let tasks = [ Runner.Task.Pack (module Typecheck) ]
end
