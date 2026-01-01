(** P4 Typechecker target - Implements TASK for P4 typechecking *)

(* Recursively collect files with given suffix from directory *)
let collect_files ~suffix dir =
  let rec walk acc path =
    if Sys.is_directory path then
      Array.fold_left
        (fun acc name -> walk acc (Filename.concat path name))
        acc (Sys.readdir path)
    else if Filename.check_suffix path suffix then path :: acc
    else acc
  in
  walk [] dir |> List.sort String.compare

(* P4 Typechecker task - implements TASK with extra make function *)
module Typecheck = struct
  let name = "typecheck"

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
