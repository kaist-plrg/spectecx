module type Language = sig
  val name : string
  val title : string
  val parser : (Lexing.lexbuf -> Parse.token) -> Lexing.lexbuf -> Lang.Il.value
  val relation : string
end

module Make (Language : Language) = struct
  module Target : Spectec.Target.S = struct
    let name = Language.name

    let spec_dir =
      let root =
        match Itpl_sites.Sites.specs with
        | dir :: _ -> dir
        | [] -> "spectec/specs/itpl"
      in
      Filename.concat root name

    let builtins = []
    let opaque_types = []
    let is_impure_func _ = false
    let is_impure_rel _ = false
    let state_version = ref 0

    let with_state f =
      let next_vid = ref 0 in
      let fresh_vid () =
        let vid = !next_vid in
        incr next_vid;
        vid
      in
      Lang.Il.Value.GlobalVidProvider.with_provider fresh_vid f
  end

  module Eval = struct
    module Target = Target

    type input = { filename : string; expect : Spectec.Task.expectation }

    let name = "evaluator"
    let parse_string = Lexer.parse_string Language.parser
    let unparse = Unparse.unparse
    let source { filename; _ } = filename
    let expectation { expect; _ } = expect
    let save_output _ _ = ()

    let collect ?dir () =
      let rec gather path =
        if Sys.is_directory path then
          Sys.readdir path |> Array.to_list |> List.sort String.compare
          |> List.concat_map (fun entry -> gather (Filename.concat path entry))
        else if Filename.check_suffix path ("." ^ Language.name) then
          let expect =
            if
              Core.String.is_substring (Filename.basename path)
                ~substring:"_error"
            then Spectec.Task.Negative
            else Spectec.Task.Positive
          in
          [ { filename = path; expect } ]
        else []
      in
      match dir with
      | Some path when Sys.file_exists path -> gather path
      | _ -> []

    let parse_input ~spec:_ { filename; _ } =
      Lexer.parse_file Language.parser filename
      |> Result.map (fun input -> (Language.relation, [ input ]))

    let format_output values =
      let module Concrete = Interface.Concrete in
      values
      |> List.map
           (Format.asprintf "%a"
              (Concrete.Pp.pp_value Concrete.Hint.SMap.empty))
      |> String.concat ", "
  end

  let cli_flags =
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map filename =
      flag "-p" (required string) ~doc:("FILE " ^ Language.title ^ " input")
    in
    { Eval.filename; expect = Spectec.Task.Positive }

  module Eval_cli : Cli.Task_cli.S = struct
    module Task = Eval

    let flags = cli_flags
  end

  module Cli : Cli.Target_cli.S = struct
    module Target = Target

    let command =
      let target = (module Target : Spectec.Target.S) in
      Core.Command.group
        ~summary:(Language.title ^ " commands")
        [
          Cli.Subcommand.make_task target ~name:"eval"
            ~summary:("Run " ^ Language.title ^ " evaluator")
            (module Eval_cli);
          Cli.Subcommand.make_parse target ~name:"parse"
            ~summary:("Parse " ^ Language.title ^ " input")
            (module Eval_cli);
          Cli.Subcommand.make_batch target ~name:"batch" [ (module Eval_cli) ];
        ]
  end
end

module Make_typed (Language : Language) = struct
  module Base = Make (Language)
  module Target = Base.Target
  module Eval = Base.Eval
  module Eval_cli = Base.Eval_cli

  module Typecheck = struct
    include Eval

    let name = "typechecker"

    let collect ?dir () =
      Eval.collect ?dir ()
      |> List.map (fun (input : input) ->
             if
               Core.String.is_substring
                 (Filename.basename input.filename)
                 ~substring:"_runtime_error"
             then { input with expect = Spectec.Task.Positive }
             else input)

    let parse_input ~spec input =
      Eval.parse_input ~spec input
      |> Result.map (fun (_, values) -> ("Check_prog", values))

    let format_output values =
      values
      |> List.map (Format.asprintf "%a" Unparse.print_type)
      |> String.concat ", "
  end

  module Typecheck_cli : Cli.Task_cli.S = struct
    module Task = Typecheck

    let flags = Base.cli_flags
  end

  module Cli : Cli.Target_cli.S = struct
    module Target = Target

    let command =
      let target = (module Target : Spectec.Target.S) in
      Core.Command.group
        ~summary:(Language.title ^ " commands")
        [
          Cli.Subcommand.make_task target ~name:"typecheck"
            ~summary:("Run " ^ Language.title ^ " typechecker")
            (module Typecheck_cli);
          Cli.Subcommand.make_task target ~name:"eval"
            ~summary:("Typecheck and evaluate " ^ Language.title ^ " input")
            (module Eval_cli);
          Cli.Subcommand.make_parse target ~name:"parse"
            ~summary:("Parse " ^ Language.title ^ " input")
            (module Eval_cli);
          Cli.Subcommand.make_batch target ~name:"batch"
            [ (module Typecheck_cli); (module Eval_cli) ];
        ]
  end
end
