module Lsp = Linol_eio
module Sources = Spectec_lsp.Sources
module Check = Spectec_lsp.Check

let print_diagnostics = function
  | [] -> print_endline "(no diagnostics)"
  | reports ->
      List.iter
        (fun (file, diagnostics) ->
          Printf.printf "file: %s\n" (Filename.basename file);
          List.iter
            (fun d ->
              print_endline
                (Yojson.Safe.to_string (Lsp.Diagnostic.yojson_of_t d)))
            diagnostics)
        reports

let case ?path ?(buffers = []) name text =
  let path = Option.value path ~default:name in
  let sources =
    Sources.load ~path
      ~buffers:(Spectec.{ filename = path; contents = text } :: buffers)
  in
  Printf.printf "## %s\n" name;
  Check.analyze sources
  |> Check.diagnostics ~encoding:`UTF16 ~sources
  |> print_diagnostics;
  print_newline ()

let project_cases () =
  let path = "multifile/2-use.spectec" in
  let text = In_channel.with_open_bin path In_channel.input_all in
  case ~path "cross-file type resolves" text;
  List.iter
    (fun (name, contents) ->
      case ~path
        ~buffers:[ Spectec.{ filename = "multifile/1-def.spectec"; contents } ]
        name text)
    [
      ("cross-file parse error", "syntax foo =\n");
      ("cross-file elaboration error", "syntax foo = missing\n");
    ]

let editing_cases () =
  let path = "../../specs/impty/base/spec.spectec" in
  let text = In_channel.with_open_bin path In_channel.input_all in
  let assignment = "  env |- (x '=' e) -| (x -> v)::env" in
  let lines = String.split_on_char '\n' text in
  assert (List.length (List.filter (String.equal assignment) lines) = 1);
  let invalid =
    lines
    |> List.map (fun line ->
           if String.equal line assignment then
             "  env |- (x '=' e) -| (x -> e)::env"
           else line)
    |> String.concat "\n"
  in
  case ~path "typed imp: valid" text;
  case ~path "typed imp: expression in a value slot" invalid;
  case ~path "typed imp: recovered" text

let () =
  case "valid" "syntax type =\n  | INT\n  | type -> type\n";
  case "parse error" "syntax type =\n  | INT\n  | type ->\n";
  case "malformed atom" "syntax t = | X.foo\n";
  case "elab error" "syntax t = foo\n";
  project_cases ();
  editing_cases ()
