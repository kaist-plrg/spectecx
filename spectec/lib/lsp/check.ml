open Common.Source
module Lsp = Linol_eio

let severity : Spectec.Diagnostic.severity -> Lsp.DiagnosticSeverity.t =
  function
  | Spectec.Diagnostic.Error -> Lsp.DiagnosticSeverity.Error
  | Spectec.Diagnostic.Warning -> Lsp.DiagnosticSeverity.Warning
  | Spectec.Diagnostic.Info -> Lsp.DiagnosticSeverity.Information
  | Spectec.Diagnostic.Hint -> Lsp.DiagnosticSeverity.Hint

let of_diagnostic ~encoding ~sources (d : Spectec.Diagnostic.t) :
    Lsp.Diagnostic.t =
  let open Spectec.Diagnostic in
  let text =
    Option.value (Sources.text sources d.region.left.file) ~default:""
  in
  Lsp.Diagnostic.create
    ~range:(Convert.range ~encoding ~text d.region)
    ~severity:(severity d.severity) ~source:d.source
    ?code:(Option.map (fun c -> `String c) d.code)
    ~message:(`String d.message) ()

let unreadable file message : Spectec.Diagnostic.t =
  Spectec.Diagnostic.error ~source:"io" (region_of_file file)
    (Printf.sprintf "cannot read spec file %s: %s" (Filename.basename file)
       message)

let internal_error ~open_path exn : Spectec.Diagnostic.t =
  Spectec.Diagnostic.error ~source:"internal" (region_of_file open_path)
    ("internal error: " ^ Printexc.to_string exn)

let analyze (sources : Sources.t) =
  try
    match sources.errors with
    | _ :: _ -> List.map (fun (file, msg) -> unreadable file msg) sources.errors
    | [] -> (
        let parsed, parse_diagnostics =
          Spectec.with_diagnostics (fun () ->
              Spectec.parse_spec_sources sources.contents)
        in
        match parsed with
        | Error _ -> Spectec.Diagnostic.Bag.to_sorted_list parse_diagnostics
        | Ok spec ->
            let _, type_diagnostics =
              Spectec.with_diagnostics (fun () -> Spectec.elaborate spec)
            in
            Spectec.Diagnostic.Bag.merge parse_diagnostics type_diagnostics
            |> Spectec.Diagnostic.Bag.to_sorted_list)
  with exn ->
    let open_path =
      match sources.files with
      | path :: _ -> path
      | [] -> sources.scope.directory
    in
    [ internal_error ~open_path exn ]

let diagnostics ~encoding ~sources diagnostics =
  let files =
    List.map (fun (d : Spectec.Diagnostic.t) -> d.region.left.file) diagnostics
    |> List.sort_uniq Spectec.compare_spec_files
  in
  List.map
    (fun file ->
      ( file,
        List.filter_map
          (fun (d : Spectec.Diagnostic.t) ->
            if String.equal d.region.left.file file then
              Some (of_diagnostic ~encoding ~sources d)
            else None)
          diagnostics ))
    files
