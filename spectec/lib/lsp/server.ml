open Linol_eio
module Paths = Map.Make (String)

let publish_diagnostics ~(notify_back : Jsonrpc2.notify_back) uri diagnostics =
  notify_back#send_notification
    (Linol.Lsp.Server_notification.PublishDiagnostics
       (PublishDiagnosticsParams.create ~uri ~diagnostics ()))

let server =
  object (self)
    inherit Jsonrpc2.server
    method spawn_query_handler f = spawn f
    val mutable workspace = Workspace.create ()
    val mutable uris = Paths.empty

    method private uri_for path =
      Option.value
        (Paths.find_opt (Sources.canonical path) uris)
        ~default:(DocumentUri.of_path path)

    val mutable published = Publication.empty

    method private publish ~notify_back ?closed analyses =
      let next, reports = Publication.update ?closed published analyses in
      published <- next;
      List.iter
        (fun (file, diagnostics) ->
          publish_diagnostics ~notify_back (self#uri_for file) diagnostics)
        reports

    method private recheck ~notify_back ?closed path =
      let next, analyses = Workspace.affected workspace path in
      workspace <- next;
      self#publish ~notify_back ?closed analyses

    method private update ~notify_back uri text =
      let path = DocumentUri.to_path uri in
      uris <- Paths.add (Sources.canonical path) uri uris;
      workspace <- Workspace.set workspace ~path text;
      self#recheck ~notify_back path

    method on_notif_doc_did_open ~notify_back (doc : TextDocumentItem.t)
        ~content =
      self#update ~notify_back doc.uri content

    method on_notif_doc_did_change ~notify_back
        (doc : VersionedTextDocumentIdentifier.t) _changes ~old_content:_
        ~new_content =
      self#update ~notify_back doc.uri new_content

    method! on_notif_doc_did_save ~notify_back
        (params : DidSaveTextDocumentParams.t) =
      let path = DocumentUri.to_path params.textDocument.uri in
      (match params.text with
      | Some text -> workspace <- Workspace.set workspace ~path text
      | None -> ());
      self#recheck ~notify_back path

    method on_notif_doc_did_close ~notify_back (doc : TextDocumentIdentifier.t)
        =
      let path = DocumentUri.to_path doc.uri in
      let next, closed = Workspace.close workspace path in
      workspace <- next;
      self#recheck ~notify_back ?closed path
  end

let serve () =
  Eio_main.run @@ fun env -> Jsonrpc2.run (Jsonrpc2.create_stdio ~env server)
