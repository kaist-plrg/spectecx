module Sources = Spectec_lsp.Sources
module Workspace = Spectec_lsp.Workspace

let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  let definition = Sources.canonical "unmarked/2-def.spectec" in
  let use = Sources.canonical "unmarked/10-use.spectec" in
  let inserted = Sources.canonical "unmarked/5-added.spectec" in
  let sources =
    Sources.load ~path:use
      ~buffers:
        [
          Spectec.{ filename = inserted; contents = "syntax middle = base\n" };
          Spectec.{ filename = use; contents = "syntax use = middle\n" };
        ]
  in
  assert (sources.files = [ definition; inserted; use ]);
  assert (Spectec_lsp.Check.analyze sources = []);
  assert (Sources.text sources use = Some "syntax use = middle\n");
  assert (Sources.text sources "unmarked/10-use.spectec" = None);
  let alias =
    Filename.concat (Filename.dirname use) "../unmarked/10-use.spectec"
  in
  assert (Sources.text sources alias = None);
  assert (Sources.canonical "unmarked/../unmarked/2-def.spectec" = definition);
  let workspace = Workspace.create () in
  let workspace = Workspace.set workspace ~path:definition (read definition) in
  let workspace = Workspace.set workspace ~path:use (read use) in
  let workspace, original = Workspace.analyze workspace definition in
  assert (original.diagnostics = []);
  let workspace, shared = Workspace.analyze workspace use in
  assert (original == shared);
  let saved_workspace = workspace in
  let workspace =
    Workspace.set workspace ~path:definition "syntax base = text\n"
  in
  let workspace, changed = Workspace.analyze workspace use in
  assert (changed != original);
  assert (Sources.text changed.sources definition = Some "syntax base = text\n");
  let workspace, shared = Workspace.analyze workspace definition in
  assert (changed == shared);
  let _, saved = Workspace.analyze saved_workspace use in
  assert (saved == original);
  assert (Sources.text saved.sources definition = Some (read definition));
  let workspace = Workspace.set workspace ~path:definition "syntax base =\n" in
  let workspace, incomplete = Workspace.analyze workspace use in
  assert (incomplete.diagnostics <> []);
  let workspace =
    Workspace.set workspace ~path:definition "syntax base = missing\n"
  in
  let workspace, invalid = Workspace.analyze workspace definition in
  assert (invalid.diagnostics <> []);
  let workspace =
    Workspace.set workspace ~path:definition "syntax base = text\n"
  in
  let workspace, recovered = Workspace.analyze workspace use in
  assert (recovered.diagnostics = []);
  let before_close = workspace in
  let workspace, closed = Workspace.close workspace definition in
  assert (closed = None);
  let workspace, remaining = Workspace.affected workspace definition in
  assert (List.length remaining = 1);
  let rechecked = List.hd remaining in
  assert (Sources.text rechecked.sources definition = Some (read definition));
  assert (rechecked.diagnostics = []);
  let _, retained = Workspace.analyze before_close use in
  assert (retained == recovered);
  let workspace, closed = Workspace.close workspace use in
  assert (closed = Some (Sources.scope_of use));
  let workspace, affected = Workspace.affected workspace definition in
  assert (affected = []);
  let workspace, affected = Workspace.affected workspace use in
  assert (affected = []);
  let outer = Sources.canonical "overlapping/1-outer.spectec" in
  let inner = Sources.canonical "overlapping/inner/2-inner.spectec" in
  let workspace = Workspace.set workspace ~path:outer (read outer) in
  let workspace = Workspace.set workspace ~path:inner (read inner) in
  let workspace, initial = Workspace.affected workspace inner in
  assert (List.length initial = 2);
  let workspace, affected = Workspace.affected workspace outer in
  assert (List.length affected = 1);
  let workspace = Workspace.set workspace ~path:inner "syntax inner = nat\n" in
  let workspace, updated = Workspace.affected workspace inner in
  assert (
    List.for_all (fun (a : Workspace.analysis) -> a.diagnostics = []) updated);
  let workspace, closed = Workspace.close workspace inner in
  assert (closed = Some (Sources.scope_of inner));
  let _, remaining = Workspace.affected workspace inner in
  assert (List.length remaining = 1);
  assert (Sources.text (List.hd remaining).sources inner = Some (read inner))
