module Workspace = Spectec_lsp.Workspace

let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  let definition = Sources.canonical_path "unmarked/2-def.spectec" in
  let use = Sources.canonical_path "unmarked/10-use.spectec" in
  let inserted = Sources.canonical_path "unmarked/5-added.spectec" in
  let sources =
    Sources.load use
      ~buffers:
        [
          Spectec.{ filename = inserted; contents = "syntax middle = base\n" };
          Spectec.{ filename = use; contents = "syntax use = middle\n" };
        ]
  in
  assert (sources.files = [ definition; inserted; use ]);
  assert (Spectec_lsp.Check.analyze sources = []);
  assert (
    Sources.find_text_opt sources.contents ~file:use
    = Some "syntax use = middle\n");
  assert (
    Sources.find_text_opt sources.contents ~file:"unmarked/10-use.spectec"
    = None);
  let alias =
    Filename.concat (Filename.dirname use) "../unmarked/10-use.spectec"
  in
  assert (Sources.find_text_opt sources.contents ~file:alias = None);
  assert (
    Sources.canonical_path "unmarked/../unmarked/2-def.spectec" = definition);
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
  assert (
    Sources.find_text_opt changed.sources.contents ~file:definition
    = Some "syntax base = text\n");
  let workspace, shared = Workspace.analyze workspace definition in
  assert (changed == shared);
  let _, saved = Workspace.analyze saved_workspace use in
  assert (saved == original);
  assert (
    Sources.find_text_opt saved.sources.contents ~file:definition
    = Some (read definition));
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
  assert (
    Sources.find_text_opt rechecked.sources.contents ~file:definition
    = Some (read definition));
  assert (rechecked.diagnostics = []);
  let _, retained = Workspace.analyze before_close use in
  assert (retained == recovered);
  let workspace, closed = Workspace.close workspace use in
  assert (closed = Some (Sources.spec_directory_of use));
  let workspace, affected = Workspace.affected workspace definition in
  assert (affected = []);
  let workspace, affected = Workspace.affected workspace use in
  assert (affected = []);
  let outer = Sources.canonical_path "overlapping/1-outer.spectec" in
  let inner = Sources.canonical_path "overlapping/inner/2-inner.spectec" in
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
  assert (closed = Some (Sources.spec_directory_of inner));
  let _, remaining = Workspace.affected workspace inner in
  assert (List.length remaining = 1);
  assert (
    Sources.find_text_opt (List.hd remaining).sources.contents ~file:inner
    = Some (read inner))
