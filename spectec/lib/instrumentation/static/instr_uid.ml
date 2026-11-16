(* Assigns the instruction numbers shown in the full AL coverage report. *)

open Common.Annotated
module Location = Common.Location
module Al = Lang.Al
open Static

(* Normalize whitespace to single spaces *)
let normalize_whitespace s =
  let buf = Buffer.create (String.length s) in
  let last_ws = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\n' || c = '\t' || c = '\r' then (
        if not !last_ws then Buffer.add_char buf ' ';
        last_ws := true)
      else (
        Buffer.add_char buf c;
        last_ws := false))
    s;
  Buffer.contents buf

(* Truncate string to max length with "..." *)
let truncate max_len s =
  if String.length s > max_len then String.sub s 0 (max_len - 3) ^ "..." else s

let instr_key instr =
  let content = Al.Print.string_of_instr instr |> normalize_whitespace in
  (instr.at, truncate 30 content)

module State = struct
  let instr_to_uid : (Location.t * string, int) Hashtbl.t = Hashtbl.create 256
  let uid_to_instr : (int, Location.t * string) Hashtbl.t = Hashtbl.create 256
  let next_uid = ref 0

  let reset () =
    Hashtbl.clear instr_to_uid;
    Hashtbl.clear uid_to_instr;
    next_uid := 0
end

let assign_uid key =
  match Hashtbl.find_opt State.instr_to_uid key with
  | Some uid -> uid
  | None ->
      let uid = !State.next_uid in
      State.next_uid := !State.next_uid + 1;
      Hashtbl.replace State.instr_to_uid key uid;
      Hashtbl.replace State.uid_to_instr uid key;
      uid

let get_uid key = Hashtbl.find_opt State.instr_to_uid key

let assign_instr_uid instr =
  let key = instr_key instr in
  let _ = assign_uid key in
  ()

let init spec =
  State.reset ();
  match spec with
  | AlSpec al_spec ->
      List.iter
        (fun def ->
          match def.it with
          | Al.RelD { rules; _ } ->
              List.iter
                (fun rule ->
                  let ({ Al.instrs; _ } : Al.rule') = rule.it in
                  List.iter (fun instr -> assign_instr_uid instr) instrs)
                rules
          | Al.DecD { clauses; _ } ->
              List.iter
                (fun clause ->
                  let ({ Al.instrs; _ } : Al.clause') = clause.it in
                  List.iter (fun instr -> assign_instr_uid instr) instrs)
                clauses
          | _ -> ())
        al_spec
  | SlSpec _ -> ()

(* Reset state *)
let reset () = State.reset ()

(* Export UID mapping for checkpointing *)
let export () =
  Some
    ( State.instr_to_uid |> Hashtbl.to_seq |> List.of_seq,
      State.uid_to_instr |> Hashtbl.to_seq |> List.of_seq )

(* Restore UID mapping from checkpoint *)
let restore (instr_to_uid_list, uid_to_instr_list) =
  State.reset ();
  List.iter
    (fun (key, uid) ->
      Hashtbl.replace State.instr_to_uid key uid;
      Hashtbl.replace State.uid_to_instr uid key)
    instr_to_uid_list;
  (* Update next_uid to be higher than any existing UID *)
  State.next_uid :=
    List.fold_left (fun max_uid (uid, _) -> max max_uid uid) 0 uid_to_instr_list
    + 1

(* Implement Static.S signature *)
module Instr_uid : Static.S = struct
  type export_data =
    ((Location.t * string) * int) list * (int * (Location.t * string)) list

  let name = "instr_uid"
  let init = init
  let reset = reset
  let export () = export ()
  let restore data = restore data
end
