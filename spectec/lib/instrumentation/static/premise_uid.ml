(* Premise UID static analysis.
 *
 * Assigns stable unique identifiers to premises during static initialization.
 * The UID mapping is shared across all handlers that need to reference premises.
 *)

open Common.Source
module Il = Lang.Il
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

(* Create a unique key for a premise using region + content prefix *)
let prem_key prem =
  let content = Il.Print.string_of_prem prem |> normalize_whitespace in
  (prem.at, truncate 30 content)

(* Shared state for premise UID mapping *)
module State = struct
  let prem_to_uid : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let uid_to_prem : (int, region * string) Hashtbl.t = Hashtbl.create 256
  let next_uid = ref 0

  let reset () =
    Hashtbl.clear prem_to_uid;
    Hashtbl.clear uid_to_prem;
    next_uid := 0
end

(* Assign a stable UID to a premise key, or return existing UID *)
let assign_uid key =
  match Hashtbl.find_opt State.prem_to_uid key with
  | Some uid -> uid
  | None ->
      let uid = !State.next_uid in
      State.next_uid := !State.next_uid + 1;
      Hashtbl.replace State.prem_to_uid key uid;
      Hashtbl.replace State.uid_to_prem uid key;
      uid

(* Get UID for a premise key (returns None if not assigned) *)
let get_uid key = Hashtbl.find_opt State.prem_to_uid key

(* Get premise key for a UID (returns None if not found) *)
let get_premise uid = Hashtbl.find_opt State.uid_to_prem uid

(* Recursively assign UIDs to all premises in a premise *)
let assign_premise_uid prem =
  let key = prem_key prem in
  let _ = assign_uid key in
  ()

(* Initialize: walk spec and assign UIDs to all premises *)
let init spec =
  State.reset ();
  match spec with
  | IlSpec il_spec ->
      List.iter
        (fun def ->
          match def.it with
          | Il.RelD (_, _, _, rules) ->
              List.iter
                (fun rule ->
                  let _, _, prems = rule.it in
                  List.iter (fun prem -> assign_premise_uid prem) prems)
                rules
          | Il.DecD (_, _, _, _, clauses) ->
              List.iter
                (fun clause ->
                  let _, _, prems = clause.it in
                  List.iter (fun prem -> assign_premise_uid prem) prems)
                clauses
          | Il.TypD _ -> ())
        il_spec
  | SlSpec _ -> () (* SL specs don't have premises in the same way *)

(* Reset state *)
let reset () = State.reset ()

(* Export UID mapping for checkpointing *)
let export () =
  Some
    ( State.prem_to_uid |> Hashtbl.to_seq |> List.of_seq,
      State.uid_to_prem |> Hashtbl.to_seq |> List.of_seq )

(* Restore UID mapping from checkpoint *)
let restore (prem_to_uid_list, uid_to_prem_list) =
  State.reset ();
  List.iter
    (fun (key, uid) ->
      Hashtbl.replace State.prem_to_uid key uid;
      Hashtbl.replace State.uid_to_prem uid key)
    prem_to_uid_list;
  (* Update next_uid to be higher than any existing UID *)
  State.next_uid :=
    List.fold_left (fun max_uid (uid, _) -> max max_uid uid) 0 uid_to_prem_list
    + 1

(* Implement Static.S signature *)
module Premise_uid : Static.S = struct
  type export_data =
    ((region * string) * int) list * (int * (region * string)) list

  let name = "premise_uid"
  let init = init
  let reset = reset
  let export () = export ()
  let restore data = restore data
end
