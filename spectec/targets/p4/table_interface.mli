(** Apply STF table updates to architecture state through the specification's
    [TableObject_add_entry] and [TableObject_add_default_action] relations. *)

(** Return the architecture state with the entry added. [table_name] accepts a
    full path, such as [main.filt.t], or a table's final name segment. If the
    full path names no object, [add_entry] looks up its final segment. When STF
    key names differ from the table's, keys receive the table's names in order.

    Return [TaskError] for an unknown table, a wrong key count or an entry
    rejected by the specification. SL and PL may report an interpreter error for
    a rejected entry. *)
val add_entry :
  eval_rel:
    (string -> Lang.Il.Value.t list -> Lang.Il.Value.t list Spectec.Task.result) ->
  eval_func:
    (string -> Lang.Il.Value.t list -> Lang.Il.Value.t Spectec.Task.result) ->
  ctx:Lang.Il.Value.t ->
  Lang.Il.Value.t ->
  table_name:string ->
  priority:Bigint.t option ->
  keys:(string * Stf.key_value) list ->
  action:Stf.action ->
  Lang.Il.Value.t Spectec.Task.result

(** Return the architecture state with the table's default action replaced.
    [table_name] uses the naming and lookup rules of {!add_entry}. The call
    fails if the table's current default action is [const]. *)
val add_default_action :
  eval_rel:
    (string -> Lang.Il.Value.t list -> Lang.Il.Value.t list Spectec.Task.result) ->
  eval_func:
    (string -> Lang.Il.Value.t list -> Lang.Il.Value.t Spectec.Task.result) ->
  ctx:Lang.Il.Value.t ->
  Lang.Il.Value.t ->
  table_name:string ->
  action:Stf.action ->
  Lang.Il.Value.t Spectec.Task.result
