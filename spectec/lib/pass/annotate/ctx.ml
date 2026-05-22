(** Annotation context: tracks the enclosing relation or function (so a
    [ResultI] can find its rel's [prose_out]) and threads the user-authored hint
    store through the walk. *)

type namespace = Rel of string | Func of string | Empty
type t = { namespace : namespace; henv : Hints.Henv.t }

let init (henv : Hints.Henv.t) : t = { namespace = Empty; henv }

let enter_rel (ctx : t) (id_rel : string) : t =
  { ctx with namespace = Rel id_rel }

let enter_func (ctx : t) (id_func : string) : t =
  { ctx with namespace = Func id_func }

let current_rel (ctx : t) : string option =
  match ctx.namespace with Rel id -> Some id | _ -> None

let find_alter (ctx : t) ~(hid : string) ~(subject : Hints.Henv.subject) :
    Hints.Alter.t option =
  Hints.Henv.find_alter ctx.henv ~hid ~subject

let find_prose_rel (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose" ~subject:(Hints.Henv.Rel id)

let find_prose_in_rel (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose_in" ~subject:(Hints.Henv.Rel id)

let find_prose_out_rel (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose_out" ~subject:(Hints.Henv.Rel id)

let find_prose_true_rel (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose_true" ~subject:(Hints.Henv.Rel id)

let find_prose_false_rel (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose_false" ~subject:(Hints.Henv.Rel id)

let find_prose_func (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose" ~subject:(Hints.Henv.Func id)

let find_prose_in_func (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose_in" ~subject:(Hints.Henv.Func id)

let find_prose_true_func (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose_true" ~subject:(Hints.Henv.Func id)

let find_prose_false_func (ctx : t) (id : string) : Hints.Alter.t option =
  find_alter ctx ~hid:"prose_false" ~subject:(Hints.Henv.Func id)

let find_rel_inputs (ctx : t) (id : string) : Hints.Input.t option =
  Hints.Henv.find_rel_inputs ctx.henv ~rel:id
