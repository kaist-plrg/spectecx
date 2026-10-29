type callee = Func of string | Rel of string

type t =
  | Created of { value : Lang.Il.Value.t; origin : Origin.t }
  | Derived of { value : Lang.Il.Value.t; source : Lang.Il.Value.t }
  | Combined of { value : Lang.Il.Value.t; sources : Lang.Il.Value.t list }
  | Updated of {
      value : Lang.Il.Value.t;
      base : Lang.Il.Value.t;
      element : Lang.Il.Value.t;
    }
  | Invoked of {
      callee : callee;
      inputs : Lang.Il.Value.t list;
      outputs : Lang.Il.Value.t list;
      cached : bool;
    }
