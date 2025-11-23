include Core.Types
include Core.Effects
module Print = Core.Print
module Eq = Core.Eq
module Utils = Core.Utils
module Print_debug = Core.Print_debug
module Free = Core.Free

module Value = struct
  include Core.Value

  let to_string t = Print.string_of_value ~short:false ~level:0 t
end

module Typ = struct
  include Core.Typ

  let to_string t = Print.string_of_typ t
  let eq = Eq.eq_typ
end
