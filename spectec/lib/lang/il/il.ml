include Types
module Effects = Effects
module Print = Print
module Eq = Eq
module Utils = Utils
module Print_debug = Print_debug
module Free = Free

module Value = struct
  include Value

  let to_string t = Print.string_of_value ~short:false ~level:0 t
end

module Typ = struct
  include Typ

  let to_string t = Print.string_of_typ t
  let eq = Eq.eq_typ
end
