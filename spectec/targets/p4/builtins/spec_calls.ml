open Lang.Il
open Common.Annotated
module Location = Common.Location
module Callback = Builtins.Callback
open Builtins

let call ~(callback : Callback.t) (name : string) (values : Value.t list) :
    Value.t =
  callback (name $ Location.Unknown) [] values

let find_var_local_exn ~callback ctx ~name =
  call ~callback "find_var_e" [ P4_values.bare_name name; P4_values.local; ctx ]

let update_var_local ~callback ctx ~name ~value =
  call ~callback "update_var_e"
    [ P4_values.local; ctx; P4_values.bare_name name; value ]

let find_type_local_opt ~callback ~at ctx ~name =
  call ~callback "find_type_e" [ P4_values.local; ctx; Value.text name ]
  |> Arg.option_of Arg.value at

let subst_type_local ~callback ~ctx typ =
  call ~callback "subst_type_e" [ P4_values.local; ctx; typ ]

let max_size_in_bits ~callback typ =
  call ~callback "sizeof_maxSizeInBits'" [ typ ]

let write_value_from_bits ~callback value ~bits =
  call ~callback "write_value_from_bits"
    [ value; Value.nat Bigint.zero; P4_values.value_of_bits bits ]

let find_object_state_opt ~callback ~at arch ~object_id =
  call ~callback "find_objectState_e" [ arch; object_id ]
  |> Arg.option_of Arg.value at

let update_object_state_opt ~callback ~at arch ~object_id ~state =
  call ~callback "update_objectState_e" [ arch; object_id; state ]
  |> Arg.option_of Arg.value at
