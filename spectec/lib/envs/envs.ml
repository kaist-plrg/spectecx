module Make = Envs_make
module Il = Envs_il

module RTEnv = Make.MakeIdMap (struct
  type t = Lang.Il.reltyp

  let to_string = Lang.Il.Print.string_of_reltyp
end)
