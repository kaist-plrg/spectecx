include Itpl.Make_typed (struct
  let name = "stfae"
  let title = "STFAE"
  let parser = Parse.stfae
  let relation = "Run_prog"
end)
