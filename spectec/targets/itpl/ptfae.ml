include Itpl.Make_typed (struct
  let name = "ptfae"
  let title = "PTFAE"
  let parser = Parse.ptfae
  let relation = "Run_prog"
end)
