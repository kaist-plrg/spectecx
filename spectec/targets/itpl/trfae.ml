include Itpl.Make_typed (struct
  let name = "trfae"
  let title = "TRFAE"
  let parser = Parse.trfae
  let relation = "Run_prog"
end)
