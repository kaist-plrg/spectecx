include Itpl.Make_typed (struct
  let name = "tifae"
  let title = "TIFAE"
  let parser = Parse.fae
  let relation = "Run_prog"
end)
