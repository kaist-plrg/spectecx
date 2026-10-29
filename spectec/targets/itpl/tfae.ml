include Itpl.Make_typed (struct
  let name = "tfae"
  let title = "TFAE"
  let parser = Parse.tfae
  let relation = "Run_prog"
end)
