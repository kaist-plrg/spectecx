include Itpl.Make_typed (struct
  let name = "tvfae"
  let title = "TVFAE"
  let parser = Parse.tvfae
  let relation = "Run_prog"
end)
