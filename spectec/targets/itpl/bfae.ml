include Itpl.Make (struct
  let name = "bfae"
  let title = "BFAE"
  let parser = Parse.bfae
  let relation = "Eval_prog"
end)
