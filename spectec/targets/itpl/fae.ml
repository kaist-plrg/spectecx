include Itpl.Make (struct
  let name = "fae"
  let title = "FAE"
  let parser = Parse.fae
  let relation = "Eval_prog"
end)
