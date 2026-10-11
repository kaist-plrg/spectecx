include Itpl.Make (struct
  let name = "kfae"
  let title = "KFAE"
  let parser = Parse.kfae
  let relation = "Eval_prog"
end)
