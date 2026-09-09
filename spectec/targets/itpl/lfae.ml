include Itpl.Make (struct
  let name = "lfae"
  let title = "LFAE"
  let parser = Parse.fae
  let relation = "Eval_prog"
end)
