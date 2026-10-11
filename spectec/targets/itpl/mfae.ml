include Itpl.Make (struct
  let name = "mfae"
  let title = "MFAE"
  let parser = Parse.mfae
  let relation = "Eval_prog"
end)
