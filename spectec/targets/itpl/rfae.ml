include Itpl.Make (struct
  let name = "rfae"
  let title = "RFAE"
  let parser = Parse.rfae
  let relation = "Eval_prog"
end)
