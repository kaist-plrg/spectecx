include Itpl.Make (struct
  let name = "fvae"
  let title = "FVAE"
  let parser = Parse.fvae
  let relation = "Eval_prog"
end)
