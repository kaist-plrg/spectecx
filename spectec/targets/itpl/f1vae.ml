include Itpl.Make (struct
  let name = "f1vae"
  let title = "F1VAE"
  let parser = Parse.f1vae
  let relation = "Eval_prog"
end)
