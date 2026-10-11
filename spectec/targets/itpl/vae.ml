include Itpl.Make (struct
  let name = "vae"
  let title = "VAE"
  let parser = Parse.vae
  let relation = "Eval_prog"
end)
