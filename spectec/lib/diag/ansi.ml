type style = Bold | Dim | Red | Yellow | Blue | Cyan | Green
type t = { enabled : bool }

let plain = { enabled = false }
let color = { enabled = true }

let code = function
  | Bold -> "\027[1m"
  | Dim -> "\027[2m"
  | Red -> "\027[31m"
  | Yellow -> "\027[33m"
  | Blue -> "\027[34m"
  | Cyan -> "\027[36m"
  | Green -> "\027[32m"

let reset = "\027[0m"

let style ansi styles s =
  if (not ansi.enabled) || styles = [] then s
  else String.concat "" (List.map code styles) ^ s ^ reset
