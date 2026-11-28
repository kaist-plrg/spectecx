module Util = Yojson.Safe.Util

exception Bad_request of string

let bad_request fmt =
  Format.kasprintf (fun message -> raise (Bad_request message)) fmt

let member name (request : Transport.request) =
  Util.member name request.arguments

let int_or name ~default request =
  match member name request with
  | `Null -> default
  | `Int n -> n
  | _ -> bad_request "`%s` is not a number." name

let bool_or name ~default request =
  match member name request with
  | `Null -> default
  | `Bool b -> b
  | _ -> bad_request "`%s` is not a boolean." name
