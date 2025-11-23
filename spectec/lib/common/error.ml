open Source

let debug_errors = false

let warn (at : region) (category : string) (msg : string) =
  Printf.eprintf "%s\n%!"
    ((if at = no_region then "" else string_of_region at)
    ^ "Warning:" ^ category ^ ":" ^ msg)
