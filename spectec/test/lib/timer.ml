(** Simple timing utilities *)

let now () = Core_unix.gettimeofday ()

let time f =
  let start = now () in
  let result = f () in
  let duration = now () -. start in
  (duration, result)
