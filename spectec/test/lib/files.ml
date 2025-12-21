(** File collection utilities *)

open Core

let skip_dirs = String.Set.of_list [ "include" ]

let rec gather acc ~suffix dir =
  let entries = Sys_unix.readdir dir in
  Array.sort entries ~compare:String.compare;
  Array.fold entries ~init:acc ~f:(fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys_unix.is_directory_exn path then
        if Set.mem skip_dirs entry then acc else gather acc ~suffix path
      else if String.is_suffix path ~suffix then path :: acc
      else acc)

let collect ~suffix dir = gather [] ~suffix dir |> List.rev
