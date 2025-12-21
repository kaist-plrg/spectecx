(** Exclude list for skipping test files *)

open Core
module Excludes = Set.Make (String)

type t = Excludes.t

let normalize line = "../../../../../tests/interp/p4-tests/tests/" ^ line

let from_file filename =
  In_channel.read_lines filename
  |> List.filter_map ~f:(fun line ->
         let trimmed = String.strip line in
         if String.is_empty trimmed then None
         else if Char.equal trimmed.[0] '#' then None
         else Some (normalize trimmed))

let load paths =
  let files = List.concat_map paths ~f:(Files.collect ~suffix:".exclude") in
  files |> List.concat_map ~f:from_file |> Excludes.of_list

let mem set filename = Set.mem set filename
let empty = Excludes.empty
