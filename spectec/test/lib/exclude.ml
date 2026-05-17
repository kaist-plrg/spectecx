(** Exclude list for skipping test files *)

open Core
module Excludes = Set.Make (String)

type t = Excludes.t

let normalize line =
  let local_prefix = "spectec/testdata/interp/p4/p4c/" in
  let upstream_prefix = "p4c/testdata/" in
  let already_normalized = "../../../../../" ^ local_prefix in
  let chop_prefix prefix s = String.drop_prefix s (String.length prefix) in
  if String.is_prefix line ~prefix:already_normalized then line
  else
    let relative =
      if String.is_prefix line ~prefix:local_prefix then
        chop_prefix local_prefix line
      else if String.is_prefix line ~prefix:upstream_prefix then
        chop_prefix upstream_prefix line
      else line
    in
    already_normalized ^ relative

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
