module Location = Common.Location

type target = Rel of string | Rule of string * string | Prem of Sites.site
type breakpoint = { number : int; target : target }

let string_of_line at =
  match at with
  | Location.Span { file; left; _ } -> Format.sprintf "%s:%d" file left.line
  | Location.Unknown | Location.File _ -> Location.to_string at

let string_of_target = function
  | Rel id -> id
  | Rule (id, rule_id) -> id ^ "/" ^ rule_id
  | Prem { owner; span; _ } -> string_of_line span ^ " in " ^ owner

type t = { mutable breakpoints : breakpoint list; mutable next_number : int }

let create () = { breakpoints = []; next_number = 1 }
let to_list table = table.breakpoints

let find table target =
  List.find_opt (fun b -> b.target = target) table.breakpoints

let on_premise table at =
  List.find_opt
    (fun b -> match b.target with Prem site -> site.hit = at | _ -> false)
    table.breakpoints

let add table target =
  match find table target with
  | Some breakpoint -> `Existing breakpoint
  | None ->
      let breakpoint = { number = table.next_number; target } in
      table.next_number <- table.next_number + 1;
      table.breakpoints <- table.breakpoints @ [ breakpoint ];
      `New breakpoint

let delete table number =
  let exists = List.exists (fun b -> b.number = number) table.breakpoints in
  table.breakpoints <-
    List.filter (fun b -> b.number <> number) table.breakpoints;
  exists
