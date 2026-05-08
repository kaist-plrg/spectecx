open Common.Source
module P = Parse

(* --- utilities -------------------------------------------------------- *)

let read_lines (ic : in_channel) : string list =
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> List.rev acc
  in
  aux []

(* Extract top-level "(…)" groups, handling nested parens. *)
let extract_paren_groups (s : string) : string list =
  let n = String.length s in
  let groups = ref [] in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '(' then begin
      let start = !i in
      let depth = ref 1 in
      incr i;
      while !i < n && !depth > 0 do
        (if s.[!i] = '(' then incr depth
         else if s.[!i] = ')' then decr depth);
        incr i
      done;
      groups := String.sub s start (!i - start) :: !groups
    end else incr i
  done;
  List.rev !groups

(* Parse "(id : typ_str)" into an ast_param. *)
let parse_param_group (grp : string) : (Qc_ast.ast_param, string) result =
  let inner =
    let len = String.length grp in
    if len >= 2 && grp.[0] = '(' && grp.[len - 1] = ')'
    then String.sub grp 1 (len - 2) |> String.trim
    else grp
  in
  match String.index_opt inner ':' with
  | None ->
    Error (Printf.sprintf "param declaration missing ':' in '%s'" grp)
  | Some colon ->
    let id_str  = String.sub inner 0 colon |> String.trim in
    let typ_str =
      String.sub inner (colon + 1) (String.length inner - colon - 1)
      |> String.trim
    in
    if id_str = "" then Error (Printf.sprintf "empty identifier in '%s'" grp)
    else
      match P.parse_plaintyp typ_str with
      | Error e -> Error (P.error_to_string e)
      | Ok plaintyp ->
        Ok { Qc_ast.p_id = id_str $ no_region; p_typ = plaintyp }

(* Parse all "(id : typ)" groups on a line. *)
let parse_params_line (line : string) :
    (Qc_ast.ast_param list, string) result =
  let groups = extract_paren_groups line in
  List.fold_right
    (fun r acc ->
      match r, acc with
      | Ok p, Ok ps -> Ok (p :: ps)
      | Error e, _ -> Error e
      | _, Error e -> Error e)
    (List.map parse_param_group groups)
    (Ok [])

(* Parse a "-- <prem_text>" line into an EL premise. *)
let parse_prem_line (line : string) : (Lang.El.prem, string) result =
  let text =
    let line = String.trim line in
    if String.length line >= 2 && String.sub line 0 2 = "--"
    then String.sub line 2 (String.length line - 2) |> String.trim
    else line
  in
  match P.parse_prem text with
  | Error e -> Error (P.error_to_string e)
  | Ok prem -> Ok prem

(* --- line classification ---------------------------------------------- *)

type line_kind =
  | L_Header of [ `Prop | `Gen ]
  | L_Param
  | L_Prem
  | L_Goal
  | L_Blank

let classify (line : string) : line_kind =
  match String.trim line with
  | "" -> L_Blank
  | "quickcheck/prop:" -> L_Header `Prop
  | "quickcheck/gen:"  -> L_Header `Gen
  | s when s.[0] = '(' -> L_Param
  | s when String.length s >= 2 && String.sub s 0 2 = "--" -> L_Prem
  | _ -> L_Goal

(* --- block parsers ---------------------------------------------------- *)

let parse_prop_block (block_lines : string list) :
    (Qc_ast.ast_block, string) result =
  let rec collect_params acc lines =
    match lines with
    | [] -> Error "quickcheck/prop: missing goal relation name"
    | line :: rest -> (
        match classify line with
        | L_Blank -> collect_params acc rest
        | L_Param -> (
            match parse_params_line line with
            | Error e -> Error e
            | Ok ps -> collect_params (acc @ ps) rest)
        | L_Prem ->
          Error "quickcheck/prop: goal relation name required before premises"
        | L_Goal -> (
            match P.parse_prem (String.trim line) with
            | Error e -> Error (P.error_to_string e)
            | Ok goal -> collect_prems acc goal [] rest)
        | L_Header _ -> Error "quickcheck/prop: unexpected nested header")
  and collect_prems params goal prems_acc lines =
    match lines with
    | [] ->
      Ok (Qc_ast.AB_Prop { params; goal; prems = List.rev prems_acc })
    | line :: rest -> (
        match classify line with
        | L_Blank -> collect_prems params goal prems_acc rest
        | L_Prem -> (
            match parse_prem_line line with
            | Error e -> Error e
            | Ok p -> collect_prems params goal (p :: prems_acc) rest)
        | L_Header _ -> Error "quickcheck/prop: unexpected nested header"
        | L_Param ->
          Error "quickcheck/prop: param declaration after goal not allowed"
        | L_Goal ->
          Error
            (Printf.sprintf "quickcheck/prop: unexpected line '%s'" line))
  in
  collect_params [] block_lines

let parse_gen_block (block_lines : string list) :
    (Qc_ast.ast_block, string) result =
  let rec collect_params acc lines =
    match lines with
    | [] ->
      Ok (Qc_ast.AB_Gen { params = acc; prems = [] })
    | line :: rest -> (
        match classify line with
        | L_Blank -> collect_params acc rest
        | L_Param -> (
            match parse_params_line line with
            | Error e -> Error e
            | Ok ps -> collect_params (acc @ ps) rest)
        | L_Prem | L_Goal -> collect_prems acc [] (line :: rest)
        | L_Header _ -> Error "quickcheck/gen: unexpected nested header")
  and collect_prems params prems_acc lines =
    match lines with
    | [] ->
      Ok (Qc_ast.AB_Gen { params; prems = List.rev prems_acc })
    | line :: rest -> (
        match classify line with
        | L_Blank -> collect_prems params prems_acc rest
        | L_Prem | L_Goal -> (
            match parse_prem_line line with
            | Error e -> Error e
            | Ok p -> collect_prems params (p :: prems_acc) rest)
        | L_Header _ -> Error "quickcheck/gen: unexpected nested header"
        | L_Param ->
          Error "quickcheck/gen: param declaration after premises not allowed")
  in
  collect_params [] block_lines

(* --- top-level -------------------------------------------------------- *)

(* Group lines into (header_kind, body_lines) pairs. *)
let split_into_blocks (lines : string list) :
    ([ `Prop | `Gen ] * string list) list =
  let rec aux kind acc result lines =
    match lines with
    | [] ->
      let result =
        match kind with
        | None -> result
        | Some k -> (k, List.rev acc) :: result
      in
      List.rev result
    | line :: rest -> (
        match classify line with
        | L_Header new_kind ->
          let result =
            match kind with
            | None -> result
            | Some k -> (k, List.rev acc) :: result
          in
          aux (Some new_kind) [] result rest
        | _ -> aux kind (line :: acc) result rest)
  in
  aux None [] [] lines

let parse_file (path : string) : (Qc_ast.ast_file, string) result =
  match open_in path with
  | exception Sys_error msg ->
    Error (Printf.sprintf "quickcheck: cannot open '%s': %s" path msg)
  | ic ->
    let result =
      Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
      let lines = read_lines ic in
      let blocks = split_into_blocks lines in
      if blocks = [] then
        Error (Printf.sprintf "quickcheck: no blocks found in '%s'" path)
      else
        List.fold_right
          (fun (kind, body) acc ->
            match acc with
            | Error _ -> acc
            | Ok bs ->
              let r =
                match kind with
                | `Prop -> parse_prop_block body
                | `Gen  -> parse_gen_block body
              in
              (match r with
               | Error e -> Error e
               | Ok b -> Ok (b :: bs)))
          blocks (Ok [])
    in
    result
