module Mixfix = Il.Mixfix
module Terminal = Grammar.Terminal
open Common.Source

type 'tok lexicon = {
  classify : 'tok -> Terminal.t option;
  lexical_tag : 'tok -> string option;
  lexical_tags : string list;
  separator : string -> Xl.Atom.t option;
  spell : Xl.Atom.t -> Xl.Atom.t list;
}

let plain_lexicon classify =
  {
    classify;
    lexical_tag = (fun _ -> None);
    lexical_tags = [];
    separator = (fun _ -> None);
    spell = (fun atom -> [ atom ]);
  }

type ambiguity = {
  syntax : string;
  first : int;
  last : int;
  readings : string list;
}

type outcome =
  | Accepted of { value : Il.value; trees : int; ambiguities : ambiguity list }
  | Rejected of { at : int; expected : string list }

let rec string_of_arg = function
  | Grammar.Nonterminal id -> id.it
  | Grammar.Primitive (Grammar.Num numtyp) -> Xl.Num.string_of_typ numtyp
  | Grammar.Primitive Grammar.Bool -> "bool"
  | Grammar.Primitive Grammar.Text -> "text"
  | Grammar.Iter (arg, Il.Opt) -> string_of_arg arg ^ "?"
  | Grammar.Iter (arg, Il.List) -> string_of_arg arg ^ "*"

let string_of_production name (prod : Grammar.production) =
  let mixeme = function
    | Mixfix.Atom atom -> Xl.Atom.to_string atom.it
    | Mixfix.Arg arg -> string_of_arg arg
  in
  name ^ " ::= " ^ String.concat " " (List.map mixeme prod.notation)

(* The grammar as plain BNF

   Each production becomes one rule. An iteration [x*] or [x?] becomes a
   nonterminal of that name with rules of its own. A lexical tag and the
   primitive after it become one terminal. Nonterminal 0 is the augmented
   start [start' ::= start]. *)

(* A terminal: an atom, by its surface text, or a primitive of a lexical class
   ([None] for none). *)
type matcher = Text of string | Prim of Grammar.primitive * string option
type symbol = N of int | T of matcher

type rule = {
  lhs : int;
  rhs : symbol array;
  carries : bool array;  (** Which rhs positions contribute a value. *)
  build : Il.value list -> Il.value;
  label : string;
}

type bnf = {
  names : string array;
  rules : rule array;
  by_lhs : int list array;  (** Rule indices per nonterminal, ascending. *)
  nullable : bool array;
}

let rec typ_of_arg (arg : Grammar.arg) : Il.typ =
  let typ' : Il.typ' =
    match arg with
    | Grammar.Nonterminal id -> Il.Typ.var id.it []
    | Grammar.Primitive (Grammar.Num numtyp) -> Il.NumT numtyp
    | Grammar.Primitive Grammar.Bool -> Il.BoolT
    | Grammar.Primitive Grammar.Text -> Il.TextT
    | Grammar.Iter (arg, iter) -> Il.IterT { typ = typ_of_arg arg; iter }
  in
  typ' $ no_region

let to_bnf (lexicon : _ lexicon) (grammar : Grammar.t) (start : string) : bnf =
  let find_syntax name =
    match
      List.find_opt (fun (s : Grammar.syntax) -> s.name.it = name) grammar
    with
    | Some syntax -> syntax
    | None -> failwith ("Earley: grammar references unknown syntax " ^ name)
  in
  let index = Hashtbl.create 64 and names = ref [] in
  let intern name =
    match Hashtbl.find_opt index name with
    | Some i -> i
    | None ->
        let i = Hashtbl.length index in
        Hashtbl.add index name i;
        names := name :: !names;
        i
  in
  let rules = ref [] in
  let add_rule lhs rhs build label =
    let rhs, carries = List.split rhs in
    rules :=
      {
        lhs;
        rhs = Array.of_list rhs;
        carries = Array.of_list carries;
        build;
        label;
      }
      :: !rules
  in
  let atom_symbols atom =
    List.map
      (fun a -> (T (Text (Xl.Atom.to_string a)), false))
      (lexicon.spell atom)
  in
  (* The primitive a syntax stands for through plain inclusions, such as text
     for [id = text]: what a lexical tag in front of it constrains. *)
  let rec primitive_of name seen =
    if List.mem name seen then None
    else
      match (find_syntax name).productions with
      | [ { construction = Grammar.Alias; notation = [ Mixfix.Arg arg ]; _ } ]
        -> (
          match arg with
          | Grammar.Primitive prim -> Some prim
          | Grammar.Nonterminal sub -> primitive_of sub.it (name :: seen)
          | Grammar.Iter _ -> None)
      | _ -> None
  in
  let rec symbol_of_arg (arg : Grammar.arg) : symbol =
    match arg with
    | Grammar.Nonterminal id ->
        ignore (find_syntax id.it);
        N (intern id.it)
    | Grammar.Primitive prim -> T (Prim (prim, None))
    | Grammar.Iter (elem, iter) ->
        let name = string_of_arg arg in
        if Hashtbl.mem index name then N (intern name)
        else
          let self = intern name in
          iteration_rules self name elem iter;
          N self
  and iteration_rules self name elem iter =
    let elem_sym = (symbol_of_arg elem, true) in
    let typ = typ_of_arg elem in
    let elem_name = string_of_arg elem in
    match iter with
    | Il.Opt ->
        add_rule self [] (fun _ -> Il.Value.opt typ None) (name ^ " ::=");
        add_rule self [ elem_sym ]
          (function [ v ] -> Il.Value.opt typ (Some v) | _ -> assert false)
          (name ^ " ::= " ^ elem_name)
    | Il.List ->
        (* [items] is the nonempty list, left-recursive so a long list does
           not nest. It carries its elements newest first; [self] reverses. *)
        let items_name = name ^ "'" in
        let items = intern items_name in
        let sep =
          match elem with
          | Grammar.Nonterminal id -> lexicon.separator id.it
          | _ -> None
        in
        let sep_syms = match sep with Some a -> atom_symbols a | None -> [] in
        let sep_text =
          match sep with Some a -> " " ^ Xl.Atom.to_string a | None -> ""
        in
        let elements (v : Il.value) =
          match v.it with Il.ListV vs -> vs | _ -> assert false
        in
        let list vs = Il.Value.list typ vs in
        add_rule items [ elem_sym ]
          (function [ v ] -> list [ v ] | _ -> assert false)
          (items_name ^ " ::= " ^ elem_name);
        add_rule items
          (((N items, true) :: sep_syms) @ [ elem_sym ])
          (function
            | [ acc; v ] -> list (v :: elements acc) | _ -> assert false)
          (items_name ^ " ::= " ^ items_name ^ sep_text ^ " " ^ elem_name);
        let finish = function
          | [ acc ] -> list (List.rev (elements acc))
          | _ -> assert false
        in
        add_rule self [] (fun _ -> list []) (name ^ " ::=");
        add_rule self [ (N items, true) ] finish (name ^ " ::= " ^ items_name);
        if sep_syms <> [] then
          add_rule self
            ((N items, true) :: sep_syms)
            finish
            (name ^ " ::= " ^ items_name ^ sep_text)
  in
  let rhs_of_notation (notation : Grammar.arg Mixfix.t) =
    let rec go pending = function
      | [] -> []
      | Mixfix.Atom { it = Xl.Atom.Tag tag; _ } :: rest ->
          let tag = (tag :> string) in
          go
            (if List.mem tag lexicon.lexical_tags then Some tag else pending)
            rest
      | Mixfix.Atom atom :: rest -> atom_symbols atom.it @ go pending rest
      | Mixfix.Arg arg :: rest -> (
          match (pending, arg) with
          | None, arg -> (symbol_of_arg arg, true) :: go None rest
          | Some tag, Grammar.Primitive prim ->
              (T (Prim (prim, Some tag)), true) :: go None rest
          | Some tag, Grammar.Nonterminal id -> (
              match primitive_of id.it [] with
              | Some prim -> (T (Prim (prim, Some tag)), true) :: go None rest
              | None ->
                  failwith
                    (Printf.sprintf
                       "Earley: lexical tag _%s precedes %s, which is not a \
                        primitive"
                       tag id.it))
          | Some tag, Grammar.Iter _ ->
              failwith
                (Printf.sprintf "Earley: lexical tag _%s precedes an iteration"
                   tag))
    in
    go None notation
  in
  let top = intern (start ^ "'") in
  ignore (find_syntax start);
  add_rule top
    [ (N (intern start), true) ]
    (function [ v ] -> v | _ -> assert false)
    (start ^ "' ::= " ^ start);
  List.iter
    (fun (syntax : Grammar.syntax) ->
      let lhs = intern syntax.name.it in
      List.iter
        (fun (prod : Grammar.production) ->
          add_rule lhs
            (rhs_of_notation prod.notation)
            (Parse.value_of_production prod)
            (string_of_production syntax.name.it prod))
        syntax.productions)
    grammar;
  let names = Array.of_list (List.rev !names) in
  let rules = Array.of_list (List.rev !rules) in
  let by_lhs = Array.make (Array.length names) [] in
  for r = Array.length rules - 1 downto 0 do
    by_lhs.(rules.(r).lhs) <- r :: by_lhs.(rules.(r).lhs)
  done;
  let nullable = Array.make (Array.length names) false in
  let changed = ref true in
  while !changed do
    changed := false;
    Array.iter
      (fun rule ->
        if
          (not nullable.(rule.lhs))
          && Array.for_all
               (function N x -> nullable.(x) | T _ -> false)
               rule.rhs
        then (
          nullable.(rule.lhs) <- true;
          changed := true))
      rules
  done;
  { names; rules; by_lhs; nullable }

(* Recognition *)

type token = { terminal : Terminal.t option; tag : string option }

let matches (m : matcher) (tok : token) =
  match (m, tok.terminal) with
  | Text s, Some (Terminal.Atom a) -> Xl.Atom.to_string a = s
  | Prim (prim, tag), Some (Terminal.Primitive (p, _)) ->
      prim = p && tag = tok.tag
  | _ -> false

let string_of_matcher = function
  | Text s -> s
  | Prim (prim, tag) ->
      "<"
      ^ string_of_arg (Grammar.Primitive prim)
      ^ (match tag with Some t -> ":" ^ t | None -> "")
      ^ ">"

(* Item [rule, dot, origin] in set j: [rule]'s rhs before [dot] derives the
   tokens from [origin] up to (not including) j. *)
type item = { rule : int; dot : int; origin : int }

let next_symbol (bnf : bnf) item =
  let rhs = bnf.rules.(item.rule).rhs in
  if item.dot < Array.length rhs then Some rhs.(item.dot) else None

let recognize (bnf : bnf) (tokens : token array) : item list array =
  let n = Array.length tokens in
  let sets = Array.make (n + 1) [] in
  let seen = Array.init (n + 1) (fun _ -> Hashtbl.create 64) in
  let agenda = Array.init (n + 1) (fun _ -> Queue.create ()) in
  (* (j, x) -> the items of set j whose next symbol is nonterminal x. *)
  let waiting = Hashtbl.create 1024 in
  let add j item =
    if not (Hashtbl.mem seen.(j) item) then (
      Hashtbl.add seen.(j) item ();
      sets.(j) <- item :: sets.(j);
      (match next_symbol bnf item with
      | Some (N x) -> Hashtbl.add waiting (j, x) item
      | _ -> ());
      Queue.push item agenda.(j))
  in
  let advance item = { item with dot = item.dot + 1 } in
  add 0 { rule = 0; dot = 0; origin = 0 };
  for j = 0 to n do
    while not (Queue.is_empty agenda.(j)) do
      let item = Queue.pop agenda.(j) in
      match next_symbol bnf item with
      | None ->
          (* Complete: advance the items in the origin set that wait on it. *)
          let lhs = bnf.rules.(item.rule).lhs in
          List.iter
            (fun waiter -> add j (advance waiter))
            (Hashtbl.find_all waiting (item.origin, lhs))
      | Some (N x) ->
          List.iter
            (fun r -> add j { rule = r; dot = 0; origin = j })
            bnf.by_lhs.(x);
          (* x may derive nothing; this also covers completions of x at j that
             happened before this item was added (Aycock and Horspool 2002). *)
          if bnf.nullable.(x) then add j (advance item)
      | Some (T m) ->
          if j < n && matches m tokens.(j) then add (j + 1) (advance item)
    done
  done;
  sets

(* Derivations

   After recognition, every complete item [rule, end, i] in set j says the
   rule derives tokens i to j. [count] and [ways] count derivations through
   these; [build_nt] follows the first one, building its value and noting each
   place another one branches off. *)

let saturating_add a b = if a > max_int - b then max_int else a + b

let saturating_mul a b =
  if a = 0 || b = 0 then 0 else if a > max_int / b then max_int else a * b

let derive (bnf : bnf) (tokens : token array) (sets : item list array) =
  let completed = Hashtbl.create 1024 and ends = Hashtbl.create 1024 in
  Array.iteri
    (fun j items ->
      List.iter
        (fun item ->
          if next_symbol bnf item = None then (
            let x = bnf.rules.(item.rule).lhs in
            Hashtbl.add completed (x, item.origin, j) item.rule;
            if not (List.mem j (Hashtbl.find_all ends (x, item.origin))) then
              Hashtbl.add ends (x, item.origin) j))
        items)
    sets;
  let rules_of x i j =
    List.sort_uniq compare (Hashtbl.find_all completed (x, i, j))
  in
  let ends_of x i = List.sort compare (Hashtbl.find_all ends (x, i)) in
  let count_memo = Hashtbl.create 1024 and ways_memo = Hashtbl.create 4096 in
  (* A nonterminal that derives itself over the same span would loop; the
     memo entry is 0 while it is being computed, which cuts the cycle. *)
  let rec count x i j =
    match Hashtbl.find_opt count_memo (x, i, j) with
    | Some c -> c
    | None ->
        Hashtbl.replace count_memo (x, i, j) 0;
        let c =
          List.fold_left
            (fun acc r -> saturating_add acc (ways r 0 i j))
            0 (rules_of x i j)
        in
        Hashtbl.replace count_memo (x, i, j) c;
        c
  (* How many ways rule [r]'s rhs from position [k] derives tokens p to j. *)
  and ways r k p j =
    match Hashtbl.find_opt ways_memo (r, k, p, j) with
    | Some c -> c
    | None ->
        let rhs = bnf.rules.(r).rhs in
        let c =
          if k = Array.length rhs then if p = j then 1 else 0
          else
            match rhs.(k) with
            | T m ->
                if p < j && matches m tokens.(p) then ways r (k + 1) (p + 1) j
                else 0
            | N x ->
                List.fold_left
                  (fun acc q ->
                    if q > j then acc
                    else
                      saturating_add acc
                        (saturating_mul (count x p q) (ways r (k + 1) q j)))
                  0 (ends_of x p)
        in
        Hashtbl.replace ways_memo (r, k, p, j) c;
        c
  in
  let ambiguities = ref [] in
  let note syntax i j readings =
    ambiguities := { syntax; first = i; last = j - 1; readings } :: !ambiguities
  in
  let token_value p =
    match tokens.(p).terminal with
    | Some (Terminal.Primitive (_, v)) -> v
    | _ -> assert false
  in
  let rec build_nt x i j =
    let viable = List.filter (fun r -> ways r 0 i j > 0) (rules_of x i j) in
    (match viable with
    | _ :: _ :: _ ->
        note bnf.names.(x) i j (List.map (fun r -> bnf.rules.(r).label) viable)
    | _ -> ());
    build_rule (List.hd viable) i j
  and build_rule r i j =
    let rule = bnf.rules.(r) in
    let rec go k p acc =
      if k = Array.length rule.rhs then List.rev acc
      else
        let keep v acc =
          if rule.carries.(k) then Lazy.force v :: acc else acc
        in
        match rule.rhs.(k) with
        | T _ -> go (k + 1) (p + 1) (keep (lazy (token_value p)) acc)
        | N x ->
            let splits =
              List.filter
                (fun q -> q <= j && count x p q > 0 && ways r (k + 1) q j > 0)
                (ends_of x p)
            in
            (match splits with
            | _ :: _ :: _ ->
                note bnf.names.(rule.lhs) i j
                  (List.map
                     (fun q ->
                       Printf.sprintf "%s, with %s ending before token %d"
                         rule.label bnf.names.(x) q)
                     splits)
            | _ -> ());
            let q = List.hd splits in
            go (k + 1) q (keep (lazy (build_nt x p q)) acc)
    in
    rule.build (go 0 i [])
  in
  let n = Array.length tokens in
  let trees = count 0 0 n in
  let value = build_nt 0 0 n in
  (value, trees, List.rev !ambiguities)

let run (lexicon : 'tok lexicon) (grammar : Grammar.t) (start : string)
    (toks : 'tok list) : outcome =
  let bnf = to_bnf lexicon grammar start in
  let tokens =
    Array.of_list
      (List.map
         (fun tok ->
           { terminal = lexicon.classify tok; tag = lexicon.lexical_tag tok })
         toks)
  in
  let sets = recognize bnf tokens in
  let n = Array.length tokens in
  let accepted =
    List.exists
      (fun item ->
        item.rule = 0 && item.origin = 0 && next_symbol bnf item = None)
      sets.(n)
  in
  if accepted then
    let value, trees, ambiguities = derive bnf tokens sets in
    Accepted { value; trees; ambiguities }
  else
    (* Sets fill left to right, so the last nonempty one is where every parse
       stopped. *)
    let rec furthest j =
      if j > 0 && sets.(j) = [] then furthest (j - 1) else j
    in
    let at = furthest n in
    let expected =
      List.filter_map
        (fun item ->
          match next_symbol bnf item with
          | Some (T m) -> Some (string_of_matcher m)
          | _ -> None)
        sets.(at)
      |> List.sort_uniq compare
    in
    Rejected { at; expected }
