open Common.Source
module Il = Lang.Il
module Value = Il.Value

module Func = struct
  type t = int
end

module Rel = struct
  type t = string
end

module Ctx = Interp_runtime.Context.Make (Func) (Rel)

let id name = name $ no_region
let var name = (id name, [])
let nat n = Value.nat (Bigint.of_int n)
let typ_nat = Il.Typ.nat $ no_region
let typdef = ([], Il.PlainT typ_nat $ no_region)
let itervar name iters : Il.var = { varid = id name; typ = typ_nat; iters }
let check name condition = if not condition then failwith name

let check_value ctx key expected =
  let actual = Ctx.find_value_exn ctx ~var:key in
  check "unexpected value binding" (Value.compare actual expected = 0)

let expect_error expected f =
  match f () with
  | _ -> failwith ("expected context error: " ^ expected)
  | exception Interp_runtime.Context.Error (_, message) ->
      check ("unexpected context error: " ^ message) (message = expected)

let expect_failure expected = function
  | Result.Error [ { Common.Attempt.message; _ } ] ->
      check ("unexpected iteration failure: " ^ message) (message = expected)
  | _ -> failwith ("expected iteration failure: " ^ expected)

let make global =
  let builtins = Builtins.make [] in
  let cache =
    Interp_runtime.Cache.make
      ~is_impure_func:(fun _ -> false)
      ~is_impure_rel:(fun _ -> false)
      ~state_version:(ref 0)
  in
  let opaque_types = Opaque_types.make [] |> Result.get_ok in
  Ctx.make global ~builtins ~cache ~opaque_types ~check_determinism:false

let () =
  let definitions =
    Ctx.Global.
      [
        Syntax (id "T", typdef); Function (id "f", 17); Relation (id "r", "body");
      ]
  in
  let global = Ctx.Global.of_definitions_exn definitions in
  let ctx = make global in
  check "global type" (Ctx.find_typdef_exn ctx ~id:(id "T") = typdef);
  check "global function" (Ctx.find_func_exn ctx ~id:(id "f") = 17);
  check "global relation" (Ctx.find_rel_exn ctx ~id:(id "r") = "body");
  check "global function is not local"
    (not (Ctx.has_local_func ctx ~id:(id "f")));
  expect_error "function `f` was already defined" (fun () ->
      Ctx.Global.of_definitions_exn (definitions @ [ Function (id "f", 18) ]));
  expect_error "function `f` was already defined" (fun () ->
      Ctx.add_func_exn ctx ~id:(id "f") ~func:18);
  let ctx_bound = Ctx.add_value_exn ctx ~var:(var "x") ~value:(nat 1) in
  check "insertion preserves parent" (not (Ctx.has_value ctx ~var:(var "x")));
  let ctx_rebound = Ctx.set_value ctx_bound ~var:(var "x") ~value:(nat 2) in
  check_value ctx_bound (var "x") (nat 1);
  check_value ctx_rebound (var "x") (nat 2);
  expect_error "value `x` was already defined" (fun () ->
      Ctx.add_value_exn ctx_bound ~var:(var "x") ~value:(nat 2));
  expect_error "value `x` was already defined" (fun () ->
      Ctx.add_values_exn ctx ~bindings:[ (var "x", nat 1); (var "x", nat 2) ]);
  check "failed batch preserves parent" (not (Ctx.has_value ctx ~var:(var "x")));
  let ctx_rebound =
    Ctx.set_values ctx ~bindings:[ (var "x", nat 1); (var "x", nat 2) ]
  in
  check_value ctx_rebound (var "x") (nat 2);
  let ctx_local =
    Ctx.add_typdef_exn ctx_bound ~id:(id "U") ~typdef |> fun ctx ->
    Ctx.add_func_exn ctx ~id:(id "g") ~func:23
  in
  let ctx_empty_values = Ctx.without_values ctx_local in
  check "value scope" (not (Ctx.has_value ctx_empty_values ~var:(var "x")));
  check "value scope retains types"
    (Ctx.find_typdef_exn ctx_empty_values ~id:(id "U") = typdef);
  check "value scope retains functions"
    (Ctx.find_func_exn ctx_empty_values ~id:(id "g") = 23);
  let ctx_callee = Ctx.for_call ctx_local in
  check "call scope has no values"
    (not (Ctx.has_value ctx_callee ~var:(var "x")));
  check "call scope has no local types"
    (Ctx.find_typdef_opt ctx_callee ~id:(id "U") = None);
  check "call scope has no local functions"
    (Ctx.find_func_opt ctx_callee ~id:(id "g") = None);
  check "call scope retains globals"
    (Ctx.find_func_exn ctx_callee ~id:(id "f") = 17);
  check_value ctx_local (var "x") (nat 1);
  let vars = [ itervar "x" []; itervar "y" [] ] in
  let optional = Value.opt typ_nat in
  let ctx_options =
    Ctx.set_values ctx_bound
      ~bindings:
        [
          ((id "x", [ Il.Opt ]), optional (Some (nat 3)));
          ((id "y", [ Il.Opt ]), optional (Some (nat 4)));
        ]
  in
  let ctx_iteration =
    Ctx.for_optional_iteration ctx_options ~vars |> Result.get_ok |> Option.get
  in
  check_value ctx_iteration (var "x") (nat 3);
  check_value ctx_iteration (var "y") (nat 4);
  check_value ctx_options (var "x") (nat 1);
  let ctx_mixed =
    Ctx.set_value ctx_options ~var:(id "y", [ Il.Opt ]) ~value:(optional None)
  in
  Ctx.for_optional_iteration ctx_mixed ~vars
  |> expect_failure "mismatch in optionality of iterated variables";
  let ctx_absent =
    Ctx.set_value ctx_mixed ~var:(id "x", [ Il.Opt ]) ~value:(optional None)
  in
  check "absent optional bindings"
    (Ctx.for_optional_iteration ctx_absent ~vars = Ok None);
  let ctx_nested =
    Ctx.set_value ctx
      ~var:(id "x", [ Il.Opt; Il.List ])
      ~value:
        (Value.list
           (Il.Typ.opt typ_nat $ no_region)
           [ optional (Some (nat 5)); optional None ])
  in
  let ctxs =
    Ctx.for_list_iteration ctx_nested ~vars:[ itervar "x" [ Il.Opt ] ]
    |> Result.get_ok
  in
  check "nested list length" (List.length ctxs = 2);
  let ctx_first =
    Ctx.for_optional_iteration (List.hd ctxs) ~vars:[ itervar "x" [] ]
    |> Result.get_ok |> Option.get
  in
  check_value ctx_first (var "x") (nat 5);
  check "nested absence"
    (Ctx.for_optional_iteration (List.nth ctxs 1) ~vars:[ itervar "x" [] ]
    = Ok None);
  check "nested binding preserves parent"
    (not (Ctx.has_value ctx_nested ~var:(id "x", [ Il.Opt ])));
  Interp_runtime.Context.transpose_values [ [ nat 1 ]; [ nat 2; nat 3 ] ]
  |> expect_failure "cannot transpose a matrix of value batches"
