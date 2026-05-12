open Lang.Il
open Common.Source

(* ===== Shared value construction helpers ===== *)

let id_val name =
  Value.case_v ~var:"id" [Value.atom "`ID"; Value.arg (Value.text name)]

(* expr values: literal and id cases are FLAT in the IL (elab_typcase_plain
   expands | literal and | id into expr's VariantT directly, no wrapper) *)
let expr_num  n      = Value.case_v ~var:"expr" [Value.atom "`NUM";  Value.arg (Value.nat (Bigint.of_int n))]
let expr_bool b      = Value.case_v ~var:"expr" [Value.atom "`BOOL"; Value.arg (Value.bool b)]
let expr_var  s      = Value.case_v ~var:"expr" [Value.atom "`ID";   Value.arg (Value.text s)]
let expr_add  e1 e2  = Value.case_v ~var:"expr" [Value.arg e1; Value.atom "+";  Value.arg e2]
let expr_leq  e1 e2  = Value.case_v ~var:"expr" [Value.arg e1; Value.atom "<="; Value.arg e2]
let expr_not  e      = Value.case_v ~var:"expr" [Value.atom "!";  Value.arg e]
let expr_and  e1 e2  = Value.case_v ~var:"expr" [Value.arg e1; Value.atom "&&"; Value.arg e2]

(* closure-only expr constructors *)
(* FUN `( type_arg id_arg ) -> type_ret `{ body }
   Mixop: [Atom FUN; Atom LParen; Arg type; Arg id; Atom RParen; Atom Arrow; Arg type; Atom LBrace; Arg expr; Atom RBrace] *)
let expr_fun ta xa tr e =
  Value.case_v ~var:"expr"
    [Value.atom "FUN"; Value.atom "("; Value.arg ta; Value.arg xa;
     Value.atom ")"; Value.atom "->"; Value.arg tr;
     Value.atom "{"; Value.arg e; Value.atom "}"]

(* expr_f `( expr_a )
   Mixop: [Arg expr; Atom LParen; Arg expr; Atom RParen] *)
let expr_call ef ea =
  Value.case_v ~var:"expr" [Value.arg ef; Value.atom "("; Value.arg ea; Value.atom ")"]

let cmd_skip         = Value.case_v ~var:"command" [Value.atom "SKIP"]
let cmd_decl ty id e = Value.case_v ~var:"command" [Value.arg ty; Value.arg id; Value.atom "="; Value.arg e]
let cmd_assign id e  = Value.case_v ~var:"command" [Value.arg id; Value.atom "="; Value.arg e]
let cmd_seq   c1 c2  = Value.case_v ~var:"command" [Value.arg c1; Value.atom ";"; Value.arg c2]
let cmd_ite e c1 c2  = Value.case_v ~var:"command"
    [Value.atom "IF"; Value.arg e; Value.atom "THEN"; Value.arg c1;
     Value.atom "ELSE"; Value.arg c2; Value.atom "END"]
let cmd_while e c    = Value.case_v ~var:"command"
    [Value.atom "WHILE"; Value.arg e; Value.atom "DO"; Value.arg c; Value.atom "END"]

let fresh_name (ctx : ('a * 'b) list) = Printf.sprintf "x%d" (List.length ctx)

(* ===== Base Impty generator (INT, BOOL only) ===== *)

type impty_ty = TInt | TBool
type ctx = (string * impty_ty) list

let type_val = function
  | TInt  -> Value.case_v ~var:"type" [Value.atom "INT"]
  | TBool -> Value.case_v ~var:"type" [Value.atom "BOOL"]

let vars_of ctx ty =
  List.filter_map (fun (name, t) -> if t = ty then Some name else None) ctx

let rec gen_expr (ctx : ctx) (ty : impty_ty) : Value.t Gen.t =
  let open Gen in
  sized (fun size ->
    match ty with
    | TInt ->
      let int_vars = vars_of ctx TInt in
      let base =
        (3, (let* n = choose_int (0, max 1 size) in
             return (expr_num n))) ::
        (if int_vars <> [] then
           [(2, (let* name = elements int_vars in return (expr_var name)))]
         else [])
      in
      let recursive =
        if size <= 0 then []
        else
          [(1, (let* e1 = scale (fun n -> n / 2) (gen_expr ctx TInt) in
                let* e2 = scale (fun n -> n / 2) (gen_expr ctx TInt) in
                return (expr_add e1 e2)))]
      in
      frequency (base @ recursive)
    | TBool ->
      let bool_vars = vars_of ctx TBool in
      let base =
        (3, (let* b = Arbitrary.Bool.arbitrary in
             return (expr_bool b))) ::
        (if bool_vars <> [] then
           [(2, (let* name = elements bool_vars in return (expr_var name)))]
         else [])
      in
      let recursive =
        if size <= 0 then []
        else
          [ (1, (let* e1 = scale (fun n -> n / 2) (gen_expr ctx TInt) in
                 let* e2 = scale (fun n -> n / 2) (gen_expr ctx TInt) in
                 return (expr_leq e1 e2)));
            (1, (let* e = scale (fun n -> n / 2) (gen_expr ctx TBool) in
                 return (expr_not e)));
            (1, (let* e1 = scale (fun n -> n / 2) (gen_expr ctx TBool) in
                 let* e2 = scale (fun n -> n / 2) (gen_expr ctx TBool) in
                 return (expr_and e1 e2))) ]
      in
      frequency (base @ recursive)
  )

(* Returns the generated command together with the output context, because
   seq must thread the context from c1 into c2. *)
and gen_command (ctx : ctx) : (Value.t * ctx) Gen.t =
  let open Gen in
  sized (fun size ->
    let base = [(1, return (cmd_skip, ctx))] in
    let decl =
      let name = fresh_name ctx in
      let* ty   = elements [TInt; TBool] in
      let* e    = gen_expr ctx ty in
      return (cmd_decl (type_val ty) (id_val name) e, (name, ty) :: ctx)
    in
    let assign =
      if ctx = [] then []
      else
        [(1, (let* (name, ty) = elements ctx in
              let* e = gen_expr ctx ty in
              return (cmd_assign (id_val name) e, ctx)))]
    in
    let recursive =
      if size <= 0 then []
      else
        [ (2, (let* (c1, ctx1) = resize (size / 2) (gen_command ctx)  in
               let* (c2, ctx2) = resize (size / 2) (gen_command ctx1) in
               return (cmd_seq c1 c2, ctx2)));
          (1, (let* e       = gen_expr ctx TBool in
               let* (c1, _) = resize (size / 2) (gen_command ctx) in
               let* (c2, _) = resize (size / 2) (gen_command ctx) in
               return (cmd_ite e c1 c2, ctx)));
          (1, (let* e      = gen_expr ctx TBool in
               let* (c, _) = resize (size / 2) (gen_command ctx) in
               return (cmd_while e c, ctx))) ]
    in
    frequency (base @ [(2, decl)] @ assign @ recursive)
  )

let gen_well_typed_prog : (string * value) list Gen.t =
  let open Gen in
  let* (cmd, _) = gen_command [] in
  return [("prog", cmd)]

(* ===== Closure Impty generator (INT, BOOL, function types) ===== *)

type closure_ty =
  | CInt
  | CBool
  | CFun of closure_ty * closure_ty

type cctx = (string * closure_ty) list

let rec ctype_val = function
  | CInt  -> Value.case_v ~var:"type" [Value.atom "INT"]
  | CBool -> Value.case_v ~var:"type" [Value.atom "BOOL"]
  | CFun (t1, t2) ->
    Value.case_v ~var:"type" [Value.arg (ctype_val t1); Value.atom "->"; Value.arg (ctype_val t2)]

let cvars_of (ctx : cctx) ty =
  List.filter_map (fun (name, t) -> if t = ty then Some name else None) ctx

(* Find variables with type (arg_ty -> ret_ty); return (name, arg_ty) pairs. *)
let cfun_vars_of (ctx : cctx) ret_ty =
  List.filter_map (fun (name, t) ->
    match t with
    | CFun (arg_ty, r) when r = ret_ty -> Some (name, arg_ty)
    | _ -> None) ctx

let gen_simple_cty : closure_ty Gen.t = Gen.elements [CInt; CBool]

(* Generate a random type up to the given nesting depth. *)
let rec gen_cty (depth : int) : closure_ty Gen.t =
  let open Gen in
  if depth <= 0 then gen_simple_cty
  else
    frequency
      [ (3, gen_simple_cty);
        (1, (let* t1 = gen_cty (depth - 1) in
             let* t2 = gen_cty (depth - 1) in
             return (CFun (t1, t2)))) ]

let rec gen_cexpr (ctx : cctx) (ty : closure_ty) : Value.t Gen.t =
  let open Gen in
  sized (fun size ->
    match ty with
    | CInt ->
      let ivars = cvars_of ctx CInt in
      let fvars = cfun_vars_of ctx CInt in
      let base =
        [(3, (let* n = choose_int (0, max 1 size) in return (expr_num n)))] @
        (if ivars <> [] then
           [(2, (let* v = elements ivars in return (expr_var v)))]
         else [])
      in
      let recursive =
        if size <= 0 then []
        else
          [(1, (let* e1 = scale (fun n -> n / 2) (gen_cexpr ctx CInt) in
                let* e2 = scale (fun n -> n / 2) (gen_cexpr ctx CInt) in
                return (expr_add e1 e2)))] @
          (if fvars <> [] then
             [(1, (let* (fname, at) = elements fvars in
                   let* ea = scale (fun n -> n / 2) (gen_cexpr ctx at) in
                   return (expr_call (expr_var fname) ea)))]
           else [])
      in
      frequency (base @ recursive)
    | CBool ->
      let bvars = cvars_of ctx CBool in
      let fvars = cfun_vars_of ctx CBool in
      let base =
        [(3, (let* b = Arbitrary.Bool.arbitrary in return (expr_bool b)))] @
        (if bvars <> [] then
           [(2, (let* v = elements bvars in return (expr_var v)))]
         else [])
      in
      let recursive =
        if size <= 0 then []
        else
          [ (1, (let* e1 = scale (fun n -> n / 2) (gen_cexpr ctx CInt) in
                 let* e2 = scale (fun n -> n / 2) (gen_cexpr ctx CInt) in
                 return (expr_leq e1 e2)));
            (1, (let* e = scale (fun n -> n / 2) (gen_cexpr ctx CBool) in
                 return (expr_not e)));
            (1, (let* e1 = scale (fun n -> n / 2) (gen_cexpr ctx CBool) in
                 let* e2 = scale (fun n -> n / 2) (gen_cexpr ctx CBool) in
                 return (expr_and e1 e2))) ] @
          (if fvars <> [] then
             [(1, (let* (fname, at) = elements fvars in
                   let* ea = scale (fun n -> n / 2) (gen_cexpr ctx at) in
                   return (expr_call (expr_var fname) ea)))]
           else [])
      in
      frequency (base @ recursive)
    | CFun (ta, tr) ->
      let fvars = cvars_of ctx ty in
      let param = fresh_name ctx in
      let ext_ctx = (param, ta) :: ctx in
      (* Body is always generated at half size; since gen_cty uses depth=1,
         ta and tr are always simple types, so this terminates. *)
      let base =
        [(2, (let* body = scale (fun n -> n / 2) (gen_cexpr ext_ctx tr) in
              return (expr_fun (ctype_val ta) (id_val param) (ctype_val tr) body)))] @
        (if fvars <> [] then
           [(2, (let* v = elements fvars in return (expr_var v)))]
         else [])
      in
      frequency base
  )

and gen_ccommand (ctx : cctx) : (Value.t * cctx) Gen.t =
  let open Gen in
  sized (fun size ->
    let base = [(1, return (cmd_skip, ctx))] in
    let decl =
      let name = fresh_name ctx in
      let* ty = gen_cty 1 in
      let* e  = gen_cexpr ctx ty in
      return (cmd_decl (ctype_val ty) (id_val name) e, (name, ty) :: ctx)
    in
    let assign =
      if ctx = [] then []
      else
        [(1, (let* (name, ty) = elements ctx in
              let* e = gen_cexpr ctx ty in
              return (cmd_assign (id_val name) e, ctx)))]
    in
    let recursive =
      if size <= 0 then []
      else
        [ (2, (let* (c1, ctx1) = resize (size / 2) (gen_ccommand ctx)  in
               let* (c2, ctx2) = resize (size / 2) (gen_ccommand ctx1) in
               return (cmd_seq c1 c2, ctx2)));
          (1, (let* e       = gen_cexpr ctx CBool in
               let* (c1, _) = resize (size / 2) (gen_ccommand ctx) in
               let* (c2, _) = resize (size / 2) (gen_ccommand ctx) in
               return (cmd_ite e c1 c2, ctx)));
          (1, (let* e      = gen_cexpr ctx CBool in
               let* (c, _) = resize (size / 2) (gen_ccommand ctx) in
               return (cmd_while e c, ctx))) ]
    in
    frequency (base @ [(2, decl)] @ assign @ recursive)
  )

let gen_closure_prog : (string * value) list Gen.t =
  let open Gen in
  let* (cmd, _) = gen_ccommand [] in
  return [("prog", cmd)]

(* ===== Dispatch ===== *)

let gen_inputs (_spec : spec) (name : string) :
    (string * value) list Gen.t option =
  ignore no_region;
  match name with
  | "base_prog" -> Some gen_well_typed_prog
  | "closure_prog" -> Some gen_closure_prog
  | _ -> None
