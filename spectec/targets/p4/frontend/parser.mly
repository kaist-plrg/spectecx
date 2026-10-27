%{
  open Lang.Il
  open Lang.Il.Value
  open Lang.Il.Case
  open Context
  open Extract

  let at = Common.Source.region_of_positions

  (* The region of characters [l, r) of a single-line token at [loc] *)
  let at_chars (position_left, _) l r =
    let shift n =
      { position_left with Lexing.pos_cnum = position_left.Lexing.pos_cnum + n }
    in
    at (shift l, shift r)

  let at_first_char loc = at_chars loc 0 1

  (* A string literal can span lines, so its closing quote is located from
     the token's end. *)
  let at_last_char (_, position_right) =
    let shift n =
      { position_right with Lexing.pos_cnum = position_right.Lexing.pos_cnum + n }
    in
    at (shift (-1), position_right)

  (* [@pragma] and [pragma] both yield the [PRAGMA] token.
     The spec always expresses this with two atoms: ['@' PRAGMA]. *)
  let at_pragma ((position_left, position_right) as loc) =
    if position_right.Lexing.pos_cnum - position_left.Lexing.pos_cnum = 7
    then (at_chars loc 0 1, at_chars loc 1 7)
    else (Common.Source.no_region, at loc)

  (* Empty leading nonterminals can move Menhir's start position before
     the first source token. Use the first leading value that spans text,
     and retain Menhir's end position. *)
  let at_v (position_left, position_right) (leading : value list) =
    let region = at (position_left, position_right) in
    let nonempty (v : value) =
      let open Common.Source in
      v.at.left <> v.at.right && v.at <> no_region
    in
    match List.find_opt nonempty leading with
    | Some v -> { region with left = v.at.left }
    | None -> region

  let case var region mixemes = mixemes |> case_v ~var <<<| region

  (* Atoms without a source token, or with a computed region, use the
     corresponding [Case] constructors directly. *)
  let kw loc id = Case.kw ~at:(at loc) id
  let op loc s = Case.op ~at:(at loc) s
  let lparen loc = Case.lparen ~at:(at loc) ()
  let rparen loc = Case.rparen ~at:(at loc) ()
  let lbrack loc = Case.lbrack ~at:(at loc) ()
  let rbrack loc = Case.rbrack ~at:(at loc) ()
  let lbrace loc = Case.lbrace ~at:(at loc) ()
  let rbrace loc = Case.rbrace ~at:(at loc) ()
  let langle loc = Case.langle ~at:(at loc) ()
  let rangle loc = Case.rangle ~at:(at loc) ()

  let declare_var_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_var id b

  let rec declare_vars_of_il (v: value) : unit =
    match flatten_case_v v with
    | "nameList", [","], [ v_nameList; v_name ] ->
        declare_vars_of_il v_nameList;
        declare_var_of_il v_name false
    | "identifier", _, _
    | "nonTypeName", _, _
    | "name", _, _
    | "typeIdentifier", _, _ -> declare_var_of_il v false
    | _ -> failwith
        (Printf.sprintf "@declare_vars_of_il: expected name, got %s"
           (id_of_case_v v))

  let declare_type_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_type id b

  let rec declare_types_of_il (v: value) : unit =
    match flatten_case_v v with
    | "typeParameterList", [","], [ v_tpList; v_name ] ->
        declare_types_of_il v_tpList;
        declare_type_of_il v_name false
    | "identifier", _, _
    | "nonTypeName", _, _
    | "name", _, _
    | "typeIdentifier", _, _ -> declare_type_of_il v false
    | _ -> failwith
        (Printf.sprintf "@declare_types_of_il: expected name, got %s"
           (id_of_case_v v))
%}

(**************************** TOKENS ******************************)
%token END
%token TYPENAME IDENTIFIER
%token<Lang.Il.value> NAME STRING_LITERAL
%token<Lang.Il.value * string> NUMBER_INT NUMBER
%token LE GE SHL AND OR NE EQ
%token PLUS MINUS PLUS_SAT MINUS_SAT MUL INVALID DIV MOD
%token BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token AT PLUSPLUS
%token DONTCARE
%token MASK DOTS RANGE
%token TRUE FALSE
%token ABSTRACT ACTION ACTIONS APPLY BOOL BIT BREAK CONST CONTINUE CONTROL DEFAULT
%token ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT FOR
%token INT KEY LIST SELECT MATCH_KIND OUT PACKAGE PARSER PRIORITY RETURN STATE STRING STRUCT
%token SWITCH TABLE THIS TRANSITION TUPLE TYPEDEF TYPE VALUE_SET VARBIT VOID
%token PRAGMA PRAGMA_END
%token PLUS_ASSIGN PLUS_SAT_ASSIGN MINUS_ASSIGN MINUS_SAT_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN  SHL_ASSIGN SHR_ASSIGN BIT_AND_ASSIGN BIT_XOR_ASSIGN BIT_OR_ASSIGN
%token<Lang.Il.value> UNEXPECTED_TOKEN

(**************************** PRIORITY AND ASSOCIATIVITY ******************************)
%right THEN ELSE
%nonassoc QUESTION
%nonassoc COLON
%left OR
%left AND
%left EQ NE
%left L_ANGLE R_ANGLE LE GE
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left SHL R_ANGLE_SHIFT
%left PLUSPLUS PLUS MINUS PLUS_SAT MINUS_SAT
%left MUL DIV MOD
%right PREFIX
%nonassoc L_PAREN L_BRACKET L_ANGLE_ARGS
%left DOT

%start p4program

(**************************** TYPES ******************************)
%type <Lang.Il.value>
  (* Aux *) int externName declarationList
  (* Misc *) trailingCommaOpt (* Numbers *) number (* Strings *) stringLiteral
  (* Names *)
  identifier typeIdentifier nonTypeName prefixedNonTypeName typeName prefixedTypeName tableCustomName name nameList member
  (* Directions *) direction
  (* Types *)
  baseType specializedType namedType headerStackType listType tupleType typeRef typeOrVoid
  (* Type parameters *) typeParameter typeParameterList typeParameterListOpt
  (* Parameters *) parameter nonEmptyParameterList parameterList
  (* Constructor parameters *) constructorParameterListOpt
  (* Expression key-value pairs *) namedExpression namedExpressionList
  (* Expressions *)
  literalExpression referenceExpression defaultExpression
  (* >> Unary, binary, and ternary expressions *)
  unop unaryExpression binop binaryExpression binaryExpressionNonBrace ternaryExpression ternaryExpressionNonBrace
  (* >> Cast expressions *) castExpression
  (* >> Data (aggregate) expressions *) dataExpression
  (* >> Member and index access expressions *)
  errorAccessExpression memberAccessExpression indexAccessExpression accessExpression
  memberAccessExpressionNonBrace indexAccessExpressionNonBrace accessExpressionNonBrace
  (* >> Call expressions *)
  routineTarget constructorTarget callTarget callExpression
  routineTargetNonBrace callTargetNonBrace callExpressionNonBrace
  (* >> Parenthesized Expressions *) parenthesizedExpression
  (* >> Expressions *)
  expression expressionList memberAccessBase sequenceElementExpression recordElementExpression dataElementExpression
  (* >> Non-brace Expressions *) expressionNonBrace memberAccessBaseNonBrace
  (* Keyset Expressions *) simpleKeysetExpression simpleKeysetExpressionList tupleKeysetExpression keysetExpression
  (* Type arguments *)
  realTypeArgument realTypeArgumentList typeArgument typeArgumentList argument argumentListNonEmpty argumentList
  (* L-values *) lvalue
  (* Statements *)
  emptyStatement assignop assignmentStatement callStatement directApplicationStatement returnStatement exitStatement blockStatement conditionalStatement
  (* >> For statements *)
  forInitStatement forInitStatementListNonEmpty forInitStatementList forUpdateStatement forUpdateStatementListNonEmpty
  forUpdateStatementList forCollectionExpression forStatement
  (* >> Switch statements *) switchLabel switchCase switchCaseList switchStatement
  breakStatement continueStatement statement
  (* Declarations *)
  (* >> Constant and variable declarations *)
  initialValue constantDeclaration initializerOpt variableDeclaration blockElementStatement blockElementStatementList
  (* >> Function declarations *) functionPrototype functionDeclaration
  (* >> Action declarations *) actionDeclaration
  (* >> Instantiations *) objectInitializer instantiation objectDeclaration objectDeclarationList
  (* >> Error declarations *) errorDeclaration
  (* >> Match kind declarations *) matchKindDeclaration
  (* >> Derived type declarations *)
  enumTypeDeclaration typeField typeFieldList structTypeDeclaration headerTypeDeclaration headerUnionTypeDeclaration derivedTypeDeclaration
  (* >> Typedef and newtype declarations *) typedefType typedefDeclaration
  (* >> Extern declarations *)
  externFunctionDeclaration methodPrototype methodPrototypeList externObjectDeclaration externDeclaration
  (* >> Parser statements and declarations *)
  (* >>>> Select expressions *) selectCase selectCaseList selectExpression
  (* >>>> Transition statements *) stateExpression transitionStatement
  (* >>>> Value set declarations *) valueSetType valueSetDeclaration
  (* >>>> Parser type declarations *) parserTypeDeclaration
  (* >>>> Parser Declarations *)
  parserBlockStatement parserStatement parserStatementList parserState
  parserStateList parserLocalDeclaration parserLocalDeclarationList parserDeclaration
  (* >> Control statements and declarations *)
  (* >>>> Table declarations *) constOpt
  (* >>>>>> Table key property *) tableKey tableKeyList
  (* >>>>>> Table actions property *) tableActionReference tableAction tableActionList
  (* >>>>>> Table entry property *) tableEntryPriority tableEntry tableEntryList
  (* >>>>>> Table properties *) tableProperty tablePropertyList tableDeclaration
  (* >>>> Control type declarations *) controlTypeDeclaration
  (* >>>> Control declarations *) controlBody controlLocalDeclaration controlLocalDeclarationList controlDeclaration
  (* >> Package type declarations *) packageTypeDeclaration
  (* >> Type declarations *) typeDeclaration
  (* >> Declaration *) declaration
  (* Annotations *) annotationToken annotationBody structuredAnnotationBody annotation annotationListNonEmpty annotationList p4program
%type <Lang.Il.value> push_name push_externName
%type <unit> push_scope pop_scope go_toplevel go_local
%%

(**************************** CONTEXTS ******************************)
push_scope:
  | (* empty *)
    { push_scope() }
;
push_name:
  | n = name
   { push_scope();
     declare_type_of_il n false;
     n }
;
push_externName:
  | n = externName
    { push_scope();
      declare_type_of_il n false;
      n }
;
pop_scope:
  | (* empty *)
    { pop_scope() }
;
go_toplevel:
  | (* empty *)
    { go_toplevel () }
;
go_local:
  | (* empty *)
    { go_local () }
;
toplevel(X):
  | go_toplevel x = X go_local
    { x }
;

(**************************** P4-16 GRAMMAR ******************************)
(* Aux *)
externName:
  | n = nonTypeName
    { declare_type_of_il n false;
      n }
;
int:
  | int = NUMBER_INT
    { fst int }
;

%inline r_angle:
  | r_angle = R_ANGLE
    { r_angle }
  | r_angle = R_ANGLE_SHIFT
    { r_angle }
;
%inline l_angle:
  | l_angle = L_ANGLE
    { l_angle }
  | l_angle = L_ANGLE_ARGS
    { l_angle }
;

(* Misc *)
trailingCommaOpt:
  | (* empty *)
    { case "trailingCommaOpt" (at $sloc) [ tag "EMPTY" ] }
  | COMMA
    { case "trailingCommaOpt" (at $sloc) [ op $loc($1) "," ] }
;

(* Numbers *)
number:
  | int = int
    { case "number" (at_v $sloc [ int ]) [ Case.kw "D"; arg int ] }
(* Processed by lexer *)
  | number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
  | text = STRING_LITERAL
    { case "stringLiteral" (at $sloc) [ Case.op ~at:(at_first_char $loc(text)) (Char.escaped '"'); arg text;
         Case.op ~at:(at_last_char $loc(text)) (Char.escaped '"') ]}
;

(* Names *)
identifier:
  | text = NAME IDENTIFIER
    { case "identifier" (at $sloc) [ tag "ID"; arg text ] }
;

typeIdentifier:
  | text = NAME TYPENAME
    { case "typeIdentifier" (at $sloc) [ tag "TID"; arg text ] }
;

(* >> Non-type names *)
nonTypeName:
  | id = identifier { id }
  | APPLY { case "nonTypeName" (at $sloc) [ kw $loc($1) "APPLY" ] }
  | KEY { case "nonTypeName" (at $sloc) [ kw $loc($1) "KEY" ] }
  | ACTIONS { case "nonTypeName" (at $sloc) [ kw $loc($1) "ACTIONS" ] }
  | STATE { case "nonTypeName" (at $sloc) [ kw $loc($1) "STATE" ] }
  | ENTRIES { case "nonTypeName" (at $sloc) [ kw $loc($1) "ENTRIES" ] }
  | TYPE { case "nonTypeName" (at $sloc) [ kw $loc($1) "TYPE" ] }
  | PRIORITY { case "nonTypeName" (at $sloc) [ kw $loc($1) "PRIORITY" ] }
;

prefixedNonTypeName:
  | n = nonTypeName { n }
  | DOT go_toplevel n = nonTypeName go_local
    { case "prefixedNonTypeName" (at $sloc) [ tag "ID"; op $loc($1) "."; arg n ] }
;

(* >> Type names *)
typeName:
  | n = typeIdentifier { n }
;

prefixedTypeName:
  | n = typeName { n }
  | DOT go_toplevel tid = typeName go_local
    { case "prefixedType" (at $sloc) [ tag "TID"; op $loc($1) "."; arg tid ] }
;

(* >> Table custom property names *)
tableCustomName:
  | id = identifier { id }
  | tid = typeIdentifier { tid }
  | APPLY { case "tableCustomName" (at $sloc) [ kw $loc($1) "APPLY" ] }
  | STATE { case "tableCustomName" (at $sloc) [ kw $loc($1) "STATE" ] }
  | TYPE { case "tableCustomName" (at $sloc) [ kw $loc($1) "TYPE" ] }
  | PRIORITY { case "tableCustomName" (at $sloc) [ kw $loc($1) "PRIORITY" ] }
;

(* >> Names *)
name:
  | n = nonTypeName
  | n = typeName
    { n }
  | LIST { case "name" (at $sloc) [ kw $loc($1) "LIST" ] }
;

nameList:
  | n = name { n }
  | ns = nameList COMMA n = name
    { case "nameList" (at_v $sloc [ ns ]) [ arg ns; op $loc($2) ","; arg n ] }
;

member:
  | name = name
    { name }
;

(* Directions *)
direction:
  | (* empty *) { case "direction" (at $sloc) [ tag "EMPTY" ] }
  | IN { case "direction" (at $sloc) [ kw $loc($1) "IN" ] }
  | OUT { case "direction" (at $sloc) [ kw $loc($1) "OUT" ] }
  | INOUT { case "direction" (at $sloc) [ kw $loc($1) "INOUT" ] }
;

(* Types *)
(* >> Base types *)
baseType:
  | BOOL { case "baseType" (at $sloc) [ kw $loc($1) "BOOL" ] }
  | MATCH_KIND { case "baseType" (at $sloc) [ kw $loc($1) "MATCH_KIND" ] }
  | ERROR { case "baseType" (at $sloc) [ kw $loc($1) "ERROR" ] }
  | BIT { case "baseType" (at $sloc) [ kw $loc($1) "BIT" ] }
  | STRING { case "baseType" (at $sloc) [ kw $loc($1) "STRING" ]}
  | INT
    { case "baseType" (at $sloc) [ kw $loc($1) "INT" ] }
  | BIT l_angle v = int r_angle
    { case "baseType" (at $sloc) [ kw $loc($1) "BIT"; langle $loc($2); arg v; rangle $loc($4) ] }
  | INT l_angle v = int r_angle
    { case "baseType" (at $sloc) [ kw $loc($1) "INT"; langle $loc($2); arg v; rangle $loc($4) ] }
  | VARBIT l_angle v = int r_angle
    { case "baseType" (at $sloc) [ kw $loc($1) "VARBIT"; langle $loc($2); arg v; rangle $loc($4) ] }
  | BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { case "baseType" (at $sloc) [ kw $loc($1) "BIT"; langle $loc($2); lparen $loc($3); arg e; rparen $loc($5); rangle $loc($6) ] }
  | INT l_angle L_PAREN e = expression R_PAREN r_angle
    { case "baseType" (at $sloc) [ kw $loc($1) "INT"; langle $loc($2); lparen $loc($3); arg e; rparen $loc($5); rangle $loc($6) ] }
  | VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { case "baseType" (at $sloc) [ kw $loc($1) "VARBIT"; langle $loc($2); lparen $loc($3); arg e; rparen $loc($5); rangle $loc($6) ] }
;

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { case "specializedType" (at_v $sloc [ n ]) [ arg n; langle $loc($2); arg tal; rangle $loc($4) ] }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { case "headerStackType" (at_v $sloc [ t ]) [ arg t; lbrack $loc($2); arg e; rbrack $loc($4) ] }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { case "listType" (at $sloc) [ kw $loc($1) "LIST"; langle $loc($2); arg targ; rangle $loc($4) ] }
;

(* >> Tuple types *)
tupleType:
  | TUPLE l_angle targs = typeArgumentList r_angle
    { case "tupleType" (at $sloc) [ kw $loc($1) "TUPLE"; langle $loc($2); arg targs; rangle $loc($4) ] }
;

(* >> Types *)
typeRef:
  | t = baseType
  | t = namedType
  | t = headerStackType
  | t = listType
  | t = tupleType
    { t }
;

typeOrVoid:
  | t = typeRef { t }
  | VOID { case "typeOrVoid" (at $sloc) [ kw $loc($1) "VOID" ] }
  (* From Petr4: HACK for generic return type *)
  | id = identifier
    { match flatten_case_v id with
      | "identifier", ["_ID"], [ value_text ]  ->
        case "typeIdentifier" (at_v $sloc [ id ]) [ tag "TID"; arg value_text ]
      | _ -> failwith "@typeOrVoid: expected identifier" }
;

(* Type parameters *)
typeParameter:
  | n = name { n }

typeParameterList:
  | tp = typeParameter { tp }
  | tps = typeParameterList COMMA tp = typeParameter
    { case "typeParameterList" (at_v $sloc [ tps ]) [ arg tps; op $loc($2) ","; arg tp ] }
;

typeParameterListOpt:
  | (* empty *) { case "typeParameterListOpt" (at $sloc) [ tag "EMPTY" ] }
  | l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      case "typeParameterListOpt" (at $sloc) [ langle $loc($1); arg tps; rangle $loc($3) ] }
;

(* Parameters *)
parameter:
  | al = annotationList dir = direction t = typeRef n = name i = initializerOpt
    { declare_var_of_il n false;
      case "parameter" (at_v $sloc [ al; dir; t; n; i ]) [ arg al; arg dir; arg t; arg n; arg i ] }
;

nonEmptyParameterList:
  | p = parameter { p }
  | ps = nonEmptyParameterList COMMA p = parameter
    { case "nonEmptyParameterList" (at_v $sloc [ ps ]) [ arg ps; op $loc($2) ","; arg p ] }
;

parameterList:
  | (* empty *) { case "parameterList" (at $sloc) [ tag "EMPTY" ] }
  | ps = nonEmptyParameterList { ps }
;

(* Constructor parameters *)
constructorParameterListOpt:
  | (* empty *) { case "constructorParameterListOpt" (at $sloc) [ tag "EMPTY" ] }
  | L_PAREN ps = parameterList R_PAREN
    { case "constructorParameterListOpt" (at $sloc) [ lparen $loc($1); arg ps; rparen $loc($3) ] }
;

(* Expression key-value pairs *)
namedExpression:
  | n = name ASSIGN e = expression { case "namedExpression" (at_v $sloc [ n ]) [ arg n; op $loc($2) "="; arg e ] }
;

namedExpressionList:
  | e = namedExpression { e }
  | es = namedExpressionList COMMA e = namedExpression { case "namedExpressionList" (at_v $sloc [ es ]) [ arg es; op $loc($2) ","; arg e ] }
;

(* Expressions *)
(* >> Literal expressions *)
%inline literalExpression:
  | TRUE { case "literalExpression" (at $sloc) [ kw $loc($1) "TRUE" ] }
  | FALSE { case "literalExpression" (at $sloc) [ kw $loc($1) "FALSE" ] }
  | num = number { num }
  | str = stringLiteral { str }
;

(* >> Reference expressions *)
%inline referenceExpression:
  | n = prefixedNonTypeName { n }
  | THIS { case "referenceExpression" (at $sloc) [ kw $loc($1) "THIS" ] }
;

(* >> Default expressions *)
%inline defaultExpression:
  | DOTS { case "defaultExpression" (at $sloc) [ op $loc($1) "..." ] }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop:
  | NOT { case "unop" (at $sloc) [ op $loc($1) "!" ] }
  | COMPLEMENT { case "unop" (at $sloc) [ op $loc($1) "~" ] }
  | MINUS { case "unop" (at $sloc) [ op $loc($1) "-" ] }
  | PLUS { case "unop" (at $sloc) [ op $loc($1) "+" ] }
;

%inline unaryExpression:
  | o = unop e = expression %prec PREFIX
    { case "unaryExpression" (at_v $sloc [ o; e ]) [ arg o; arg e ] }
;

%inline binop:
  | MUL { case "binop" (at $sloc) [ op $loc($1) "*" ] }
  | DIV { case "binop" (at $sloc) [ op $loc($1) "/" ] }
  | MOD { case "binop" (at $sloc) [ op $loc($1) "%" ] }
  | PLUS { case "binop" (at $sloc) [ op $loc($1) "+" ] }
  | PLUS_SAT { case "binop" (at $sloc) [ op $loc($1) "|+|" ] }
  | MINUS { case "binop" (at $sloc) [ op $loc($1) "-" ] }
  | MINUS_SAT { case "binop" (at $sloc) [ op $loc($1) "|-|" ] }
  | SHL { case "binop" (at $sloc) [ op $loc($1) "<<" ] }
  | r_angle R_ANGLE_SHIFT { case "binop" (at $sloc) [ op $sloc ">>" ] }
  | LE { case "binop" (at $sloc) [ op $loc($1) "<=" ] }
  | GE { case "binop" (at $sloc) [ op $loc($1) ">=" ] }
  | l_angle { case "binop" (at $sloc) [ op $loc($1) "<" ] }
  | r_angle { case "binop" (at $sloc) [ op $loc($1) ">" ] }
  | NE { case "binop" (at $sloc) [ op $loc($1) "!=" ] }
  | EQ { case "binop" (at $sloc) [ op $loc($1) "==" ] }
  | BIT_AND { case "binop" (at $sloc) [ op $loc($1) "&" ] }
  | BIT_XOR { case "binop" (at $sloc) [ op $loc($1) "^" ] }
  | BIT_OR { case "binop" (at $sloc) [ op $loc($1) "|" ] }
  | PLUSPLUS { case "binop" (at $sloc) [ op $loc($1) "++" ] }
  | AND { case "binop" (at $sloc) [ op $loc($1) "&&" ] }
  | OR { case "binop" (at $sloc) [ op $loc($1) "||" ] }
;

%inline binaryExpression:
  | l = expression o = binop r = expression
    { case "binaryExpression" (at_v $sloc [ l; o; r ]) [ arg l; arg o; arg r ] }
;

%inline binaryExpressionNonBrace:
  | l = expressionNonBrace o = binop r = expression
    { case "binaryExpressionNonBrace" (at_v $sloc [ l; o; r ]) [ arg l; arg o; arg r ] }
;

%inline ternaryExpression:
  | c = expression QUESTION t = expression COLON f = expression
    { case "ternaryExpression" (at_v $sloc [ c ]) [ arg c; op $loc($2) "?"; arg t; op $loc($4) ":"; arg f ] }
;

%inline ternaryExpressionNonBrace:
  | c = expressionNonBrace QUESTION t = expression COLON f = expression
    { case "ternaryExpressionNonBrace" (at_v $sloc [ c ]) [ arg c; op $loc($2) "?"; arg t; op $loc($4) ":"; arg f ] }
;

(* >> Cast expressions *)
%inline castExpression:
  | L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { case "castExpression" (at $sloc) [ lparen $loc($1); arg t; rparen $loc($3); arg e ] }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
  | INVALID { case "dataExpression" (at $sloc) [ op $loc($1) "{#}" ] }
  | L_BRACE e = dataElementExpression c = trailingCommaOpt R_BRACE
    { case "dataExpression" (at $sloc) [ lbrace $loc($1); arg e; arg c; rbrace $loc($4) ] }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
  | ERROR DOT m = member
    { case "errorAccessExpression" (at $sloc) [ kw $loc($1) "ERROR"; op $loc($2) "."; arg m ] }
;

%inline memberAccessExpression:
  | e = memberAccessBase DOT m = member %prec DOT
    { case "memberAccessExpression" (at_v $sloc [ e ]) [ arg e; op $loc($2) "."; arg m ] }
;

%inline indexAccessExpression:
  | a = expression L_BRACKET i = expression R_BRACKET
    { case "indexAccessExpression" (at_v $sloc [ a ]) [ arg a; lbrack $loc($2); arg i; rbrack $loc($4) ] }
  | a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
    { case "indexAccessExpression" (at_v $sloc [ a ]) [ arg a; lbrack $loc($2); arg h; op $loc($4) ":"; arg l; rbrack $loc($6) ] }
;

%inline accessExpression:
  | e = errorAccessExpression
  | e = memberAccessExpression
  | e = indexAccessExpression
    { e }
;

%inline memberAccessExpressionNonBrace:
  | e = memberAccessBaseNonBrace DOT m = member %prec DOT
    { case "memberAccessExpressionNonBrace" (at_v $sloc [ e ]) [ arg e; op $loc($2) "."; arg m ] }
;

%inline indexAccessExpressionNonBrace:
  | a = expressionNonBrace L_BRACKET i = expression R_BRACKET
    { case "indexAccessExpressionNonBrace" (at_v $sloc [ a ]) [ arg a; lbrack $loc($2); arg i; rbrack $loc($4) ] }
  | a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
    { case "indexAccessExpressionNonBrace" (at_v $sloc [ a ]) [ arg a; lbrack $loc($2); arg h; op $loc($4) ":"; arg l; rbrack $loc($6) ] }
;

%inline accessExpressionNonBrace:
  | e = errorAccessExpression
  | e = memberAccessExpressionNonBrace
  | e = indexAccessExpressionNonBrace
    { e }
;

(* >> Call expressions *)
%inline routineTarget:
  | e = expression { e }
;

%inline constructorTarget:
  | n = namedType { n }
;

%inline callTarget:
  | t = routineTarget
  | t = constructorTarget
    { t }
;

%inline callExpression:
  | t = callTarget L_PAREN args = argumentList R_PAREN
    { case "callExpression" (at_v $sloc [ t ]) [ arg t; lparen $loc($2); arg args; rparen $loc($4) ] }
  | t = routineTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
    { case "callExpression" (at_v $sloc [ t ]) [ arg t; langle $loc($2); arg targs; rangle $loc($4); lparen $loc($5); arg args; rparen $loc($7) ] }
;

%inline routineTargetNonBrace:
  | e = expressionNonBrace { e }
;

%inline callTargetNonBrace:
  | t = routineTargetNonBrace
  | t = constructorTarget
    { t }
;

%inline callExpressionNonBrace:
  | t = callTargetNonBrace L_PAREN args = argumentList R_PAREN
    { case "callExpressionNonBrace" (at_v $sloc [ t ]) [ arg t; lparen $loc($2); arg args; rparen $loc($4) ] }
  | t = routineTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
    { case "callExpressionNonBrace" (at_v $sloc [ t ]) [ arg t; langle $loc($2); arg targs; rangle $loc($4); lparen $loc($5); arg args; rparen $loc($7) ] }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
  | L_PAREN e = expression R_PAREN
    { case "parenthesizedExpression" (at $sloc) [ lparen $loc($1); arg e; rparen $loc($3) ] }
;

(* >> Expressions *)
expression:
  | e = literalExpression
  | e = referenceExpression
  | e = defaultExpression
  | e = unaryExpression
  | e = binaryExpression
  | e = ternaryExpression
  | e = castExpression
  | e = dataExpression
  | e = accessExpression
  | e = callExpression
  | e = parenthesizedExpression
    { e }
;

expressionList:
  | (* empty *) { case "expressionList" (at $sloc) [ tag "EMPTY" ] }
  | e = expression { e }
  | el = expressionList COMMA e = expression
    { case "expressionList" (at_v $sloc [ el ]) [ arg el; op $loc($2) ","; arg e ] }
;

%inline memberAccessBase:
  | e = prefixedTypeName
  | e = expression
    { e }
;

%inline sequenceElementExpression:
  | el = expressionList { el }
;

%inline recordElementExpression:
  | n = name ASSIGN e = expression
    { case "recordElementExpression" (at_v $sloc [ n ]) [ arg n; op $loc($2) "="; arg e ] }
  | n = name ASSIGN e = expression COMMA DOTS
    { case "recordElementExpression" (at_v $sloc [ n ]) [ arg n; op $loc($2) "="; arg e; op $loc($4) ","; op $loc($5) "..." ] }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList
    { case "recordElementExpression" (at_v $sloc [ n ]) [ arg n; op $loc($2) "="; arg e; op $loc($4) ","; arg el ] }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { case "recordElementExpression" (at_v $sloc [ n ]) [ arg n; op $loc($2) "="; arg e; op $loc($4) ","; arg el; op $loc($6) ","; op $loc($7) "..." ] }
;

%inline dataElementExpression:
  | e = sequenceElementExpression
  | e = recordElementExpression
    { e }
;

(* >> Non-brace Expressions *)
expressionNonBrace:
  | e = literalExpression
  | e = referenceExpression
  | e = unaryExpression
  | e = binaryExpressionNonBrace
  | e = ternaryExpressionNonBrace
  | e = castExpression
  | e = accessExpressionNonBrace
  | e = callExpressionNonBrace
  | e = parenthesizedExpression
    { e }
;

%inline memberAccessBaseNonBrace:
  | e = prefixedTypeName
  | e = expressionNonBrace
    { e }
;

(* Keyset Expressions *)
simpleKeysetExpression:
  | e = expression { e }
  | b = expression MASK m = expression
    { case "simpleKeysetExpression" (at_v $sloc [ b ]) [ arg b; op $loc($2) "&&&"; arg m ] }
  | l = expression RANGE h = expression
    { case "simpleKeysetExpression" (at_v $sloc [ l ]) [ arg l; op $loc($2) ".."; arg h ] }
  | DEFAULT
    { case "simpleKeysetExpression" (at $sloc) [ kw $loc($1) "DEFAULT" ] }
  | DONTCARE
    { case "simpleKeysetExpression" (at $sloc) [ op $loc($1) "_" ] }
;

simpleKeysetExpressionList:
  | e = simpleKeysetExpression { e }
  | el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { case "simpleKeysetExpressionList" (at_v $sloc [ el ]) [ arg el; op $loc($2) ","; arg e ] }
;

tupleKeysetExpression:
  | L_PAREN b = expression MASK m = expression R_PAREN
    { case "tupleKeysetExpression" (at $sloc) [ lparen $loc($1); arg b; op $loc($3) "&&&"; arg m; rparen $loc($5) ] }
  | L_PAREN l = expression RANGE h = expression R_PAREN
    { case "tupleKeysetExpression" (at $sloc) [ lparen $loc($1); arg l; op $loc($3) ".."; arg h; rparen $loc($5) ] }
  | L_PAREN DEFAULT R_PAREN
    { case "tupleKeysetExpression" (at $sloc) [ lparen $loc($1); kw $loc($2) "DEFAULT"; rparen $loc($3) ] }
  | L_PAREN DONTCARE R_PAREN
    { case "tupleKeysetExpression" (at $sloc) [ lparen $loc($1); op $loc($2) "_"; rparen $loc($3) ] }
  | L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
    { case "tupleKeysetExpression" (at $sloc) [ lparen $loc($1); arg e; op $loc($3) ","; arg es; rparen $loc($5) ] }
;

keysetExpression:
  | e = simpleKeysetExpression
  | e = tupleKeysetExpression
    { e }
;

(* Type arguments *)
realTypeArgument:
  | t = typeRef { t }
  | VOID
    { case "realTypeArgument" (at $sloc) [ kw $loc($1) "VOID" ] }
  | DONTCARE
    { case "realTypeArgument" (at $sloc) [ op $loc($1) "_" ] }
;

realTypeArgumentList:
  | targ = realTypeArgument { targ }
  | targs = realTypeArgumentList COMMA targ = realTypeArgument
    { case "realTypeArgumentList" (at_v $sloc [ targs ]) [ arg targs; op $loc($2) ","; arg targ ] }
;

typeArgument:
  | t = typeRef
  | t = nonTypeName
    { t }
  | VOID
    { case "typeArgument" (at $sloc) [ kw $loc($1) "VOID" ] }
  | DONTCARE
    { case "typeArgument" (at $sloc) [ op $loc($1) "_" ] }
;

typeArgumentList:
  | (* empty *) { case "typeArgumentList" (at $sloc) [ tag "EMPTY" ] }
  | targ = typeArgument { targ }
  | targs = typeArgumentList COMMA targ = typeArgument
    { case "typeArgumentList" (at_v $sloc [ targs ]) [ arg targs; op $loc($2) ","; arg targ ] }
;

(* Arguments *)
argument:
  | e = expression { e }
  | n = name ASSIGN e = expression
    { case "argument" (at_v $sloc [ n ]) [ arg n; op $loc($2) "="; arg e ] }
  | name = name ASSIGN DONTCARE
    { case "argument" (at_v $sloc [ name ]) [ arg name; op $loc($2) "="; op $loc($3) "_" ] }
  | DONTCARE
    { case "argument" (at $sloc) [ op $loc($1) "_" ] }
;

argumentListNonEmpty:
  | a = argument { a }
  | args = argumentListNonEmpty COMMA a = argument
    { case "argumentListNonEmpty" (at_v $sloc [ args ]) [ arg args; op $loc($2) ","; arg a ] }
;

argumentList:
  | (* empty *) { case "argumentList" (at $sloc) [ tag "EMPTY" ] }
  | args = argumentListNonEmpty { args }
;

(* L-values *)
lvalue:
  | e = referenceExpression { e }
  | lv = lvalue DOT m = member %prec DOT
    { case "lvalue" (at_v $sloc [ lv ]) [ arg lv; op $loc($2) "."; arg m ] }
  | lv = lvalue L_BRACKET i = expression R_BRACKET
    { case "lvalue" (at_v $sloc [ lv ]) [ arg lv; lbrack $loc($2); arg i; rbrack $loc($4) ] }
  | lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
    { case "lvalue" (at_v $sloc [ lv ]) [ arg lv; lbrack $loc($2); arg h; op $loc($4) ":"; arg l; rbrack $loc($6) ] }
  | L_PAREN lv = lvalue R_PAREN
    { case "lvalue" (at $sloc) [ lparen $loc($1); arg lv; rparen $loc($3) ] }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
  | SEMICOLON { case "emptyStatement" (at $sloc) [ op $loc($1) ";" ] }
;

(* >> Assignment statements *)
assignop:
  | ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "=" ] }
  | PLUS_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "+=" ] }
  | PLUS_SAT_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "|+|=" ] }
  | MINUS_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "-=" ] }
  | MINUS_SAT_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "|-|=" ] }
  | MUL_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "*=" ] }
  | DIV_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "/=" ] }
  | MOD_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "%=" ] }
  | SHL_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "<<=" ] }
  | SHR_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) ">>=" ] }
  | BIT_AND_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "&=" ] }
  | BIT_XOR_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "^=" ] }
  | BIT_OR_ASSIGN { case "assignop" (at $sloc) [ op $loc($1) "|=" ] }
;

assignmentStatement:
  | lv = lvalue o = assignop e = expression SEMICOLON
    { case "assignmentStatement" (at_v $sloc [ lv; o; e ]) [ arg lv; arg o; arg e; op $loc($4) ";" ] }
;

(* >> Call statements *)
callStatement:
  | lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
    { case "callStatement" (at_v $sloc [ lv ]) [ arg lv; lparen $loc($2); arg args; rparen $loc($4); op $loc($5) ";" ] }
  | lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
    { case "callStatement" (at_v $sloc [ lv ]) [ arg lv; langle $loc($2); arg targs; rangle $loc($4); lparen $loc($5); arg args; rparen $loc($7); op $loc($8) ";" ] }
;

(* >> Direct application statements *)
directApplicationStatement:
  | t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { case "directApplicationStatement" (at_v $sloc [ t ]) [ arg t; op $loc($2) "."; kw $loc($3) "APPLY"; lparen $loc($4); arg args; rparen $loc($6); op $loc($7) ";" ] }
;

(* >> Return statements *)
returnStatement:
  | RETURN SEMICOLON
    { case "returnStatement" (at $sloc) [ kw $loc($1) "RETURN"; op $loc($2) ";" ] }
  | RETURN e = expression SEMICOLON
    { case "returnStatement" (at $sloc) [ kw $loc($1) "RETURN"; arg e; op $loc($3) ";" ] }
;

(* >> Exit statements *)
exitStatement:
  | EXIT SEMICOLON
    { case "exitStatement" (at $sloc) [ kw $loc($1) "EXIT"; op $loc($2) ";" ] }
;

(* >> Block statements *)
blockStatement:
  | al = annotationList L_BRACE
  push_scope
  sl = blockElementStatementList R_BRACE
  pop_scope
    { case "blockStatement" (at_v $sloc [ al ]) [ arg al; lbrace $loc($2); arg sl; rbrace $loc($5) ] }
;

(* >> Conditional statements *)
conditionalStatement:
  | IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { case "conditionalStatement" (at $sloc) [ kw $loc($1) "IF"; lparen $loc($2); arg c; rparen $loc($4); arg t ] }
  | IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { case "conditionalStatement" (at $sloc) [ kw $loc($1) "IF"; lparen $loc($2); arg c; rparen $loc($4); arg t; kw $loc($6) "ELSE"; arg f ] }
;

(* >> For statements *)
forInitStatement:
  | al = annotationList t = typeRef n = name i = initializerOpt
    { case "forInitStatement" (at_v $sloc [ al; t; n; i ]) [ arg al; arg t; arg n; arg i ] }
  | lv = lvalue L_PAREN args = argumentList R_PAREN
    { case "forInitStatement" (at_v $sloc [ lv ]) [ arg lv; lparen $loc($2); arg args; rparen $loc($4) ] }
  | lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
    { case "forInitStatement" (at_v $sloc [ lv ]) [ arg lv; langle $loc($2); arg targs; rangle $loc($4); lparen $loc($5); arg args; rparen $loc($7) ] }
  | lv = lvalue o = assignop e = expression
    { case "forInitStatement" (at_v $sloc [ lv; o; e ]) [ arg lv; arg o; arg e ] }
;

forInitStatementListNonEmpty:
  | s = forInitStatement { s }
  | sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { case "forInitStatementListNonEmpty" (at_v $sloc [ sl ]) [ arg sl; op $loc($2) ","; arg s ] }
;

forInitStatementList:
  | (* empty *) { case "forInitStatementList" (at $sloc) [ tag "EMPTY" ] }
  | sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
  | s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
  | s = forUpdateStatement { s }
  | sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { case "forUpdateStatementListNonEmpty" (at_v $sloc [ sl ]) [ arg sl; op $loc($2) ","; arg s ] }
;

forUpdateStatementList:
  | (* empty *) { case "forUpdateStatementList" (at $sloc) [ tag "EMPTY" ] }
  | sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
  | e = expression { e }
  | l = expression RANGE h = expression
    { case "forCollectionExpr" (at_v $sloc [ l ]) [ arg l; op $loc($2) ".."; arg h ] }
;

forStatement:
  | al = annotationList FOR L_PAREN il = forInitStatementList SEMICOLON c = expression SEMICOLON ul = forUpdateStatementList R_PAREN b = statement
    { case "forStatement" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "FOR"; lparen $loc($3); arg il; op $loc($5) ";"; arg c; op $loc($7) ";"; arg ul; rparen $loc($9); arg b ] }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { case "forStatement" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "FOR"; lparen $loc($3); arg t; arg n; kw $loc($6) "IN"; arg e; rparen $loc($8); arg b ] }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { case "forStatement" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "FOR"; lparen $loc($3); arg al_in; arg t; arg n; kw $loc($7) "IN"; arg e; rparen $loc($9); arg b ] }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { case "switchLabel" (at $sloc) [ kw $loc($1) "DEFAULT" ] }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { case "switchCase" (at_v $sloc [ l ]) [ arg l; op $loc($2) ":"; arg s ] }
  | l = switchLabel COLON
    { case "switchCase" (at_v $sloc [ l ]) [ arg l; op $loc($2) ":" ] }
;

switchCaseList:
  | (* empty *)
    { case "switchCaseList" (at $sloc) [ tag "EMPTY" ] }
  | cs = switchCaseList c = switchCase
    { case "switchCaseList" (at_v $sloc [ cs; c ]) [ arg cs; arg c ] }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { case "switchStatement" (at $sloc) [ kw $loc($1) "SWITCH"; lparen $loc($2); arg e; rparen $loc($4); lbrace $loc($5); arg cs; rbrace $loc($7) ] }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { case "breakStatement" (at $sloc) [ kw $loc($1) "BREAK"; op $loc($2) ";" ] }
;

continueStatement:
  | CONTINUE SEMICOLON
    { case "continueStatement" (at $sloc) [ kw $loc($1) "CONTINUE"; op $loc($2) ";" ] }
;

(* >> Statements *)
statement:
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = directApplicationStatement
  | s = returnStatement
  | s = exitStatement
  | s = blockStatement
  | s = conditionalStatement
  | s = forStatement
  | s = breakStatement
  | s = continueStatement
  | s = switchStatement
    { s }
;

(* Declarations *)
(* >> Constant and variable declarations *)

(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
  | ASSIGN e = expression
    { case "initializer" (at $sloc) [ op $loc($1) "="; arg e ] }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { case "constantDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "CONST"; arg t; arg n; arg i; op $loc($6) ";" ] }
;

initializerOpt:
  | (* empty *)
    { case "initializerOpt" (at $sloc) [ tag "EMPTY" ] }
  | i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      case "variableDeclaration" (at_v $sloc [ al; t; n; i ]) [ arg al; arg t; arg n; arg i; op $loc($5) ";" ] }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { case "blockElementStatementList" (at $sloc) [ tag "EMPTY" ] }
  | sl = blockElementStatementList s = blockElementStatement
    { case "blockElementStatementList" (at_v $sloc [ sl; s ]) [ arg sl; arg s ] }
;

(* >> Function declarations *)
functionPrototype:
  | t = typeOrVoid n = name push_scope
  tpl = typeParameterListOpt
  L_PAREN pl = parameterList R_PAREN
    { case "functionPrototype" (at_v $sloc [ t; n; tpl ]) [ arg t; arg n; arg tpl; lparen $loc($5); arg pl; rparen $loc($7) ] }
;

functionDeclaration:
  | al = annotationList p = functionPrototype b = blockStatement pop_scope
    { case "functionDeclaration" (at_v $sloc [ al; p; b ]) [ arg al; arg p; arg b ] }
;

(* >> Action declarations *)
actionDeclaration:
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { case "actionDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "ACTION"; arg n; lparen $loc($4); arg pl; rparen $loc($6); arg s ] }
;

(* >> Instantiations *)
objectInitializer:
  | ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { case "objectInitializer" (at $sloc) [ op $loc($1) "="; lbrace $loc($2); arg ds; rbrace $loc($4) ] }
;

instantiation:
  | al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { case "instantiation" (at_v $sloc [ al; t ]) [ arg al; arg t; lparen $loc($3); arg args; rparen $loc($5); arg n; op $loc($7) ";" ] }
  | al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name i = objectInitializer SEMICOLON
    { case "instantiation" (at_v $sloc [ al; t ]) [ arg al; arg t; lparen $loc($3); arg args; rparen $loc($5); arg n; arg i; op $loc($8) ";" ] }
;

objectDeclaration:
  | d = functionDeclaration
  | d = instantiation
    { d }
;

objectDeclarationList:
  | (* empty *) { case "objectDeclarationList" (at $sloc) [ tag "EMPTY" ] }
  | ds = objectDeclarationList d = objectDeclaration
    { case "objectDeclarationList" (at_v $sloc [ ds; d ]) [ arg ds; arg d ] }
;

(* >> Error declarations *)
errorDeclaration:
  | ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      case "errorDeclaration" (at $sloc) [ kw $loc($1) "ERROR"; lbrace $loc($2); arg nl; rbrace $loc($4) ] }
;

(* >> Match kind declarations *)
matchKindDeclaration:
  | MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      case "matchKindDeclaration" (at $sloc) [ kw $loc($1) "MATCH_KIND"; lbrace $loc($2); arg nl; arg c; rbrace $loc($5) ] }
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { case "enumTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "ENUM"; arg n; lbrace $loc($4); arg nl; arg c; rbrace $loc($7) ] }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { case "enumTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "ENUM"; arg t; arg n; lbrace $loc($5); arg el; arg c; rbrace $loc($8) ] }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { case "typeField" (at_v $sloc [ al; t; n ]) [ arg al; arg t; arg n; op $loc($4) ";" ] }
;

typeFieldList:
  | (* empty *) { case "typeFieldList" (at $sloc) [ tag "EMPTY" ] }
  | fl = typeFieldList f = typeField
    { case "typeFieldList" (at_v $sloc [ fl; f ]) [ arg fl; arg f ] }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { case "structTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "STRUCT"; arg n; arg tpl; lbrace $loc($5); arg fl; rbrace $loc($7) ] }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { case "headerTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "HEADER"; arg n; arg tpl; lbrace $loc($5); arg fl; rbrace $loc($7) ] }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { case "headerUnionTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "HEADER_UNION"; arg n; arg tpl; lbrace $loc($5); arg fl; rbrace $loc($7) ] }
;

derivedTypeDeclaration:
  | d = enumTypeDeclaration
  | d = structTypeDeclaration
  | d = headerTypeDeclaration
  | d = headerUnionTypeDeclaration
    { d }
;

(* >> Typedef and newtype declarations *)
typedefType:
  | t = typeRef
  | t = derivedTypeDeclaration
    { t }
;

typedefDeclaration:
  | al = annotationList TYPEDEF t = typedefType n = name SEMICOLON
    { case "typedefDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "TYPEDEF"; arg t; arg n; op $loc($5) ";" ] }
  | al = annotationList TYPE t = typeRef n = name SEMICOLON
    { case "typedefDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "TYPE"; arg t; arg n; op $loc($5) ";" ] }
;

(* >> Extern declarations *)
externFunctionDeclaration:
  | al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
    { let decl =
        case "externFunctionDeclaration" (at_v $sloc [ al ])
          [ arg al; kw $loc($2) "EXTERN"; arg p; op $loc($5) ";" ]
      in
      declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      decl }
;

methodPrototype:
  | al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { case "methodPrototype" (at_v $sloc [ al; tid ]) [ arg al; arg tid; lparen $loc($3); arg pl; rparen $loc($5); op $loc($6) ";" ] }
  | al = annotationList p = functionPrototype pop_scope SEMICOLON
    { case "methodPrototype" (at_v $sloc [ al; p ]) [ arg al; arg p; op $loc($4) ";" ] }
  | al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { case "methodPrototype" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "ABSTRACT"; arg p; op $loc($5) ";" ] }
;

methodPrototypeList:
  | (* empty *) { case "methodPrototypeList" (at $sloc) [ tag "EMPTY" ] }
  | ps = methodPrototypeList p = methodPrototype
    { case "methodPrototypeList" (at_v $sloc [ ps; p ]) [ arg ps; arg p ] }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = methodPrototypeList R_BRACE pop_scope
    { let decl =
        case "externObjectDeclaration" (at_v $sloc [ al ])
          [ arg al; kw $loc($2) "EXTERN"; arg n; arg tpl; lbrace $loc($5); arg pl; rbrace $loc($7) ]
      in
      declare_type_of_il n (has_type_params_declaration decl);
      decl }
;

externDeclaration:
  | d = externFunctionDeclaration
  | d = externObjectDeclaration
    { d }
;

(* >> Parser statements and declarations *)
(* >>>> Select expressions *)
selectCase:
  | k = keysetExpression COLON n = name SEMICOLON
    { case "selectCase" (at_v $sloc [ k ]) [ arg k; op $loc($2) ":"; arg n; op $loc($4) ";" ] }
;

selectCaseList:
  | (* empty *) { case "selectCaseList" (at $sloc) [ tag "EMPTY" ] }
  | cl = selectCaseList c = selectCase
    { case "selectCaseList" (at_v $sloc [ cl; c ]) [ arg cl; arg c ] }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { case "selectExpression" (at $sloc) [ kw $loc($1) "SELECT"; lparen $loc($2); arg el; rparen $loc($4); lbrace $loc($5); arg cl; rbrace $loc($7) ] }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { case "stateExpression" (at_v $sloc [ n ]) [ arg n; op $loc($2) ";" ] }
  | e = selectExpression
    { e }
;

transitionStatement:
  | (* empty *) { case "transitionStatement" (at $sloc) [ tag "EMPTY" ] }
  | TRANSITION e = stateExpression
    { case "transitionStatement" (at $sloc) [ kw $loc($1) "TRANSITION"; arg e ] }
;

(* >>>> Value set declarations *)
valueSetType:
  | t = baseType
  | t = tupleType
  | t = prefixedTypeName
    { t }
;

valueSetDeclaration:
  | al = annotationList VALUE_SET l_angle t = valueSetType r_angle
    L_PAREN s = expression R_PAREN n = name SEMICOLON
    { case "valueSetDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "VALUE_SET"; langle $loc($3); arg t; rangle $loc($5); lparen $loc($6); arg s; rparen $loc($8); arg n; op $loc($10) ";" ] }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { case "parserTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "PARSER"; arg n; arg tpl; lparen $loc($5); arg pl; rparen $loc($7); op $loc($9) ";" ] }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { case "parserBlockStatement" (at_v $sloc [ al ]) [ arg al; lbrace $loc($2); arg sl; rbrace $loc($4) ] }
;

parserStatement:
  | s = constantDeclaration
  | s = variableDeclaration
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = directApplicationStatement
  | s = parserBlockStatement
  | s = conditionalStatement
    { s }
;

parserStatementList:
  | (* empty *) { case "parserStatementList" (at $sloc) [ tag "EMPTY" ] }
  | sl = parserStatementList s = parserStatement
    { case "parserStatementList" (at_v $sloc [ sl; s ]) [ arg sl; arg s ] }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { case "parserState" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "STATE"; arg n; lbrace $loc($4); arg sl; arg t; rbrace $loc($7) ] }
;

parserStateList:
  | s = parserState { s }
  | sl = parserStateList s = parserState
    { case "parserStateList" (at_v $sloc [ sl; s ]) [ arg sl; arg s ] }
;

parserLocalDeclaration:
  | d = constantDeclaration
  | d = instantiation
  | d = variableDeclaration
  | d = valueSetDeclaration
    { d }
;

parserLocalDeclarationList:
  | (* empty *) { case "parserLocalDeclarationList" (at $sloc) [ tag "EMPTY" ] }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { case "parserLocalDeclarationList" (at_v $sloc [ dl; d ]) [ arg dl; arg d ] }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
    { case "parserDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "PARSER"; arg n; arg tpl; lparen $loc($5); arg pl; rparen $loc($7); arg cpl;
      lbrace $loc($9); arg dl; arg sl; rbrace $loc($12) ] }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *) { case "constOpt" (at $sloc) [ tag "EMPTY" ] }
  | CONST { case "constOpt" (at $sloc) [ kw $loc($1) "CONST" ] }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { case "tableKey" (at_v $sloc [ e ]) [ arg e; op $loc($2) ":"; arg n; arg al; op $loc($5) ";" ] }
;

tableKeyList:
  | (* empty *) { case "tableKeyList" (at $sloc) [ tag "EMPTY" ] }
  | kl = tableKeyList k = tableKey
    { case "tableKeyList" (at_v $sloc [ kl; k ]) [ arg kl; arg k ] }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { case "tableActionReference" (at_v $sloc [ n ]) [ arg n; lparen $loc($2); arg al; rparen $loc($4) ] }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { case "tableAction" (at_v $sloc [ al; ac ]) [ arg al; arg ac; op $loc($3) ";" ] }
;

tableActionList:
  | (* empty *) { case "tableActionList" (at $sloc) [ tag "EMPTY" ] }
  | acl = tableActionList ac = tableAction
    { case "tableActionList" (at_v $sloc [ acl; ac ]) [ arg acl; arg ac ] }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN num = number COLON
    { case "tableEntryPriority" (at $sloc) [ kw $loc($1) "PRIORITY"; op $loc($2) "="; arg num; op $loc($4) ":" ] }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { case "tableEntryPriority" (at $sloc) [ kw $loc($1) "PRIORITY"; op $loc($2) "="; lparen $loc($3); arg e; rparen $loc($5); op $loc($6) ":" ] }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { case "tableEntry" (at_v $sloc [ c; p; k ]) [ arg c; arg p; arg k; op $loc($4) ":"; arg ac; arg al; op $loc($7) ";" ] }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { case "tableEntry" (at_v $sloc [ c; k ]) [ arg c; arg k; op $loc($3) ":"; arg ac; arg al; op $loc($6) ";" ] }
;

tableEntryList:
  | (* empty *) { case "tableEntryList" (at $sloc) [ tag "EMPTY" ] }
  | el = tableEntryList e = tableEntry
    { case "tableEntryList" (at_v $sloc [ el; e ]) [ arg el; arg e ] }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { case "tableProperty" (at $sloc) [ kw $loc($1) "KEY"; op $loc($2) "="; lbrace $loc($3); arg kl; rbrace $loc($5) ] }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { case "tableProperty" (at $sloc) [ kw $loc($1) "ACTIONS"; op $loc($2) "="; lbrace $loc($3); arg acl; rbrace $loc($5) ] }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { case "tableProperty" (at_v $sloc [ al; c ]) [ arg al; arg c; kw $loc($3) "ENTRIES"; op $loc($4) "="; lbrace $loc($5); arg el; rbrace $loc($7) ] }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { case "tableProperty" (at_v $sloc [ al; c; n; i ]) [ arg al; arg c; arg n; arg i; op $loc($5) ";" ] }
;

tablePropertyList:
  | (* empty *) { case "tablePropertyList" (at $sloc) [ tag "EMPTY" ] }
  | pl = tablePropertyList p = tableProperty
    { case "tablePropertyList" (at_v $sloc [ pl; p ]) [ arg pl; arg p ] }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { case "tableDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "TABLE"; arg n; lbrace $loc($4); arg pl; rbrace $loc($6) ] }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { case "controlTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "CONTROL"; arg n; arg tpl; lparen $loc($5); arg pl; rparen $loc($7); op $loc($9) ";" ] }
;

(* >>>> Control declarations *)
controlBody:
  | b = blockStatement { b }
;

controlLocalDeclaration:
  | d = constantDeclaration
  | d = instantiation
  | d = variableDeclaration
    { d }
  | d = actionDeclaration
  | d = tableDeclaration
    { declare_var (id_of_declaration d) false;
      d }
;

controlLocalDeclarationList:
  | (* empty *) { case "controlLocalDeclarationList" (at $sloc) [ tag "EMPTY" ] }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { case "controlLocalDeclarationList" (at_v $sloc [ dl; d ]) [ arg dl; arg d ] }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { case "controlDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "CONTROL"; arg n; arg tpl; lparen $loc($5); arg pl; rparen $loc($7); arg cpl;
      lbrace $loc($9); arg dl; kw $loc($11) "APPLY"; arg b; rbrace $loc($13) ] }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { case "packageTypeDeclaration" (at_v $sloc [ al ]) [ arg al; kw $loc($2) "PACKAGE"; arg n; arg tpl; lparen $loc($5); arg pl; rparen $loc($7); op $loc($9) ";" ] }
;

(* >> Type declarations *)
typeDeclaration:
  | d = derivedTypeDeclaration
  | d = typedefDeclaration
  | d = parserTypeDeclaration
  | d = controlTypeDeclaration
  | d = packageTypeDeclaration
    { d }
;

(* >> Declarations *)
declaration:
  | const = constantDeclaration
    { declare_var (id_of_declaration const) (has_type_params_declaration const);
      const }
  | inst = instantiation
    { declare_var (id_of_declaration inst) false;
      inst }
  | func = functionDeclaration
    { declare_var (id_of_declaration func) (has_type_params_declaration func);
      func }
  | action = actionDeclaration
    { declare_var (id_of_declaration action) false;
      action }
  | d = errorDeclaration
  | d = matchKindDeclaration
  | d = externDeclaration
    { d }
  | d = parserDeclaration
  | d = controlDeclaration
  | d = typeDeclaration
    { declare_type (id_of_declaration d) (has_type_params_declaration d);
      d }
;

(* Annotations *)
annotationToken:
  | UNEXPECTED_TOKEN
    { case "annotationToken" (at $sloc) [ kw $loc($1) "UNEXPECTED_TOKEN" ] }
  | ABSTRACT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "ABSTRACT" ] }
  | ACTION
    { case "annotationToken" (at $sloc) [ kw $loc($1) "ACTION" ] }
  | ACTIONS
    { case "annotationToken" (at $sloc) [ kw $loc($1) "ACTIONS" ] }
  | APPLY
    { case "annotationToken" (at $sloc) [ kw $loc($1) "APPLY" ] }
  | BOOL
    { case "annotationToken" (at $sloc) [ kw $loc($1) "BOOL" ] }
  | BIT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "BIT" ] }
  | BREAK
    { case "annotationToken" (at $sloc) [ kw $loc($1) "BREAK" ] }
  | CONST
    { case "annotationToken" (at $sloc) [ kw $loc($1) "CONST" ] }
  | CONTINUE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "CONTINUE" ] }
  | CONTROL
    { case "annotationToken" (at $sloc) [ kw $loc($1) "CONTROL" ] }
  | DEFAULT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "DEFAULT" ] }
  | ELSE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "ELSE" ] }
  | ENTRIES
    { case "annotationToken" (at $sloc) [ kw $loc($1) "ENTRIES" ] }
  | ENUM
    { case "annotationToken" (at $sloc) [ kw $loc($1) "ENUM" ] }
  | ERROR
    { case "annotationToken" (at $sloc) [ kw $loc($1) "ERROR" ] }
  | EXIT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "EXIT" ] }
  | EXTERN
    { case "annotationToken" (at $sloc) [ kw $loc($1) "EXTERN" ] }
  | FALSE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "FALSE" ] }
  | FOR
    { case "annotationToken" (at $sloc) [ kw $loc($1) "FOR" ] }
  | HEADER
    { case "annotationToken" (at $sloc) [ kw $loc($1) "HEADER" ] }
  | HEADER_UNION
    { case "annotationToken" (at $sloc) [ kw $loc($1) "HEADER_UNION" ] }
  | IF
    { case "annotationToken" (at $sloc) [ kw $loc($1) "IF" ] }
  | IN
    { case "annotationToken" (at $sloc) [ kw $loc($1) "IN" ] }
  | INOUT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "INOUT" ] }
  | INT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "INT" ] }
  | KEY
    { case "annotationToken" (at $sloc) [ kw $loc($1) "KEY" ] }
  | MATCH_KIND
    { case "annotationToken" (at $sloc) [ kw $loc($1) "MATCH_KIND" ] }
  | TYPE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "TYPE" ] }
  | OUT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "OUT" ] }
  | PARSER
    { case "annotationToken" (at $sloc) [ kw $loc($1) "PARSER" ] }
  | PACKAGE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "PACKAGE" ] }
  | PRAGMA
    { case "annotationToken" (at $sloc) [ kw $loc($1) "PRAGMA" ] }
  | RETURN
    { case "annotationToken" (at $sloc) [ kw $loc($1) "RETURN" ] }
  | SELECT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "SELECT" ] }
  | STATE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "STATE" ] }
  | STRING
    { case "annotationToken" (at $sloc) [ kw $loc($1) "STRING" ] }
  | STRUCT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "STRUCT" ] }
  | SWITCH
    { case "annotationToken" (at $sloc) [ kw $loc($1) "SWITCH" ] }
  | TABLE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "TABLE" ] }
  | THIS
    { case "annotationToken" (at $sloc) [ kw $loc($1) "THIS" ] }
  | TRANSITION
    { case "annotationToken" (at $sloc) [ kw $loc($1) "TRANSITION" ] }
  | TRUE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "TRUE" ] }
  | TUPLE
    { case "annotationToken" (at $sloc) [ kw $loc($1) "TUPLE" ] }
  | TYPEDEF
    { case "annotationToken" (at $sloc) [ kw $loc($1) "TYPEDEF" ] }
  | VARBIT
    { case "annotationToken" (at $sloc) [ kw $loc($1) "VARBIT" ] }
  | VALUE_SET
    { case "annotationToken" (at $sloc) [ kw $loc($1) "VALUE_SET" ] }
  | LIST
    { case "annotationToken" (at $sloc) [ kw $loc($1) "LIST" ] }
  | VOID
    { case "annotationToken" (at $sloc) [ kw $loc($1) "VOID" ] }
  | DONTCARE
    { case "annotationToken" (at $sloc) [ op $loc($1) "_" ] }
  | id = identifier
    { id }
  | tid = typeIdentifier
    { tid }
  | str = stringLiteral
    { str }
  | num = number
    { num }
  | MASK
    { case "annotationToken" (at $sloc) [ op $loc($1) "&&&" ] }
  (* TODO: missing DOTS "..." in spec *)
  | RANGE
    { case "annotationToken" (at $sloc) [ op $loc($1) ".." ] }
  | SHL
    { case "annotationToken" (at $sloc) [ op $loc($1) "<<" ] }
  | AND
    { case "annotationToken" (at $sloc) [ op $loc($1) "&&" ] }
  | OR
    { case "annotationToken" (at $sloc) [ op $loc($1) "||" ] }
  | EQ
    { case "annotationToken" (at $sloc) [ op $loc($1) "==" ] }
  | NE
    { case "annotationToken" (at $sloc) [ op $loc($1) "!=" ] }
  | GE
    { case "annotationToken" (at $sloc) [ op $loc($1) ">=" ] }
  | LE
    { case "annotationToken" (at $sloc) [ op $loc($1) "<=" ] }
  | PLUSPLUS
    { case "annotationToken" (at $sloc) [ op $loc($1) "++" ] }
  | PLUS
    { case "annotationToken" (at $sloc) [ op $loc($1) "+" ] }
  | PLUS_SAT
    { case "annotationToken" (at $sloc) [ op $loc($1) "|+|" ] }
  | MINUS
    { case "annotationToken" (at $sloc) [ op $loc($1) "-" ] }
  | MINUS_SAT
    { case "annotationToken" (at $sloc) [ op $loc($1) "|-|" ] }
  | MUL
    { case "annotationToken" (at $sloc) [ op $loc($1) "*" ] }
  | DIV
    { case "annotationToken" (at $sloc) [ op $loc($1) "/" ] }
  | MOD
    { case "annotationToken" (at $sloc) [ op $loc($1) "%" ] }
  | BIT_OR
    { case "annotationToken" (at $sloc) [ op $loc($1) "|" ] }
  | BIT_AND
    { case "annotationToken" (at $sloc) [ op $loc($1) "&" ] }
  | BIT_XOR
    { case "annotationToken" (at $sloc) [ op $loc($1) "^" ] }
  | COMPLEMENT
    { case "annotationToken" (at $sloc) [ op $loc($1) "~" ] }
  | L_BRACKET
    { case "annotationToken" (at $sloc) [ op $loc($1) "[" ] }
  | R_BRACKET
    { case "annotationToken" (at $sloc) [ op $loc($1) "]" ] }
  | L_BRACE
    { case "annotationToken" (at $sloc) [ op $loc($1) "{" ] }
  | R_BRACE
    { case "annotationToken" (at $sloc) [ op $loc($1) "}" ] }
  | L_ANGLE
    { case "annotationToken" (at $sloc) [ op $loc($1) "<" ] }
  | R_ANGLE
    { case "annotationToken" (at $sloc) [ op $loc($1) ">" ] }
  | NOT
    { case "annotationToken" (at $sloc) [ op $loc($1) "!" ] }
  | COLON
    { case "annotationToken" (at $sloc) [ op $loc($1) ":" ] }
  | COMMA
    { case "annotationToken" (at $sloc) [ op $loc($1) "," ] }
  | QUESTION
    { case "annotationToken" (at $sloc) [ op $loc($1) "?" ] }
  | DOT
    { case "annotationToken" (at $sloc) [ op $loc($1) "." ] }
  | ASSIGN
    { case "annotationToken" (at $sloc) [ op $loc($1) "=" ] }
  | SEMICOLON
    { case "annotationToken" (at $sloc) [ op $loc($1) ";" ] }
  | AT
    { case "annotationToken" (at $sloc) [ op $loc($1) "@" ] }
;

annotationBody:
  | (* empty *) { case "annotationBody" (at $sloc) [ tag "EMPTY" ] }
  | ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { case "annotationBody" (at_v $sloc [ ab ]) [ arg ab; lparen $loc($2); arg ab_in; rparen $loc($4) ] }
  | ab = annotationBody tok = annotationToken
    { case "annotationBody" (at_v $sloc [ ab; tok ]) [ arg ab; arg tok ] }
;

structuredAnnotationBody:
  | e = dataElementExpression c = trailingCommaOpt
    { case "structuredAnnotationBody" (at_v $sloc [ e; c ]) [ arg e; arg c ] }
;

annotation:
  | AT name = name
    { case "annotation" (at $sloc) [ op $loc($1) "@"; arg name ] }
  | AT name = name L_PAREN body = annotationBody R_PAREN
    { case "annotation" (at $sloc) [ op $loc($1) "@"; arg name; lparen $loc($3); arg body; rparen $loc($5) ] }
  | AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { case "annotation" (at $sloc) [ op $loc($1) "@"; arg name; lbrack $loc($3); arg body; rbrack $loc($5) ] }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
  | PRAGMA name = name body = annotationBody PRAGMA_END
    { let at_at, at_kw = at_pragma $loc($1) in
      case "annotation" (at ($symbolstartpos, $endpos(body))) [ Case.op ~at:at_at "@"; Case.kw ~at:at_kw "PRAGMA"; arg name; arg body ] }
;

annotationListNonEmpty:
  | a = annotation { a }
  | al = annotationListNonEmpty a = annotation
    { case "annotationListNonEmpty" (at_v $sloc [ al; a ]) [ arg al; arg a ] }
;

%inline annotationList:
  | (* empty *) { case "annotationList" (at $sloc) [ tag "EMPTY" ] }
  | al = annotationListNonEmpty { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *) { case "p4program" (at $sloc) [ tag "EMPTY" ] }
  | ds = declarationList d = declaration
    { case "p4program" (at_v $sloc [ ds; d ]) [ arg ds; arg d ] }
  | ds = declarationList SEMICOLON
    { case "p4program" (at_v $sloc [ ds ]) [ arg ds; op $loc($2) ";" ] }
;

p4program:
  | ds = declarationList END { ds }
