P4 CLI: instruction-coverage full in SL mode against header declarations.

The P4 spec supplies iterated SL instructions for these coverage tests.

  $ cat > two.p4 <<EOF
  > header h_t { bit<8> a; bit<8> b; }
  > EOF
  $ cat > none.p4 <<EOF
  > header h_t { }
  > EOF
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p two.p4 --color never --sl --instruction-coverage.level full > two.txt 2>/dev/null
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p none.p4 --color never --sl --instruction-coverage.level full > none.txt 2>/dev/null

The two fields of the header give two repetitions. Coverage counts an attempt and its result for each repetition.

  $ sed -n '/^relation Decl_ok:/,/^relation/p' two.txt | grep -A7 'Case (% has type headerTypeDeclaration):'
     1/####      Case (% has type headerTypeDeclaration):
     1            (Let (annotationList header name typeParameterListOpt { typeFieldList }...
     1              (TypeParameterListOpt_ok: (block) TC_0 |- typeParameterListOpt : TC_1...
     1                (Let (annotationList_f type_f name_f ;)* be $flatten_typeFieldList(...
     2                  (Type_ok: (block) TC_1 |- (type_f as typeOrVoid) : typeIR_f # typ...
     2/####               If ((typeId'* matches pattern []))*, then
     1                      (Let nameIR be $name(name))
     2                        (Let nameIR_f be $name(name_f))*
  $ sed -n '/^relation Type_wf:/,/^relation/p' two.txt | grep -A5 'Case (% has type headerTypeIR):'
     1/####      Case (% has type headerTypeIR):
     1            (Let (header _typeId < _typeArgumentIR* > { (_annotationList typeIR_fie...
     1/####         If ($distinct_<nameIR>(nameIR_field*)), then
     2/####           If ($nestable_header(typeIR_field))*, then
     2/####             If (Type_wf: B |- typeIR_field holds)*, then
     1                    The relation holds

An empty header gives zero repetitions. Coverage leaves the iterated instruction uncovered even though execution proceeds to the following instructions.

  $ sed -n '/^relation Decl_ok:/,/^relation/p' none.txt | grep -A7 'Case (% has type headerTypeDeclaration):'
     1/####      Case (% has type headerTypeDeclaration):
     1            (Let (annotationList header name typeParameterListOpt { typeFieldList }...
     1              (TypeParameterListOpt_ok: (block) TC_0 |- typeParameterListOpt : TC_1...
     1                (Let (annotationList_f type_f name_f ;)* be $flatten_typeFieldList(...
  ####                  (Type_ok: (block) TC_1 |- (type_f as typeOrVoid) : typeIR_f # typ...
  ####/####               If ((typeId'* matches pattern []))*, then
     1                      (Let nameIR be $name(name))
  ####                        (Let nameIR_f be $name(name_f))*
  $ sed -n '/^relation Type_wf:/,/^relation/p' none.txt | grep -A5 'Case (% has type headerTypeIR):'
     1/####      Case (% has type headerTypeIR):
     1            (Let (header _typeId < _typeArgumentIR* > { (_annotationList typeIR_fie...
     1/####         If ($distinct_<nameIR>(nameIR_field*)), then
  ####/####           If ($nestable_header(typeIR_field))*, then
  ####/####             If (Type_wf: B |- typeIR_field holds)*, then
     1                    The relation holds
