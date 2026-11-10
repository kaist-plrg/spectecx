impty CLI: instruction-coverage summary in SL mode against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

In SL mode the handler counts instruction attempts, successes and failures,
and matches and failures of individual case guards. The summary lists every
instruction and case that never succeeded, including nested entries.

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --sl --instruction-coverage.level summary
  
  === SL Instruction Coverage ===
  
  SL Instructions: 65/149 attempted (43.62%), 65/149 succeeded (43.62%)
  36 rel instructions
  36 if and case instructions: succeeded 18/36 (50.00%), failed 2/36 (5.56%), neither 18/36 (50.00%), total 20/72 (27.78%)
  
  Never succeeded:
    lookup:
      Case (% matches [])
    lookup:
      Return ?()
    lookup:
      If (pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _), then
    lookup:
      (Let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} be pair<K, V>*{pair<K, V> <- pair<K, V>*})
    lookup:
      Return $lookup<K, V>(K_t -> V_t*{K_t <- K_t*, V_t <- V_t*}, K_query)
    Check_expr:
      Case (% matches `_BOOL %`)
    Check_expr:
      (Let b be literal)
    Check_expr:
      Result in bool
    Check_expr:
      Case (% matches `! %`)
    Check_expr:
      (Let ! e be expr)
    Check_expr:
      (Check_expr: tenv |- e : type)
    Check_expr:
      If (type matches `BOOL`), then
    Check_expr:
      Result in bool
    Check_expr:
      Case (% matches `% + %`)
    Check_expr:
      (Let e_l + e_r be expr)
    Check_expr:
      (Check_expr: tenv |- e_l : type)
    Check_expr:
      If (type matches `INT`), then
    Check_expr:
      (Check_expr: tenv |- e_r : type')
    Check_expr:
      If (type' matches `INT`), then
    Check_expr:
      Result in int
    Check_expr:
      Case (% matches `% && %`)
    Check_expr:
      (Let e_l && e_r be expr)
    Check_expr:
      (Check_expr: tenv |- e_l : type)
    Check_expr:
      If (type matches `BOOL`), then
    Check_expr:
      (Check_expr: tenv |- e_r : type')
    Check_expr:
      If (type' matches `BOOL`), then
    Check_expr:
      Result in bool
    Check_command:
      Case (% matches `SKIP`)
    Check_command:
      Result in tenv
    Check_command:
      Case (% matches `% = %`)
    Check_command:
      (Let x = e be command)
    Check_command:
      (Check_expr: tenv |- e : t)
    Check_command:
      If (($lookup<id, type>(tenv, x) = ?(t))), then
    Check_command:
      Result in tenv
    Check_command:
      Case (% matches `IF % THEN % ELSE % END`)
    Check_command:
      (Let if e then c_1 else c_2 end be command)
    Check_command:
      (Check_expr: tenv |- e : type)
    Check_command:
      If (type matches `BOOL`), then
    Check_command:
      (Check_command: tenv |- c_1 -| tenv_1)
    Check_command:
      (Check_command: tenv |- c_2 -| tenv_2)
    Check_command:
      Result in tenv
    Check_command:
      Case (% matches `WHILE % DO % END`)
    Check_command:
      (Let while e do c end be command)
    Check_command:
      (Check_expr: tenv |- e : type)
    Check_command:
      If (type matches `BOOL`), then
    Check_command:
      (Check_command: tenv |- c -| tenv_1)
    Check_command:
      Result in tenv
    Eval_expr:
      Case (% matches `_BOOL %`)
    Eval_expr:
      (Let b be literal)
    Eval_expr:
      Result in b
    Eval_expr:
      Case (% matches `! %`)
    Eval_expr:
      (Let ! e be expr)
    Eval_expr:
      (Eval_expr: env |- e ==> literal)
    Eval_expr:
      If (literal matches `_BOOL %`), then
    Eval_expr:
      (Let b_e be literal)
    Eval_expr:
      (Let b be ~b_e)
    Eval_expr:
      Result in b
    Eval_expr:
      Case (% matches `% + %`)
    Eval_expr:
      (Let e_l + e_r be expr)
    Eval_expr:
      (Eval_expr: env |- e_l ==> literal)
    Eval_expr:
      If (literal matches `_NUM %`), then
    Eval_expr:
      (Let i_l be literal)
    Eval_expr:
      (Eval_expr: env |- e_r ==> literal')
    Eval_expr:
      If (literal' matches `_NUM %`), then
    Eval_expr:
      (Let i_r be literal')
    Eval_expr:
      (Let i be (i_l + i_r))
    Eval_expr:
      Result in i
    Eval_expr:
      Case (% matches `% && %`)
    Eval_expr:
      (Let e_l && e_r be expr)
    Eval_expr:
      (Eval_expr: env |- e_l ==> literal)
    Eval_expr:
      If (literal matches `_BOOL %`), then
    Eval_expr:
      (Let b_l be literal)
    Eval_expr:
      (Eval_expr: env |- e_r ==> literal')
    Eval_expr:
      If (literal' matches `_BOOL %`), then
    Eval_expr:
      (Let b_r be literal')
    Eval_expr:
      (Let b be (b_l /\ b_r))
    Eval_expr:
      Result in b
    Eval_command:
      Case (% matches `SKIP`)
    Eval_command:
      Result in env
    Eval_command:
      Case (% matches `% = %`)
    Eval_command:
      (Let x = e be command)
    Eval_command:
      (Eval_expr: env |- e ==> v)
    Eval_command:
      Result in x -> v :: env
    Eval_command:
      Case (% matches `IF % THEN % ELSE % END`)
    Eval_command:
      (Let if e then c_1 else c_2 end be command)
    Eval_command:
      (Eval_expr: env |- e ==> literal)
    Eval_command:
      If ((literal = true)), then
    Eval_command:
      (Eval_command: env |- c_1 -| env_1)
    Eval_command:
      Result in env_1
    Eval_command:
      If ((literal = false)), then
    Eval_command:
      (Eval_command: env |- c_2 -| env_2)
    Eval_command:
      Result in env_2
    Eval_command:
      Case (% matches `WHILE % DO % END`)
    Eval_command:
      (Let while e do c end be command)
    Eval_command:
      (Eval_expr: env |- e ==> literal)
    Eval_command:
      If ((literal = false)), then
    Eval_command:
      Result in env
    Eval_command:
      If ((literal = true)), then
    Eval_command:
      (Eval_command: env |- c -| env_1)
    Eval_command:
      (Eval_command: env_1 |- while e do c end -| env_2)
    Eval_command:
      Result in env_2
  [
    y -> true,
    x -> 5
  ]

An SL batch reports instruction coverage across its inputs.

  $ spectec impty batch --spec $SPEC --batch-dir ../../testdata/interp/impty/base --color never --sl --instruction-coverage.level summary
  
  === SL Instruction Coverage ===
  
  SL Instructions: 149/149 attempted (100.00%), 149/149 succeeded (100.00%)
  36 rel instructions
  36 if and case instructions: succeeded 36/36 (100.00%), failed 11/36 (30.56%), neither 0/36 (0.00%), total 47/72 (65.28%)
  typechecker: 14/14 passed, 0 failed
  evaluator: 14/14 passed, 0 failed

SL typechecking over a directory reports instruction coverage across its inputs.

  $ spectec impty typecheck --spec $SPEC -p $HELLO --batch-dir ../../testdata/interp/impty/base --color never --sl --instruction-coverage.level summary | grep -A2 'SL Instruction Coverage'
  === SL Instruction Coverage ===
  
  SL Instructions: 70/149 attempted (46.98%), 70/149 succeeded (46.98%)
