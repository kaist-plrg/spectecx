impty CLI: instruction-coverage full in SL mode against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Full instruction coverage emits a GCOV-style annotated spec over the SL
instructions:

The full report shows successes and failures for each `if` or case
instruction. It also shows how often each case guard held or failed.

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --sl --instruction-coverage.level full
  
  === SL Instruction Coverage ===
  
  SL Instructions: 65/149 attempted (43.62%), 65/149 succeeded (43.62%)
  36 rel instructions
  36 if and case instructions: succeeded 18/36 (50.00%), failed 2/36 (5.56%), neither 18/36 (50.00%), total 20/72 (27.78%)
  
  def $lookup:
     2/####   Case analysis on pair<K, V>*{pair<K, V> <- pair<K, V>*}
  ####/   2      Case (% matches []):
  ####            Return ?()
     2/####      Case (% matches _ :: _):
     2            (Let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} be pair<K, V>*...
     2/####         If ((K_h = K_query)), then
     2                Return ?(V_h)
  ####/####   If (pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _), then
  ####          (Let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} be pair<K, V>*{p...
  ####            Return $lookup<K, V>(K_t -> V_t*{K_t <- K_t*, V_t <- V_t*}, K_query)
  
  relation Check_expr:
     3/   1   Case analysis on expr
     2/   2      Case (% <: literal):
     2            (Let literal be expr as literal)
     2/####         Case analysis on literal
     2/####            Case (% matches `_NUM %`):
     2                  (Let i be literal)
     2                    Result in int
  ####/####            Case (% matches `_BOOL %`):
  ####                  (Let b be literal)
  ####                    Result in bool
     1/   1      Case (% <: id):
     1            (Let x be expr as id)
     1              (Let type?{type <- type?} be $lookup<id, type>(tenv, x))
     1/####           If (type?{type <- type?} matches (_)), then
     1                  (Let ?(t) be type?{type <- type?})
     1                    Result in t
     1/####   Case analysis on expr
  ####/   1      Case (% matches `! %`):
  ####            (Let ! e be expr)
  ####              (Check_expr: tenv |- e : type)
  ####/####           If (type matches `BOOL`), then
  ####                  Result in bool
  ####/   1      Case (% matches `% + %`):
  ####            (Let e_l + e_r be expr)
  ####              (Check_expr: tenv |- e_l : type)
  ####/####           If (type matches `INT`), then
  ####                  (Check_expr: tenv |- e_r : type')
  ####/####               If (type' matches `INT`), then
  ####                      Result in int
     1/####      Case (% matches `% <= %`):
     1            (Let e_l <= e_r be expr)
     1              (Check_expr: tenv |- e_l : type)
     1/####           If (type matches `INT`), then
     1                  (Check_expr: tenv |- e_r : type')
     1/####               If (type' matches `INT`), then
     1                      Result in bool
  ####/####      Case (% matches `% && %`):
  ####            (Let e_l && e_r be expr)
  ####              (Check_expr: tenv |- e_l : type)
  ####/####           If (type matches `BOOL`), then
  ####                  (Check_expr: tenv |- e_r : type')
  ####/####               If (type' matches `BOOL`), then
  ####                      Result in bool
  
  relation Check_command:
     3/####   Case analysis on command
  ####/   3      Case (% matches `SKIP`):
  ####            Result in tenv
     2/   1      Case (% matches `% % = %`):
     2            (Let t x = e be command)
     2              (Check_expr: tenv |- e : type)
     2/####           If ((type = t)), then
     2                  Result in x -> t :: tenv
  ####/   1      Case (% matches `% = %`):
  ####            (Let x = e be command)
  ####              (Check_expr: tenv |- e : t)
  ####/####           If (($lookup<id, type>(tenv, x) = ?(t))), then
  ####                  Result in tenv
  ####/   1      Case (% matches `IF % THEN % ELSE % END`):
  ####            (Let if e then c_1 else c_2 end be command)
  ####              (Check_expr: tenv |- e : type)
  ####/####           If (type matches `BOOL`), then
  ####                  (Check_command: tenv |- c_1 -| tenv_1)
  ####                    (Check_command: tenv |- c_2 -| tenv_2)
  ####                      Result in tenv
  ####/   1      Case (% matches `WHILE % DO % END`):
  ####            (Let while e do c end be command)
  ####              (Check_expr: tenv |- e : type)
  ####/####           If (type matches `BOOL`), then
  ####                  (Check_command: tenv |- c -| tenv_1)
  ####                    Result in tenv
     1/####      Case (% matches `% ; %`):
     1            (Let c_1 ; c_2 be command)
     1              (Check_command: tenv |- c_1 -| tenv_1)
     1                (Check_command: tenv_1 |- c_2 -| tenv_2)
     1                  Result in tenv_2
  
  relation Check_prog:
     1        (Check_command: [] |- command -| tenv)
     1          The relation holds
  
  relation Eval_expr:
     3/   1   Case analysis on expr
     2/   2      Case (% <: literal):
     2            (Let literal be expr as literal)
     2/####         Case analysis on literal
     2/####            Case (% matches `_NUM %`):
     2                  (Let i be literal)
     2                    Result in i
  ####/####            Case (% matches `_BOOL %`):
  ####                  (Let b be literal)
  ####                    Result in b
     1/   1      Case (% <: id):
     1            (Let x be expr as id)
     1              (Let value?{value <- value?} be $lookup<id, value>(env, x))
     1/####           If (value?{value <- value?} matches (_)), then
     1                  (Let ?(v) be value?{value <- value?})
     1                    Result in v
     1/####   Case analysis on expr
  ####/   1      Case (% matches `! %`):
  ####            (Let ! e be expr)
  ####              (Eval_expr: env |- e ==> literal)
  ####/####           If (literal matches `_BOOL %`), then
  ####                  (Let b_e be literal)
  ####                    (Let b be ~b_e)
  ####                      Result in b
  ####/   1      Case (% matches `% + %`):
  ####            (Let e_l + e_r be expr)
  ####              (Eval_expr: env |- e_l ==> literal)
  ####/####           If (literal matches `_NUM %`), then
  ####                  (Let i_l be literal)
  ####                    (Eval_expr: env |- e_r ==> literal')
  ####/####                 If (literal' matches `_NUM %`), then
  ####                        (Let i_r be literal')
  ####                          (Let i be (i_l + i_r))
  ####                            Result in i
     1/####      Case (% matches `% <= %`):
     1            (Let e_l <= e_r be expr)
     1              (Eval_expr: env |- e_l ==> literal)
     1/####           If (literal matches `_NUM %`), then
     1                  (Let i_l be literal)
     1                    (Eval_expr: env |- e_r ==> literal')
     1/####                 If (literal' matches `_NUM %`), then
     1                        (Let i_r be literal')
     1                          (Let b be (i_l <= i_r))
     1                            Result in b
  ####/####      Case (% matches `% && %`):
  ####            (Let e_l && e_r be expr)
  ####              (Eval_expr: env |- e_l ==> literal)
  ####/####           If (literal matches `_BOOL %`), then
  ####                  (Let b_l be literal)
  ####                    (Eval_expr: env |- e_r ==> literal')
  ####/####                 If (literal' matches `_BOOL %`), then
  ####                        (Let b_r be literal')
  ####                          (Let b be (b_l /\ b_r))
  ####                            Result in b
  
  relation Eval_command:
     3/####   Case analysis on command
  ####/   3      Case (% matches `SKIP`):
  ####            Result in env
     2/   1      Case (% matches `% % = %`):
     2            (Let t x = e be command)
     2              (Eval_expr: env |- e ==> v)
     2                Result in x -> v :: env
  ####/   1      Case (% matches `% = %`):
  ####            (Let x = e be command)
  ####              (Eval_expr: env |- e ==> v)
  ####                Result in x -> v :: env
  ####/   1      Case (% matches `IF % THEN % ELSE % END`):
  ####            (Let if e then c_1 else c_2 end be command)
  ####              (Eval_expr: env |- e ==> literal)
  ####/####           If ((literal = true)), then
  ####                  (Eval_command: env |- c_1 -| env_1)
  ####                    Result in env_1
  ####/####           If ((literal = false)), then
  ####                  (Eval_command: env |- c_2 -| env_2)
  ####                    Result in env_2
  ####/   1      Case (% matches `WHILE % DO % END`):
  ####            (Let while e do c end be command)
  ####              (Eval_expr: env |- e ==> literal)
  ####/####           If ((literal = false)), then
  ####                  Result in env
  ####/####           If ((literal = true)), then
  ####                  (Eval_command: env |- c -| env_1)
  ####                    (Eval_command: env_1 |- while e do c end -| env_2)
  ####                      Result in env_2
     1/####      Case (% matches `% ; %`):
     1            (Let c_1 ; c_2 be command)
     1              (Eval_command: env |- c_1 -| env_1)
     1                (Eval_command: env_1 |- c_2 -| env_2)
     1                  Result in env_2
  
  relation Eval_prog:
     1        (Eval_command: [] |- command -| env)
     1          Result in env
  
  relation Run_prog:
     1/####   If (Check_prog: |- command holds), then
     1          (Eval_prog: |- command -| env)
     1            Result in env
  [
    y -> true,
    x -> 5
  ]
