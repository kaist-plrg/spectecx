let () =
  Opaque_values.run ();
  Opaque.run ();
  Value_types.run ();
  Iteration.run ();
  Optional_iteration.run ();
  Nested_call_expansion.run ();
  Expressions.run ();
  Builtin_arities.run ();
  Determinism.run ()
