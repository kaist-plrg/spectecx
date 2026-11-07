let () =
  Opaque_values.run ();
  Opaque.run ();
  Callbacks.run ();
  Value_types.run ();
  Iteration.run ();
  Optional_iteration.run ();
  Partial_binding.run ();
  Nested_call_expansion.run ();
  Expressions.run ();
  Builtin_arities.run ();
  Builtin_type_arguments.run ();
  Casts.run ();
  Determinism.run ();
  Structuring.run ();
  Value_handlers.run ()
