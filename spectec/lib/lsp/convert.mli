type encoding = [ `UTF8 | `UTF16 ]

(** Return the line at the given zero-based line number, or [None] if [text] has
    no such line. *)
val line : text:string -> int -> string option

(** Convert a zero-based byte offset into [line] to a zero-based column measured
    in the selected encoding's units. *)
val character : encoding:encoding -> line:string -> int -> int

(** Convert a source position, with a one-based line and a zero-based byte
    column, to an editor position, with a zero-based line and a column measured
    in the selected encoding's units. *)
val position :
  encoding:encoding -> text:string -> Common.Source.pos -> Linol_eio.Position.t

(** Convert a source region to an editor range, applying {!position} to each
    endpoint. *)
val range :
  encoding:encoding -> text:string -> Common.Source.region -> Linol_eio.Range.t
