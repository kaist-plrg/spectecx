module Location = Common.Location
module H = Harness
module V = Lang.Il.Value
module Typ = Lang.Il.Typ
module Case = Lang.Il.Case

let ( $ ) = Common.Annotated.( $ )
let spec = H.compile_file "overlap.spectec"
let nat_type = Typ.nat $ Location.Unknown
let nat n = V.nat (Bigint.of_int n)
let nats ns = V.list nat_type (List.map nat ns)
let letter name = Case.case_v ~var:"letter" [ Case.kw name ]

let shared_tuple () =
  V.tuple [ nat 1; V.text "a"; V.bool true; letter "A"; nats [ 1 ] ]

let check_text_results = H.check_text_results spec

(* Wrong condition comparisons can make SL and PL skip an applicable rule or
   apply a rule whose conditions fail. *)
(* These tests detect false Identical and Disjoint answers. *)
(* These tests also detect guards that test the wrong value or belong to
   the other condition in a comparison. *)
(* If structuring merges no later rule past two rules, a cautious Fuzzy
   answer for their conditions preserves results, so these tests cannot
   detect that answer. *)
(* Disjoint in place of Partition changes only the Else Phantom coverage
   marker in SL, so result checks cannot distinguish these answers. *)
(* structuring.ml checks equalities of one variable with literals on the
   same side through Case_analyses_on_two_values and similar relations. *)
(* Describe and the Case_and inequality relations in structuring.ml check
   equality paired with inequality on the same operands in the same order. *)
(* determinism.ml checks cons patterns paired with fixed-length and empty
   list patterns. *)
let run () =
  check_text_results ~relation:"Equalities_on_two_values"
    [
      ("1 and 2", (fun () -> [ nat 1; nat 2 ]), "second");
      ("1 and 3", (fun () -> [ nat 1; nat 3 ]), "third");
    ];
  check_text_results ~relation:"Equal_numbers_swapped"
    [ ("3", (fun () -> [ nat 3 ]), "other") ];
  check_text_results ~relation:"Equal_or_unequal_swapped"
    [ ("1 and 2", (fun () -> [ nat 1; nat 2 ]), "second") ];
  check_text_results ~relation:"Earlier_negation"
    [ ("7", (fun () -> [ nat 7 ]), "second") ];
  check_text_results ~relation:"Later_negation"
    [ ("3", (fun () -> [ nat 3 ]), "second") ];
  check_text_results ~relation:"Subtypes"
    [
      ("A", (fun () -> [ letter "A" ]), "first");
      ("B", (fun () -> [ letter "B" ]), "second");
      ("C", (fun () -> [ letter "C" ]), "third");
    ];
  check_text_results ~relation:"Overlapping_subtypes"
    [
      ("B", (fun () -> [ letter "B" ]), "second");
      ("C", (fun () -> [ letter "C" ]), "second");
    ];
  check_text_results ~relation:"Contained_subtypes"
    [
      ("B and 0", (fun () -> [ letter "B"; nat 0 ]), "second");
      ("C and 1", (fun () -> [ letter "C"; nat 1 ]), "second");
    ];
  check_text_results ~relation:"Constructors"
    [ ("B", (fun () -> [ letter "B" ]), "second") ];
  check_text_results ~relation:"List_lengths"
    [ ("[1, 2]", (fun () -> [ nats [ 1; 2 ] ]), "second") ];
  check_text_results ~relation:"Empty_after_singleton"
    [ ("[]", (fun () -> [ nats [] ]), "second") ];
  check_text_results ~relation:"Options"
    [ ("?()", (fun () -> [ V.opt nat_type None ]), "second") ];
  check_text_results ~relation:"Membership"
    [ ("3", (fun () -> [ nat 3 ]), "second") ];
  check_text_results ~relation:"Overlapping_membership"
    [
      ( "the shared tuple and 1",
        (fun () -> [ shared_tuple (); nat 1 ]),
        "second" );
    ];
  check_text_results ~relation:"Earlier_negation_on_two_values"
    [ ("3 and 7", (fun () -> [ nat 3; nat 7 ]), "second") ];
  check_text_results ~relation:"Subtypes_on_two_values"
    [ ("A and C", (fun () -> [ letter "A"; letter "C" ]), "second") ];
  check_text_results ~relation:"Patterns_on_two_values"
    [ ("A and B", (fun () -> [ letter "A"; letter "B" ]), "second") ];
  check_text_results ~relation:"Membership_on_two_values"
    [ ("1 and 2", (fun () -> [ nat 1; nat 2 ]), "second") ];
  check_text_results ~relation:"Equal_or_unequal_literals"
    [ ("1 and 1", (fun () -> [ nat 1; nat 1 ]), "second") ];
  check_text_results ~relation:"Equal_or_unequal_on_two_values"
    [ ("1 and 2", (fun () -> [ nat 1; nat 2 ]), "second") ];
  check_text_results ~relation:"Equal_or_unequal_literals_swapped"
    [ ("1 and 1", (fun () -> [ nat 1; nat 1 ]), "second") ];
  check_text_results ~relation:"Equal_or_unequal_on_two_values_swapped"
    [ ("1 and 2", (fun () -> [ nat 1; nat 2 ]), "second") ];
  check_text_results ~relation:"Later_negation_on_two_values"
    [ ("7 and 0", (fun () -> [ nat 7; nat 0 ]), "first") ]
