module H = Harness
module V = Lang.Il.Value
module Case = Lang.Il.Case

let spec = H.compile_file "structuring.spectec"
let nat n = V.nat (Bigint.of_int n)
let letter name = Case.case_v ~var:"letter" [ Case.kw name ]
let check_text_results = H.check_text_results spec

let run () =
  check_text_results ~relation:"Describe"
    [
      ("2", (fun () -> [ nat 2 ]), "even");
      ("3", (fun () -> [ nat 3 ]), "small odd");
      ("11", (fun () -> [ nat 11 ]), "large odd");
    ];
  H.check spec ~name:"shared predicate rejects zero" ~relation:"Describe"
    ~args:(fun () -> [ nat 0 ])
    H.fails;
  check_text_results ~relation:"Different_assertions"
    [ ("0", (fun () -> [ nat 0 ]), "second") ];
  check_text_results ~relation:"Earlier_negated_assertion"
    [ ("0", (fun () -> [ nat 0 ]), "second") ];
  check_text_results ~relation:"Later_negated_assertion"
    [ ("1", (fun () -> [ nat 1 ]), "second") ];
  check_text_results ~relation:"Fuzzy_if"
    [ ("1 and 1", (fun () -> [ nat 1; nat 1 ]), "third") ];
  check_text_results ~relation:"Case_analyses_on_two_values"
    [ ("3 and 1", (fun () -> [ nat 3; nat 1 ]), "third") ];
  check_text_results ~relation:"Case_and_membership_on_two_values"
    [ ("3 and 1", (fun () -> [ nat 3; nat 1 ]), "third") ];
  check_text_results ~relation:"Case_and_subtype_on_two_values"
    [ ("B and D", (fun () -> [ letter "B"; letter "D" ]), "third") ];
  check_text_results ~relation:"Case_and_pattern_on_two_values"
    [ ("C and D", (fun () -> [ letter "C"; letter "D" ]), "third") ];
  check_text_results ~relation:"Case_and_negation_on_two_values"
    [ ("7 and 3", (fun () -> [ nat 7; nat 3 ]), "third") ];
  check_text_results ~relation:"Case_and_inequality_on_two_values"
    [ ("1 and 2", (fun () -> [ nat 1; nat 2 ]), "third") ];
  check_text_results ~relation:"Case_and_swapped_equality_on_two_values"
    [ ("3 and 1", (fun () -> [ nat 3; nat 1 ]), "third") ];
  check_text_results ~relation:"Case_and_swapped_inequality_on_two_values"
    [ ("1 and 2", (fun () -> [ nat 1; nat 2 ]), "third") ];
  check_text_results ~relation:"Assertions_on_two_values"
    [ ("0 and 1", (fun () -> [ nat 0; nat 1 ]), "second") ];
  check_text_results ~relation:"Later_if"
    [ ("2 and 0", (fun () -> [ nat 2; nat 0 ]), "first") ];
  check_text_results ~relation:"Two_case_analyses"
    [
      ("2 and 1", (fun () -> [ nat 2; nat 1 ]), "second");
      ("2 and 3", (fun () -> [ nat 2; nat 3 ]), "fourth");
    ];
  check_text_results ~relation:"Shared_let"
    [
      ("2", (fun () -> [ nat 2 ]), "second");
      ("3", (fun () -> [ nat 3 ]), "third");
    ];
  check_text_results ~relation:"Let_past_let"
    [ ("2", (fun () -> [ nat 2 ]), "second") ];
  check_text_results ~relation:"Shared_call"
    [
      ("2", (fun () -> [ nat 2 ]), "second");
      ("3", (fun () -> [ nat 3 ]), "third");
    ]
