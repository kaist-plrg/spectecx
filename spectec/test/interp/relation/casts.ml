module H = Harness
module V = Lang.Il.Value
module B = Interp.Builtins

let ( $ ) = Common.Annotated.( $ )

let is_int (value : V.t) =
  match value.it with Lang.Il.NumV (`Int _) -> true | _ -> false

let is_nat (value : V.t) =
  match value.it with Lang.Il.NumV (`Nat _) -> true | _ -> false

let typ_int = Lang.Il.Typ.int $ Common.Location.Unknown

module Target : Interp.Target.S = struct
  include H.Pure_target

  let builtins =
    [
      ( "count_ints",
        B.Define.T0.a1 (B.Arg.list_of B.Arg.int) (fun ~at:_ ints ->
            Ok (V.nat (Bigint.of_int (List.length ints)))) );
      ( "is_some_int",
        B.Define.T0.a1 B.Arg.value (fun ~at:_ value ->
            match value.it with
            | Lang.Il.OptV (Some value) -> Ok (V.bool (is_int value))
            | _ -> Ok (V.bool false)) );
      ( "count_nats",
        B.Define.T0.a1 (B.Arg.list_of B.Arg.nat) (fun ~at:_ nats ->
            Ok (V.nat (Bigint.of_int (List.length nats)))) );
      ( "is_some_nat",
        B.Define.T0.a1 B.Arg.value (fun ~at:_ value ->
            match value.it with
            | Lang.Il.OptV (Some value) -> Ok (V.bool (is_nat value))
            | _ -> Ok (V.bool false)) );
      ( "ints",
        B.Define.T0.a1 B.Arg.nat (fun ~at:_ n ->
            Ok (V.list typ_int [ V.int Bigint.zero; V.int n ])) );
      ( "some_int",
        B.Define.T0.a1 B.Arg.nat (fun ~at:_ n ->
            Ok (V.opt typ_int (Some (V.int n)))) );
    ]
end

let spec = H.compile_file "casts.spectec"

let run () =
  H.check spec
    ~target:(module Target)
    ~name:"a cast to a list of integers converts each element"
    ~relation:"ListCast"
    ~args:(fun () -> [ V.nat (Bigint.of_int 1) ])
    (H.returns (fun () -> [ V.nat (Bigint.of_int 2) ]));
  H.check spec
    ~target:(module Target)
    ~name:"a cast to an integer option converts its value"
    ~relation:"OptionCast"
    ~args:(fun () ->
      [
        V.opt
          (Lang.Il.Typ.nat $ Common.Location.Unknown)
          (Some (V.nat Bigint.one));
      ])
    (H.returns (fun () -> [ V.bool true ]));
  H.check spec
    ~target:(module Target)
    ~name:"a cast to a list of naturals converts each element"
    ~relation:"ListDowncast"
    ~args:(fun () -> [ V.nat (Bigint.of_int 1) ])
    (H.returns (fun () -> [ V.nat (Bigint.of_int 2) ]));
  H.check spec
    ~target:(module Target)
    ~name:"a cast to a natural option converts its value"
    ~relation:"OptionDowncast"
    ~args:(fun () -> [ V.nat (Bigint.of_int 1) ])
    (H.returns (fun () -> [ V.bool true ]))
