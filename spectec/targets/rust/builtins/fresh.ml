(** The generators the document names as builtin.

    "Freshness is generated, not chosen" (spec/ch02-syntax.tex §2.9): the region
    variables of Ch. 10 and the inference variables of Ch. 14 are supplied by a
    counter, not chosen existentially.  [$fresh_rgid()] yields ["?r<n>"] and
    [$fresh_tyid()] ["?t<n>"], the spellings Fig. 2.4 uses for [RVAR] and for an
    inference variable.

    [$fresh_rvar()] yields the same counter as a [nat], because the encoding's
    region-variable production is [RVAR nat] and not [RVAR rgid]: Chapters 9 and
    10 generate [?r] with [freshrg] (ty-ref, ty-ref-mut, callsig's theta_?,
    co-sub-fnptr), and Chapter 8's [rsol] solves for one by its number.  All
    three draw on the one counter, so the ids of a run stay distinct. *)

module Il = Lang.Il
open Il
open Common.Source
open Builtins

module GlobalFreshProvider : sig
  val with_provider : (string -> string) -> (unit -> 'a) -> 'a
  val fresh : string -> string
end = struct
  let provider : (string -> string) ref = ref (fun p -> "?" ^ p ^ "0")

  let with_provider p f =
    let previous = !provider in
    provider := p;
    Fun.protect f ~finally:(fun () -> provider := previous)

  let fresh prefix = !provider prefix
end

let text_of ~(synid : string) (s : string) : Value.t =
  let typ = VarT { synid = synid $ no_region; targs = [] } in
  Il.Value.Make.text typ s

(* dec $fresh_rgid() : rgid *)
let fresh_rgid ~at : Value.t result =
  at |> ignore;
  Ok (text_of ~synid:"rgid" (GlobalFreshProvider.fresh "r"))

(* dec $fresh_tyid() : tyid *)
let fresh_tyid ~at : Value.t result =
  at |> ignore;
  Ok (text_of ~synid:"tyid" (GlobalFreshProvider.fresh "t"))

(* dec $fresh_rvar() : nat *)
let fresh_rvar ~at : Value.t result =
  at |> ignore;
  (* The provider's spelling is "?<prefix><n>"; the number is what RVAR takes. *)
  let s = GlobalFreshProvider.fresh "r" in
  let digits = String.sub s 2 (String.length s - 2) in
  Ok (Il.Value.nat (Bigint.of_string digits))

let builtins =
  [ ("fresh_rgid", Define.T0.a0 fresh_rgid);
    ("fresh_tyid", Define.T0.a0 fresh_tyid);
    ("fresh_rvar", Define.T0.a0 fresh_rvar) ]
