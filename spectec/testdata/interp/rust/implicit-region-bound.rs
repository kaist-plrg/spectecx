// Probe: implicit-region-bound.  Open question item 4 (notes/phase3-open-questions.md).
// Chapter 8, rule (ol-comp-body); the carrier clause of Fig. 8.5(a) that puts 'body first.
//
// What it separates: whether the document models rustc's `implicit_region_bound` -- the
// `T: 'fn_body` bound `VerifyBoundCx` adds to every type parameter inside a body
// (rustc_borrowck::universal_regions::implicit_region_bound, consumed by
// rustc_infer::infer::outlives::verify::VerifyBoundCx::param_or_placeholder_bound).
//
// `&t` is typed by (ty-ref) with a fresh region variable ?r, and
// wfobl(Gamma, &?r T) = { T : ?r } (Fig. 4.2).  Nothing bounds ?r from below, so
// rsol gives rho(?r) = {} and theta_rho(?r) = 'body.
//
// Hand derivation's last step: (ol-components) on T : 'body has components(Gamma, T) = T
// and the single premise T :^c 'body, which is the axiom (ol-comp-body).  Model: accept.
// Without 'body in the carrier the assignment would have been 'static -- the only other
// region in the item -- and the premise would have been T :^c 'static, which
// (ol-comp-param) cannot derive from an empty R.  Model before Task 30: reject.
//
// rustc 1.98.1: accepts, with no diagnostic.

fn borrow<T>(t: T) {
    let _r = &t;
}
