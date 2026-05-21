module Verdict = struct
  type status = [ `Pass | `Fail | `Discard ]

  type t = {
    status : status;
    arguments : string list;
    stamp : string list;
    shrink : unit -> t Gen.t list;
    generalize : unit -> (string * t Gen.t list) list;
  }

  let neutral =
    {
      status = `Discard;
      arguments = [];
      stamp = [];
      shrink = (fun () -> []);
      generalize = (fun () -> []);
    }

  let pass = { neutral with status = `Pass }
  let fail = { neutral with status = `Fail }
  let discard = neutral
  let add_argument s v = { v with arguments = s :: v.arguments }
  let add_stamp s v = { v with stamp = s :: v.stamp }
end

type t = Verdict.t Gen.t

let of_verdict v = Gen.return v
let generalize_n = 10

let rec for_all ?(shrink = fun _ -> []) ?(generalize = fun _ -> []) ~show gen
    body =
  let open Gen in
  let* a = gen in
  let* v = body a in
  let base = Verdict.add_argument (show a) v in
  return
    {
      base with
      Verdict.shrink =
        (fun () ->
          List.map
            (fun a' -> for_all ~shrink ~generalize ~show (Gen.return a') body)
            (shrink a));
      Verdict.generalize =
        (fun () ->
          List.map
            (fun (s, gen') ->
              let samples =
                List.init generalize_n (fun i ->
                    Gen.map
                      (fun v ->
                        {
                          v with
                          Verdict.arguments =
                            (match v.Verdict.arguments with
                            | _ :: rest -> s :: rest
                            | [] -> [ s ]);
                        })
                      (for_all ~shrink ~generalize ~show (Gen.variant i gen')
                         body))
              in
              (s, samples))
            (generalize a));
    }

let label s prop = Gen.map (Verdict.add_stamp s) prop
