include Types

let premise_of (instr : instr) =
  match instr.note with
  | Premise prem -> Some prem
  | Sidecondition _ | Input_sidecondition -> None

let is_synthesized instr = Option.is_none (premise_of instr)

module Free = Free
module Print = Print
