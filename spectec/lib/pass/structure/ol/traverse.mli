(** Instruction traversals do not descend into [DebugI] bodies. *)

open Ast

(** Apply [f] to each immediate child block. *)
val map_instr_blocks : instr -> f:(instr list -> instr list) -> instr

(** Apply [f] to each instruction after rewriting its child blocks. Blocks
    introduced by [f] are not visited. *)
val rewrite_instrs_postorder : instr list -> f:(instr -> instr) -> instr list

(** Call [f instr later] before visiting [instr]'s blocks. If [f] returns
    [Some instrs], replace [instr :: later] with [instrs] and restart there.
    Otherwise visit the blocks and continue with [later]. [f] must eventually
    stop producing replacements. *)
val rewrite_siblings_preorder :
  instr list -> f:(instr -> instr list -> instr list option) -> instr list

(** A merge decision for an earlier instruction and a later sibling. *)
type sibling_decision =
  | Take of instr
      (** Replace the earlier instruction with the supplied instruction and
          remove the later sibling. *)
  | Skip  (** Keep this sibling and try the next sibling. *)
  | Stop
      (** Stop searching for a sibling to merge into the earlier instruction. *)

(** For each instruction, call [f earlier later] with each later sibling,
    nearest first. After a merge, retry the merged instruction against all
    remaining siblings before visiting its blocks. Skipped siblings keep their
    order. *)
val merge_siblings_preorder :
  instr list -> f:(instr -> instr -> sibling_decision) -> instr list
