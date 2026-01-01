(** Target - Groups multiple tasks for a target.

    A TARGET defines:
    - name: Target identifier (e.g., "p4", "ethereum")
    - spec_dir: Directory containing the target's spec files
    - tasks: List of tasks that belong to the target *)

module type TARGET = sig
  val name : string
  val spec_dir : string
  val tasks : Task.packed_task list
end
