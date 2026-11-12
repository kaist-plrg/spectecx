import Itpl.Definitions.Spec.Environment

namespace Itpl

theorem mem_domain_iff {env : Env} {x : String} : x ∈ domain env ↔ (lookup env x).isSome := by
  induction env with
  | nil => simp [domain, lookup]
  | cons binding env ih =>
    cases binding with
    | value y v => by_cases h : x = y <;> simp [domain, lookup, Binding.name, h, ih]
    | recDef f param body captured =>
      by_cases h : x = f <;> simp [domain, lookup, Binding.name, h, ih]

theorem mem_domainType_iff {tenv : TypeEnv} {x : String} :
    x ∈ domainType tenv ↔ (lookupType tenv x).isSome := by
  induction tenv with
  | nil => simp [domainType, lookupType]
  | cons entry tenv ih =>
    obtain ⟨y, a⟩ := entry
    by_cases h : x = y <;> simp [domainType, lookupType, h, ih]

end Itpl
