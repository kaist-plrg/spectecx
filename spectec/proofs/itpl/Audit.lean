import Itpl
import Lean

open Lean Elab Command

#eval show CommandElabM Unit from do
  let env ← getEnv
  let mut axioms : NameSet := {}
  for (name, _) in env.constants.map₁.toList do
    if let some idx := env.getModuleIdxFor? name then
      if (`Itpl).isPrefixOf env.header.moduleNames[idx.toNat]! then
        for ax in ← liftCoreM (collectAxioms name) do
          axioms := axioms.insert ax
  for ax in axioms.toArray.qsort Name.lt do
    logInfo m!"{ax}"
