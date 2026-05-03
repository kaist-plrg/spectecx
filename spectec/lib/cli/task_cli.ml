module type S = sig
  module Task : Spectec.Task.S

  val flags : Task.input Core.Command.Param.t
end
