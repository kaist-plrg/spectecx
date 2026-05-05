module type S = sig
  module Target : Spectec.Target.S

  val name : string
  val command : Core.Command.t
end
