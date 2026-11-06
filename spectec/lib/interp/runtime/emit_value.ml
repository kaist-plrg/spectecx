module Dispatcher = Instrumentation_dispatcher.Dispatcher
module Value_event = Instrumentation_api.Value_event

type value = Lang.Il.Value.t

let derived (value : value) ~(source : value) : value =
  if Dispatcher.has_value_handlers () then
    Dispatcher.emit_value (Value_event.Derived { value; source });
  value

let constructed (value : value) ~(components : value list) : value =
  if Dispatcher.has_value_handlers () then
    Dispatcher.emit_value (Value_event.Constructed { value; components });
  value

let combined (value : value) ~(sources : value list) : value =
  if Dispatcher.has_value_handlers () then
    Dispatcher.emit_value (Value_event.Combined { value; sources });
  value

let updated (value : value) ~(base : value) ~(replacement : value) : value =
  if Dispatcher.has_value_handlers () then
    Dispatcher.emit_value (Value_event.Updated { value; base; replacement });
  value

let invoked ~callee ~inputs ~outputs ~cached =
  if Dispatcher.has_value_handlers () then
    Dispatcher.emit_value
      (Value_event.Invoked { callee; inputs; outputs; cached })
