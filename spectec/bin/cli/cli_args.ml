(** Shared CLI argument parsers, driven by handler specs. Flag names are
    --<handler-name>.<param-name>, e.g. --trace.level. *)

open Instrumentation

(** Build a Param for one typed handler spec. Maps each declared param to a
    --name.param_name flag, then calls D.parse. *)
let handler_param (module D : Handler.Spec.S) :
    Handler.Spec.selected_handler option Core.Command.Param.t =
  let open Core.Command.Param in
  (* One flag per declared parameter *)
  let flag_params =
    List.map
      (fun (param_name, doc) ->
        flag ("--" ^ D.name ^ "." ^ param_name) (optional string) ~doc
        |> map ~f:(fun v -> (param_name, v)))
      D.params
  in
  (* Combine individual flag Params into one Param yielding the full alist.
     both p rest sequences two Params; map cons's each entry onto the list. *)
  let combined =
    List.fold_right
      (fun flag_p rest ->
        both flag_p rest |> map ~f:(fun (entry, entries) -> entry :: entries))
      flag_params (return [])
  in
  map combined ~f:D.parse

(** [--color] flag selection. [Auto] enables color on a TTY (and respects the
    [NO_COLOR] environment variable); [Always] / [Never] force the choice. *)
type color = Auto | Always | Never

let color_arg : color Core.Command.Arg_type.t =
  Core.Command.Arg_type.create (function
    | "auto" -> Auto
    | "always" -> Always
    | "never" -> Never
    | s -> failwith ("expected auto|always|never, got: " ^ s))

let color_flag : color Core.Command.Param.t =
  let open Core.Command.Param in
  flag "--color"
    (optional_with_default Auto color_arg)
    ~doc:"WHEN colorize diagnostics: auto|always|never (default: auto)"

(** Shared instrumentation selection CLI flags — one set of flags per handler
    spec. *)
let config_flags : Config.t Core.Command.Param.t =
  let open Core.Command.Param in
  List.fold_right
    (fun handler_spec rest ->
      both (handler_param handler_spec) rest
      |> map ~f:(fun (handler_opt, handler_opts) -> handler_opt :: handler_opts))
    builtin_handler_specs (return [])
  |> map ~f:(List.filter_map Fun.id)
