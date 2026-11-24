open Common.Annotated

(** The subscript starts after the first underscore if a later character is
    something other than an underscore. For example, [x__] has no subscript and
    [x__1] has the subscript [_1]. *)
let split_subscript (name : string) : string * string option =
  let length = String.length name in
  match String.index_opt name '_' with
  | Some index
    when String.exists
           (fun char -> char <> '_')
           (String.sub name index (length - index)) ->
      ( String.sub name 0 index,
        Some (String.sub name (index + 1) (length - index - 1)) )
  | Some _ | None -> (name, None)

let strip_var_suffix id =
  let base, _ = split_subscript id.it in
  match String.index_opt base '\'' with
  | Some index -> String.sub base 0 index $ id.at
  | None -> base $ id.at
