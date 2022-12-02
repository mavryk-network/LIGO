type t = Var.Module_var.t Simple_utils.List.Ne.t
[@@deriving eq, compare, yojson, hash, fold, map]

let pp ppf (path : t) =
  Simple_utils.PP_helpers.(ne_list_sep Var.Module_var.pp (tag ".")) ppf path
