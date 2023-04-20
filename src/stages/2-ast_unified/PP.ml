open Types

let program ppf (p : program) =
  Format.fprintf ppf "%a" Sexp.pp_hum (S_exp.sexp_of_program p)


let ty_expr ppf (ty : ty_expr) =
  Format.fprintf ppf "%a" Sexp.pp_hum (S_exp.sexp_of_ty_expr ty)


let pattern ppf (p : pattern) =
  Format.fprintf ppf "%a" Sexp.pp_hum (S_exp.sexp_of_pattern p)
