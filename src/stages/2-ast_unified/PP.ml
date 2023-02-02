open Types

let program ppf (p : program) =
  Format.fprintf ppf "%a" Sexp.pp (S_exp.sexp_of_program p)
