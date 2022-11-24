

type ('clause,'pattern) clause = {
  pattern : 'pattern;
  rhs     : 'clause;
}

and ('expr,'clause,'pattern) t = {
  expr         : 'expr;
  cases        : ('clause,'pattern) clause Simple_utils.List.Ne.t ;
} [@@deriving yojson, map, sexp]