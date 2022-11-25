

type ('pattern,'branch) clause = {
  pattern : 'pattern;
  rhs     : 'branch;
}

and ('expr,'pattern,'branch) t = {
  expr         : 'expr;
  cases        : ('pattern,'branch) clause Simple_utils.List.Ne.t ;
} [@@deriving yojson, map, sexp]