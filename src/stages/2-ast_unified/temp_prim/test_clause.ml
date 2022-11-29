type ('instruction, 'statement) t =
  | ClauseInstr of 'instruction
  | ClauseBlock of 'statement Simple_utils.List.Ne.t
[@@deriving yojson, map, sexp]
