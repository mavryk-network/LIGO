
(* 
  if the michelson annotations are not compared when looking up the environment for a sum or record type,
  the first type could shadow the second one
*)
type type_decl1_or is michelson_or(int,"one",nat,"two")
type type_decl2_or is michelson_or(int,"three",nat,"four")