(* 
  if the michelson annotations are not compared when looking up the environment for a sum or record type,
  the first type could shadow the second one
*)
type type_decl1_pair is michelson_pair(int, "one", nat, "two")
type type_decl2_pair is michelson_pair(int, "three", nat, "four")
