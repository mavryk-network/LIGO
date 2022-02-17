open Ast

type expression_content = [%import: Ast.expression_content]
[@@deriving ez {
      prefixes = [
        ("make_e" ,
          fun ?(loc = Location.generated) expression_content ->
            Location.wrap ~loc expression_content
        ) ;
        ("get" , fun (x: 'a Location.wrap) -> x.wrap_content) ;
      ] ;
      (* wrap_constructor = ("expression_content" , (fun expression_content ?loc () -> make_e ?loc expression_content)) ;
      wrap_get = ("expression_content" , get) ; *)
    } ]

let e__type_ ~loc p : expression = make_e ~loc @@ E_literal (Literal__type_ p)
  [@@map (_type_, ("int" , "nat", "address", "mutez", "signature", "key", "key_hash", "chain_id", "bytes", "string"))]

let e_bytes_hex ~loc b : expression = e_bytes ~loc @@ Hex.to_bytes b
(* let e_int_z ~loc i : expression = e_int ~loc @@ Z.to_int i *)