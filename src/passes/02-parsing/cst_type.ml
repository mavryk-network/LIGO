module Cameligo  = Cst_cameligo
module Jsligo    = Cst_jsligo

type _ cst_type =
| Jsligo_type : Jsligo.CST.t -> Jsligo.CST.t cst_type
| Cameligo_type : Cameligo.CST.t -> Cameligo.CST.t cst_type
