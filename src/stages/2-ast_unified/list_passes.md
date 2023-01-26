



## pass 'restrict proj'

  - remove : E_proj (.. Component_expr e)
  - add : ..

  morph it to a C_MAP_GET_FORCE / C_MAP_GET ? (not sure which one; might depend on the syntax)


## pass 'enum_attributes' TODO AT THE END ..
  - remove : E_Attr (string to string) D_attr P_attr
  - add : E_AttrEnum ((Annot | LAyout | ..)) D_attrenum P_attrEnum

  build small separated language for attributes
  - "[@@private]","[@@inline]" toplevel
  - "[@layout]" types
  - etc..

 
## pass 'discriminated union'

- remove : T_Disc [("a", Some b); ("c", Some d); ("e", None)]
- add    : -

See handling of TDisc in JsLIGO abstractor AND GOOD LUCK


