## pass 'restrict pattern'

- remove : P_literal _

unsupported pattern

## pass 'not sure it's a pass'

E_let_mut_in still got : 
- lhs as a list
- type params
- rhs_type
- is_rec

must handle it in some existing pass (forgot which ones)

emit E_simple_let_mut_in

## pass 'e_for'

e_for default incr ( = 1 )

## pass 'pattern rest'

- remove : P_rest
- add : -
??

## pass 'tuple_as_record'

- remove : T_tuple _
-  add : -

convert tuple as records

## pass 'restrict type_modA'

only variable on the right A.t

## pass 'container'

- remove :  E_Map _ | E_BigMap _ | E_Sequence _ | E_List _ | E_Set _
            | E_MapLookup _
- add : -

constantize containers

## pass 'cond to match'

- remove : E_cond
- add : -

morh conds to match on True/False

## pass 'for_each_restrict'

ForAny { pattern ; _ } ; pattern must be a variable (temporary until we get smart )

## pass 'discriminated union'

- remove : T_Disc [("a", Some b); ("c", Some d); ("e", None)]
- add    : -

See handling of TDisc in JsLIGO abstractor AND GOOD LUCK


