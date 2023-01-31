

## pass 'e_for'

e_for default incr ( = 1 )

## pass 'reduce sequence'

- remove : E_sequence
- add : -

## pass 'container'

- remove :  E_Map _ | E_BigMap _ | E_List _ | E_Set _
            | E_MapLookup _
- add : -

constantize containers



## pass 'for_each_restrict'

ForAny { pattern ; _ } ; pattern must be a variable (temporary until we get smart )

## pass 'discriminated union'

- remove : T_Disc [("a", Some b); ("c", Some d); ("e", None)]
- add    : -

See handling of TDisc in JsLIGO abstractor AND GOOD LUCK


