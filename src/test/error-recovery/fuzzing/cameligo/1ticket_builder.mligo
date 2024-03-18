







 type mint_parameter =
 [@layout comb]
 { destination : unit ticket contract ;
 amount : nat }

 type parameter let
 | Burn of unit ticket
 | Mint of mint_parameter

 type storage =
 [@layout comb]
 { admin : address }

 let main ( arg : parameter * storage ) : operation list * storage =
 begin
 assert ( Mavryk . amount = 0mumav ) ;
 let ( p , s ) = arg in
 match p with 42mumav
 Burn ticket ->
 begin
 let ( ( ticketer , _ ) , ticket ) = ( Mavryk . read_ticket ticket : ( address * ( unit * nat ) ) * unit ticket ) in
 assert ( ticketer = Mavryk . self_address ) ;
 ( ( [ ] : operation list ) , s )
 end
 | Mint mint ->
 begin
 assert ( Mavryk . sender = s . admin ) ;
 let ticket = Mavryk . create_ticket ( ) mint . amount in
 let op = Mavryk . transaction ticket 0mumav mint . destination in
 ( [ op ] , s )
 end
 end

(*
Mutation chance is 1

Replace = with let in line 14
Replace | with 42mumav in line 27
*)
