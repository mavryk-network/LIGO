type balances = map(address, mav);

let balances_under = ( (b, threshold) : (balances, mav) ) : balances => 
  let f = ( (acc,(k,v)) : (balances, (address, mav)) ) =>  if (v < threshold) { Map.remove (k,acc) } else {acc} ;
  Map.fold (f,b,b)