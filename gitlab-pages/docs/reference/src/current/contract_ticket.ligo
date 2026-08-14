type storage is big_map (string, ticket (int))
type parameter is int
type result is list (operation) * storage

[@entry]
function main (const i : parameter; const store : storage) : result is
  block {
    const my_ticket1 = Option.unopt (Mavryk.create_ticket (i, 10n));
    const (_x, x) = Big_map.get_and_update ("hello", Some (my_ticket1), store);
  } with ((nil : list (operation)), x)