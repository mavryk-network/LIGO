// originate.ligo

type storage is option (ticket (bytes))
type unforged_storage is option (unforged_ticket (bytes))

function main (const _u : unit; const s : storage) : list (operation) * storage is
  ((nil : list (operation)),
   case s of [
     Some (tk) -> block { const (_info, tk2) = Mavryk.read_ticket (tk); } with Some (tk2)
   | None -> None
   ])

const mk_storage = function (const t : ticket (bytes)) : storage is Some (t);

const test_originate_contract =
  block {
    const ticket_info = (0x0202, 15n);
    const addr = Test.Proxy_ticket.originate (ticket_info, mk_storage, main);
    const unforged_storage : unforged_storage = Test.Proxy_ticket.get_storage (addr);
  } with
    // the ticket 'unforged_storage' can be manipulated freely without caring about ticket linearity
    case unforged_storage of [
      Some (x) ->
        block {
          const _l = Test.log (("unforged_ticket", unforged_storage));
          const _a1 = assert (x.value = ticket_info.0);
          const _a2 = assert (x.amount = ticket_info.1);
        } with unit
    | None -> (failwith ("impossible") : unit)
    ]