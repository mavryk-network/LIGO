module C is {
  type param is int * ticket (string)
  type storage is string * address

  [@entry]
  function main (const p : param; const _s : storage) : list (operation) * storage is
    block {
      const (_n, t) = p;
      const ((_addr, (v, _amt)), _t) = Mavryk.read_ticket (t);
    } with ((nil : list (operation)), (v, Mavryk.get_sender ()))
}

const test_transfer_to_contract =
  block {
    const orig = Test.originate (contract_of C, ("bye", Test.nth_bootstrap_account (1)), 1mumav);
    const main_addr = Test.to_address (orig.addr);

    // Use this address everytime you want to send tickets from the same proxy-contract
    // mk_param is executed __by the proxy contract__
    const mk_param = function (const t : ticket (string)) : C.param is (42, t);
    // initialize a proxy contract in charge of creating and sending your tickets
    const proxy_taddr = Test.Proxy_ticket.init_transfer (mk_param);
    const _u1 = Test.log (("poxy addr:", proxy_taddr));

    // ticket_info lets you control the amount and the value of the tickets you send
    const ticket_info1 = ("hello", 10n);
    // we send ticket to C through the proxy-contract
    const _r1 = Test.Proxy_ticket.transfer (proxy_taddr, (ticket_info1, main_addr));
    const _u2 = Test.log (Test.get_storage (orig.addr));

    const ticket_info2 = ("world", 5n);
    const _r2 = Test.Proxy_ticket.transfer (proxy_taddr, (ticket_info2, main_addr));
    const _u3 = Test.log (Test.get_storage (orig.addr));
  } with unit