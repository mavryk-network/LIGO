let create_and_call =  (st: list(address)) => {
    let (create_op, addr) =
        Mavryk.create_contract(
            ((p, s): (int, int)) => ([] : list(operation), (p + s)),
            (None : option(key_hash)),
            0mumav,
            1
        );
    let call_op =
        Mavryk.transaction(
            (addr, 41),
            0mumav,
            Mavryk.self("%callback") : contract((address, int))
        );
    ([create_op, call_op], [addr, ...st]);
};

let call_counter = ((addr, n): (address, int)) => {
    let u = assert(Mavryk.get_sender () == Mavryk.get_self_address ());
    let callee_opt: option(contract(int)) = 
        Mavryk.get_contract_opt(addr);
    let callee = 
        switch(callee_opt) {
        | Some(contract) => contract
        | None =>
            (failwith("Could not find contract") : contract(int))
        };
    Mavryk.transaction(n, 0mumav, callee);
};

type parameter = 
| Callback((address, int))
| CreateAndCall;

let main = ((param, st): (parameter, list(address))) => {
    switch(param) {
    | CreateAndCall => create_and_call(st)
    | Callback vs => ([call_counter(vs)], st)
    };
}
