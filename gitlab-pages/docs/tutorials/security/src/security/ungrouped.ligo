type parameter is Fund of unit | Send of address * mav

type transaction is Incoming of address * mav | Outgoing of address * mav

type storage is record [owner : address; transactionLog : list (transaction)]

type result is list (operation) * storage

function do_send (const dst : address; const amt : mav) : transaction * list (operation) is
  case Mavryk.get_contract_opt (dst) of [
    Some (c) -> (Outgoing (dst, amt), list [Mavryk.transaction (unit, amt, c)])
  | None -> (failwith ("Could not send tokens") : transaction * list (operation))
  ]

function do_fund (const from_ : address; const amt : mav) : transaction * list (operation) is
  (Incoming (from_, amt), (nil : list (operation)))

[@entry]
function fund (const _u : unit; const s : storage) : result is
  block {
    const r : transaction * list (operation) = do_fund (Mavryk.get_sender (), Mavryk.get_amount ());
    const tx : transaction = r.0;
    const ops : list (operation) = r.1;
  } with (ops, s with record [transactionLog = tx # s.transactionLog])

[@entry]
function send (const args : address * mav; const s : storage) : result is
  block {
    const _u : unit = assert (Mavryk.get_sender () = s.owner and Mavryk.get_amount () = 0mumav);
    const r : transaction * list (operation) = do_send (args.0, args.1);
    const tx : transaction = r.0;
    const ops : list (operation) = r.1;
  } with (ops, s with record [transactionLog = tx # s.transactionLog])
type storage is record [beneficiary : address; balances : map (address, mav)]

type parameter is mav * contract (unit)

function withdraw (const param : parameter; const s : storage) : list (operation) * storage is
  block {
    const amt : mav = param.0;
    const beneficiary : contract (unit) = param.1;
    const beneficiary_addr : address = Mavryk.address (beneficiary);
    const bal : mav =
      case Map.find_opt (beneficiary_addr, s.balances) of [
        Some (v) -> v
      | None -> 0mumav
      ];
    const new_balance : mav =
      case bal - amt of [
        Some (x) -> x
      | None -> (failwith ("Insufficient balance") : mav)
      ];
    const op : operation = Mavryk.transaction (unit, amt, beneficiary);
    const new_balances : map (address, mav) =
      Map.update (beneficiary_addr, Some (new_balance), s.balances);
  } with (list [op], s with record [balances = new_balances])
type storage is record [owner : address; beneficiaries : list (address)]

function send_rewards (const beneficiary_addr : address) : operation is
  block {
    const maybe_contract : option (contract (unit)) = Mavryk.get_contract_opt (beneficiary_addr);
    const beneficiary : contract (unit) =
      case maybe_contract of [
        Some (c) -> c
      | None -> (failwith ("CONTRACT_NOT_FOUND") : contract (unit))
      ];
  } with Mavryk.transaction (unit, 5000000mumav, beneficiary)

function main (const _p : unit; const s : storage) : list (operation) * storage is
  if Mavryk.get_sender () =/= s.owner
  then (failwith ("ACCESS_DENIED") : list (operation) * storage)
  else
    block {
      const ops : list (operation) = List.map (send_rewards, s.beneficiaries);
    } with (ops, s)