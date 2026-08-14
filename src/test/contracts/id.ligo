// PascaLIGO port of id.mligo (Mavryk dialect, 0.73 grammar).
// Semantically equivalent to contracts/id.mligo — exercised by id_tests_p.ml.
// The functions are applied directly by the test (tupled: buy (param, storage)),
// so `main` is only a conventional dispatcher and is not an [@entry].

type id is int

type id_details is
  record [
    owner: address;
    controller: address;
    profile: bytes;
  ]

type buy is
  record [
    profile: bytes;
    initial_controller: option(address);
  ]

type update_owner is
  record [
    id: id;
    new_owner: address;
  ]

type update_details is
  record [
    id: id;
    new_profile: option(bytes);
    new_controller: option(address);
  ]

type action is
  | Buy of buy
  | Update_owner of update_owner
  | Update_details of update_details
  | Skip of unit

type storage is
  record [
    identities: big_map (id, id_details);
    next_id: int;
    name_price: mav;
    skip_price: mav;
  ]

function buy (const parameter : buy; const storage : storage) : list(operation) * storage is
  begin
    const _check_amount : unit =
      if (Mavryk.get_amount() =/= storage.name_price)
      then (failwith("Incorrect amount paid.") : unit)
      else unit;
    const profile : bytes = parameter.profile;
    const initial_controller : option(address) = parameter.initial_controller;
    var identities : big_map (id, id_details) := storage.identities;
    const new_id : int = storage.next_id;
    const controller : address =
      case initial_controller of [
        Some(addr) -> addr
      | None -> Mavryk.get_sender()
      ];
    const new_id_details : id_details =
      record [
        owner = Mavryk.get_sender();
        controller = controller;
        profile = profile;
      ];
    identities[new_id] := new_id_details;
  end with ((nil : list(operation)), record [
      identities = identities;
      next_id = new_id + 1;
      name_price = storage.name_price;
      skip_price = storage.skip_price;
    ])

function update_owner (const parameter : update_owner; const storage : storage) :
         list(operation) * storage is
  begin
    const _check_amount : unit =
      if (Mavryk.get_amount() =/= 0mumav)
      then (failwith("Updating owner doesn't cost anything.") : unit)
      else unit;
    const id : int = parameter.id;
    const new_owner : address = parameter.new_owner;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details : id_details :=
      case identities[id] of [
        Some(found) -> found
      | None -> (failwith("This ID does not exist.") : id_details)
      ];
    const _check_owner : unit =
      if (Mavryk.get_sender() = id_details.owner)
      then unit
      else (failwith("You are not the owner of this ID.") : unit);
    id_details.owner := new_owner;
    identities[id] := id_details;
  end with ((nil: list(operation)), record [
      identities = identities;
      next_id = storage.next_id;
      name_price = storage.name_price;
      skip_price = storage.skip_price;
    ])

function update_details (const parameter : update_details; const storage : storage ) :
         list(operation) * storage is
  begin
    const _check_amount : unit =
      if (Mavryk.get_amount() =/= 0mumav)
      then (failwith("Updating details doesn't cost anything.") : unit)
      else unit;
    const id : int = parameter.id;
    const new_profile : option(bytes) = parameter.new_profile;
    const new_controller : option(address) = parameter.new_controller;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details : id_details :=
      case identities[id] of [
        Some(found) -> found
      | None -> (failwith("This ID does not exist.") : id_details)
      ];
    const _check_allowed : unit =
      if (Mavryk.get_sender() = id_details.controller) or (Mavryk.get_sender() = id_details.owner)
      then unit
      else (failwith("You are not the owner or controller of this ID.") : unit);
    const owner : address = id_details.owner;
    const profile : bytes =
      case new_profile of [
        None -> id_details.profile
      | Some(np) -> np
      ];
    const controller : address =
      case new_controller of [
        None -> id_details.controller
      | Some(nc) -> nc
      ];
    id_details.owner := owner;
    id_details.controller := controller;
    id_details.profile := profile;
    identities[id] := id_details;
  end with ((nil: list(operation)), record [
      identities = identities;
      next_id = storage.next_id;
      name_price = storage.name_price;
      skip_price = storage.skip_price;
    ])

// Let someone skip the next identity so nobody has to take one that's undesirable.
function skip_ (const _p: unit; const storage: storage) : list(operation) * storage is
  begin
    const _check_amount : unit =
      if (Mavryk.get_amount() =/= storage.skip_price)
      then (failwith("Incorrect amount paid.") : unit)
      else unit;
  end with ((nil: list(operation)), record [
      identities = storage.identities;
      next_id = storage.next_id + 1;
      name_price = storage.name_price;
      skip_price = storage.skip_price;
    ])

function main (const action : action; const storage : storage) : list(operation) * storage is
  case action of [
    Buy(b) -> buy (b, storage)
  | Update_owner(uo) -> update_owner (uo, storage)
  | Update_details(ud) -> update_details (ud, storage)
  | Skip(_s) -> skip_ (unit, storage)
  ]
