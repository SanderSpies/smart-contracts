/*
   Reference implementation if `multi_token` core API.

   Since Babylon does not support pairs as keys for big_map,
   This implementation uses composite `balance_key` represented as `nat`.
   Assumed number of different token types is 2^32 (`max_tokens` constant).
   Both token ID and owner ID are "packed" into single `nat` using first 32 bits
   for token ID and the rest of the bits for owner ID.
   Contract storage also keeps mapping between owner address and owner ID
   represented as `nat` (see `owner_lookup`).

   If tokens are transferred to a new owner address which does not exist
   in `owner_loop` yet, new entry withing lookup is created and the owner
   is assigned a new `nat` ID. This implementation may change in future,
   if support for white list is needed.

   Current implementation is optimized for token transfer, but makes it
   difficult for adding functionality in future which might need retrieve
   aggregate data (like list all token types held by the owner).
 */

#include "../multi_token_interface.religo"

/*  owner -> operator set */
type operators = big_map(address, set(address));

let add_operator = (operator: address, operators: operators): operators => {
  let new_operators =
    switch (Map.find_opt(sender, operators)) {
    | Some(ops) => Set.add(operator, ops)
    | None =>
      /*
         Check that sender implements `multi_token_receiver` interface.
         If not, `get_entrypoint` will fail
       */
      let receiver: contract(multi_token_receiver) =
        Operation.get_entrypoint("%multi_token_receiver", sender);
      Set.literal([operator]);
    };

  Map.update(sender, Some(new_operators), operators);
};

let remove_operator = (operator: address, operators: operators): operators => {
  let new_operators_opt =
    switch (Map.find_opt(sender, operators)) {
    | Some(ops) =>
      let ops = Set.remove(operator, ops);
      if (Set.size(ops) == 0n) {
        (None: option(set(address)));
      } else {
        Some(ops);
      };
    | None => (None: option(set(address)))
    };

  Map.update(sender, new_operators_opt, operators);
};

let is_operator = (param: is_operator_param, operators: operators): operation => {
  let req = param.is_operator_request;
  let operators = Map.find_opt(req.owner, operators);
  let result =
    switch (operators) {
    | None => false
    | Some(ops) => Set.mem(req.operator, ops)
    };

  Operation.transaction((req, result), 0mutez, param.is_operator_view);
};

let max_tokens = 4294967295n;  (* 2^32-1 *)
let owner_offset = 4294967296n;  (* 2^32 *)


/* owner_token_id -> balance */
type balances = big_map(nat, nat);
type owner_lookup = {
  owner_count: nat,
  /* owner_address -> owner_id */
  owners: big_map(address, nat),
};

type balance_storage = {
  owners: owner_lookup,
  balances,
};

type owner_result = {
  id: nat,
  owners: owner_lookup,
};

/* return updated storage and newly added owner id */
let add_owner = (owner: address, s: owner_lookup): owner_result => {
  let owner_id = s.owner_count + 1n;
  let os = Map.add(owner, owner_id, s.owners);
  let new_s = {owner_count: owner_id, owners: os};
  {id: owner_id, owners: new_s};
};

/*
   gets existing owner id. If owner does not have one, creates a new id and adds
   it to an owner_lookup
 */
let ensure_owner_id = (owner: address, s: owner_lookup): owner_result => {
  let owner_id = Map.find_opt(owner, s.owners);
  switch (owner_id) {
  | Some(id) => {id, owners: s}
  | None => add_owner(owner, s)
  };
};

let get_owner_id = (owner: address, s: owner_lookup): nat => {
  let owner_id = Map.find_opt(owner, s.owners);
  switch (owner_id) {
  | None => (failwith("No such owner"): nat)
  | Some(id) => id
  };
};

let make_balance_key_impl = (owner_id: nat, token_id: nat): nat =>
  if (token_id > max_tokens) {
    (failwith("provided token ID is out of allowed range"): nat);
  } else {
    token_id + (owner_id + owner_offset);
  };

let make_balance_key = (owner: address, token_id: nat, s: owner_lookup): nat => {
  let owner_id = get_owner_id(owner, s);
  make_balance_key_impl(owner_id, token_id);
};

type owner_key_result = {
  key: nat,
  owners: owner_lookup,
};

let get_balance = (key: nat, b: balances): nat => {
  let bal: option(nat) = Map.find_opt(key, b);
  switch (bal) {
  | None => 0n
  | Some(b) => b
  };
};

let get_balance_req = (r: balance_request, s: balance_storage): nat => {
  let balance_key = make_balance_key(r.owner, r.token_id, s.owners);
  get_balance(balance_key, s.balances);
};

let balance_of = (param: balance_of_param, s: balance_storage): operation => {
  let to_balance = (r: balance_request) => {
    let bal = get_balance_req(r, s);
    (r, bal);
  };

  let requests_2_bals = List.map(to_balance, param.balance_request);
  Operation.transaction(requests_2_bals, 0mutez, param.balance_view);
};

let transfer_balance =
    (from_key: nat, to_key: nat, amt: nat, s: balances): balances => {
  let from_bal = get_balance(from_key, s);
  switch (Michelson.is_nat(from_bal - amt)) {
  | None => (failwith("Insufficient balance"): balances)
  | Some(fbal) =>
    let s1 =
      if (fbal == 0n) {
        Map.remove(from_key, s);
      } else {
        Map.update(from_key, Some(fbal), s);
      };

    let to_bal = get_balance(to_key, s1);
    let tbal = to_bal + amt;
    let s2 = Map.update(to_key, Some(tbal), s1);
    s2;
  };
};

let transfer_safe_check = (param: transfer_param): list(operation) => {
  let receiver: contract(multi_token_receiver) =
    Operation.get_entrypoint("%multi_token_receiver", param.to_);
  let p: on_multi_tokens_received_param = {
    operator: sender,
    from_: Some(param.from_),
    batch: param.batch,
    data: param.data,
  };
  let op =
    Operation.transaction(On_multi_tokens_received(p), 0mutez, receiver);
  [op];
};

let transfer =
    (param: transfer_param, s: balance_storage)
    : (list(operation), balance_store) => {
  let from_id = get_owner_id(param.from_, s.owners);
  let to_o = ensure_owner_id(param.to_, s.owners);
  let make_transfer = (bals: balances, t: tx) => {
    let from_key = make_balance_key_impl(from_id, t.token_id);
    let to_key = make_balance_key_impl(to_o.id, t.token_id);
    transfer_balance(from_key, to_key, t.amount, bals);
  };

  let new_balances = List.fold(make_transfer, param.batch, s.balances);
  let new_store: balance_storage = {
    owners: to_o.owners,
    balances: new_balances,
  };
  let ops = transfer_safe_check(param);
  (ops, new_store);
};

let approved_transfer_from = (from_: address, operators: operators): unit =>
  if (sender == from_) {
    unit;
  } else {
    let ops = Map.find_opt(sender, operators);
    let is_op =
      switch (ops) {
      | None => false
      | Some(o) => Set.mem(from_, o)
      };

    if (is_op) {
      unit;
    } else {
      failwith("operator not approved to transfer tokens");
    };
  };

type multi_token_storage = {
  operators,
  balance_storage,
};

let multi_token_main =
    (param: multi_token, s: multi_token_storage)
    : (list(operation), multi_token_storage) =>
  switch (param) {
  | Transfer(p) =>
    let u: unit = approved_transfer_from(p.from_, s.operators);
    let ops_bstore = transfer(p, s.balance_storage);
    let new_s = {operators: s.operators, balance_storage: ops_bstore[1]};
    (ops_bstore[0], new_s);

  | Balance_of(p) =>
    let op = balance_of(p, s.balance_storage);
    ([op], s);

  | Add_operator(o) =>
    let new_operators = add_operator(o, s.operators);
    let new_s = {
      operators: new_operators,
      balance_storage: s.balance_storage,
    };
    ([]: list(operation), new_s);

  | Remove_operator(o) =>
    let new_operators = remove_operator(o, s.operators);
    let new_s = {
      operators: new_operators,
      balance_storage: s.balance_storage,
    };
    ([]: list(operation), new_s);

  | Is_operator(p) =>
    let op = is_operator(p, s.operators);
    ([op], s);
  };
