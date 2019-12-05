/*
   One of the possible implementations of admin API for `multi_token` contract.

   Only current `admin` of the contract can invoke admin API.
   Admin API allows to

     1. Change administrator,
     2. Create new toke types,
     3. Mint and burn tokens to some existing or new owner account,
     4. pause the contract.

   Mint operation performs safety check as specified for `multi_token`
   transfer entry points. Burn operation fails if the owner holds
   less tokens then burn amount.
 */

#include "multi_token_impl.religo"

type create_token_param = {
  token_id: nat,
  descriptor: string,
};

type mint_tokens_param = {
  owner: address,
  batch: list(tx),
  data: bytes,
};

type burn_tokens_param = {
  owner: address,
  batch: list(tx),
};

/* `simple_admin` entry points */
type simple_admin =
  | Set_admin(address)
  | Pause(bool)
  | Create_token(create_token_param)
  | Mint_tokens(mint_tokens_param)
  | Burn_tokens(burn_tokens_param)
  /*
     Adds implicit account to the white list to be able to receive tokens
   */
  | Add_implicit_owners(list(key_hash))
  /*
     Removes implicit account from the white list. Not whitelisted implicit accounts
     cannot receive tokens. All existing account token balances if any, will remain
     unchanged. It is still possible to transfer tokens from not whitelisted
     implicit account
   */
  | Remove_implicit_owners(list(key_hash));

type simple_admin_storage = {
  admin: address,
  paused: bool,
  /* token_id -> descriptor */
  tokens: big_map(nat, string),
};

type simple_admin_context = {
  admin_storage: simple_admin_storage,
  balance_storage,
};

let set_admin =
    (new_admin: address, s: simple_admin_storage): simple_admin_storage => {
  admin: new_admin,
  paused: s.paused,
  tokens: s.tokens,
};

let pause = (paused: bool, s: simple_admin_storage): simple_admin_storage => {
  admin: s.admin,
  paused,
  tokens: s.tokens,
};

let create_token =
    (param: create_token_param, s: simple_admin_storage): simple_admin_storage => {
  let token: option(string) = Map.find_opt(param.token_id, s.tokens);
  switch (token) {
  | Some(d) => (failwith("token already exists"): simple_admin_storage)
  | None =>
    let new_tokens = Map.add(param.token_id, param.descriptor, s.tokens);
    {admin: s.admin, paused: s.paused, tokens: new_tokens};
  };
};

let token_exists = (token_id: nat, tokens: big_map(nat, string)): unit => {
  let d = Map.find_opt(token_id, tokens);
  switch (d) {
  | None => failwith("token does not exist")
  | Some(d) => unit
  };
};

let mint_tokens_impl =
    (
      param: mint_tokens_param,
      owner_id: nat,
      tokens: big_map(nat, string),
      b: balances
    )
    : balances => {
  let make_transfer = (bals: balances, t: tx) => {
    let u: unit = token_exists(t.token_id, tokens);
    let to_key = make_balance_key_impl(owner_id, t.token_id);
    let old_bal = get_balance(to_key, bals);
    Map.update(to_key, Some(old_bal + t.amount), bals);
  };

  List.fold(make_transfer, param.batch, b);
};

let mint_safe_check =
    (param: mint_tokens_param, is_owner_implicit: bool): list(operation) =>
  if (is_owner_implicit) {
    ([]: list(operation));
  } else {
    let receiver: contract(multi_token_receiver) =
      Operation.get_entrypoint("%multi_token_receiver", param.owner);
    let p: on_multi_tokens_received_param = {
      operator: sender,
      from_: (None: option(address)),
      batch: param.batch,
      data: param.data,
    };
    let op =
      Operation.transaction(On_multi_tokens_received(p), 0mutez, receiver);
    [op];
  };

let mint_tokens =
    (param: mint_tokens_param, a: simple_admin_storage, b: balance_storage)
    : (list(operation), balance_storage) => {
  let owner = ensure_owner_id(param.owner, b.owners);
  let ops = mint_safe_check(param, owner.owner.is_implicit);
  let new_bals =
    mint_tokens_impl(param, owner.owner.id, a.tokens, b.balances);
  let new_s = {owners: owner.owners, balances: new_bals};
  (ops, new_s);
};

let burn_tokens =
    (param: burn_tokens_param, s: balance_storage): balance_storage => {
  let owner_id = get_owner_id(param.owner, s.owners);

  let make_burn = (bals: balances, t: tx) => {
    let from_key = make_balance_key_impl(owner_id, t.token_id);
    let old_bal =
      switch (Map.find_opt(from_key, bals)) {
      | Some(b) => b
      | None => 0n
      };

    switch (Michelson.is_nat(old_bal - t.amount)) {
    | None => (failwith("Insufficient funds"): balances)
    | Some(new_bal) =>
      if (new_bal == 0n) {
        Map.remove(from_key, bals);
      } else {
        Map.update(from_key, Some(new_bal), bals);
      }
    };
  };

  let new_bals = List.fold(make_burn, param.batch, s.balances);
  {owners: s.owners, balances: new_bals};
};

let get_implicit_address = (hash: key_hash): address => {
  let c: contract(unit) = Current.implicit_account(hash);
  Current.address(c);
};

let add_implicit_owners =
    (owner_hashes: list(key_hash), s: balance_storage): balance_storage => {
  let add_owner = (l: owner_lookup, h: key_hash) => {
    let owner = get_implicit_address(h);
    let entry = Map.find_opt(owner, l.owners);
    switch (entry) {
    | None =>
      let r = add_owner(owner, true, l);
      r.owners;
    | Some(o_e) =>
      if (o_e.is_implicit) {
        s.owners;
      } else {
        (
          failwith("originated owner with the same address already exists"): owner_lookup
        );
      }
    };
  };

  let new_lookup = List.fold(add_owner, owner_hashes, s.owners);
  {owners: new_lookup, balances: s.balances};
};

let remove_implicit_owners =
    (owner_hashes: list(key_hash), s: balance_storage): balance_storage => {
  let remove_owner = (l: owner_lookup, h: key_hash) => {
    let owner = get_implicit_address(h);
    let entry = Map.find_opt(owner, l.owners);
    switch (entry) {
    | None => l
    | Some(o_e) =>
      if (!o_e.is_implicit) {
        (failwith("trying to remove non-implicit account"): owner_lookup);
      } else {
        {
          owner_count: s.owners.owner_count,
          owners: Map.remove(owner, s.owners.owners),
        };
      }
    };
  };

  let new_lookup = List.fold(remove_owner, owner_hashes, s.owners);
  {owners: new_lookup, balances: s.balances};
};

let simple_admin =
    (param: simple_admin, ctx: simple_admin_context)
    : (list(operation), simple_admin_context) =>
  if (sender != ctx.admin_storage.admin) {
    (
      failwith("operation requires admin privileges"): (
        list(operation),
        simple_admin_context
      )
    );
  } else {
    switch (param) {
    | Set_admin(new_admin) =>
      let new_admin_s = set_admin(new_admin, ctx.admin_storage);
      let new_ctx = {
        admin_storage: new_admin_s,
        balance_storage: ctx.balance_storage,
      };
      ([]: list(operation), new_ctx);

    | Pause(paused) =>
      let new_admin_s = pause(paused, ctx.admin_storage);
      let new_ctx = {
        admin_storage: new_admin_s,
        balance_storage: ctx.balance_storage,
      };
      ([]: list(operation), new_ctx);

    | Create_token(param) =>
      let new_admin_s = create_token(param, ctx.admin_storage);
      let new_ctx = {
        admin_storage: new_admin_s,
        balance_storage: ctx.balance_storage,
      };
      ([]: list(operation), new_ctx);

    | Mint_tokens(param) =>
      let ops_new_bals =
        mint_tokens(param, ctx.admin_storage, ctx.balance_storage);
      let new_ctx: simple_admin_context = {
        admin_storage: ctx.admin_storage,
        balance_storage: ops_new_bals[1]
      };
      (ops_new_bals[0], new_ctx);

    | Burn_tokens(param) =>
      let new_bals = burn_tokens(param, ctx.balance_storage);
      let new_ctx = {
        admin_storage: ctx.admin_storage,
        balance_storage: new_bals,
      };
      ([]: list(operation), new_ctx);

    | Add_implicit_owners(hashes) =>
      let new_bals = add_implicit_owners(hashes, ctx.balance_storage);
      let new_ctx = {
        admin_storage: ctx.admin_storage,
        balance_storage: new_bals,
      };
      ([]: list(operation), new_ctx);

    | Remove_implicit_owners(hashes) =>
      let new_bals = remove_implicit_owners(hashes, ctx.balance_storage);
      let new_ctx = {
        admin_storage: ctx.admin_storage,
        balance_storage: new_bals,
      };
      ([]: list(operation), new_ctx);
    };
  };
