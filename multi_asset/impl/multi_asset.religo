/*
   `multi_asset` contract combines `multi_token` transfer API with
   `simple_admin` API.  Input parameter type for the `multi_asset`
   contract is a union of `multi_token` and `simple_admin` parameter types.
   Depending on the input, `multi_asset` dispatches call to either
   `multi_token` or `simple_admin` entry points.
   If contract is paused, `multi_token` entry points cannot be invoked.
 */

#include "simple_admin.religo"

type multi_asset_storage = {
  admin: simple_admin_storage,
  assets: multi_token_storage,
};

type multi_asset_param =
  | Assets(multi_token)
  | Admin(simple_admin);

let multi_asset_main =
    (param: multi_asset_param, s: multi_asset_storage)
    : (list(operation), multi_asset_storage) =>
  switch (param) {
  | Admin(p) =>
    let ctx = {
      admin_storage: s.admin,
      balance_storage: s.assets.balance_storage,
    };

    let ops_ctx = simple_admin(p, ctx);

    let new_ctx = ops_ctx[1];
    let new_s = {
      admin: new_ctx.admin_storage,
      assets: {
        operators: s.assets.operators,
        balances: new_ctx.balance_storage,
      },
    };
    (ops_ctx[0], s);

  | Assets(p) =>
    if (s.admin.paused) {
      (
        failwith("contract is paused"): (
          list(operation),
          multi_asset_storage
        )
      );
    } else {
      let ops_assets = multi_token_main(p, s.assets);
      let new_s = {admin: s.admin, assets: ops_assets[1]};
      (ops_assets[0], new_s);
    }
  };
