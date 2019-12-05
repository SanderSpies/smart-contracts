#include "../multi_token_interface.religo"

type state = {
  owner: address,
  token_id: nat,
  balance: nat,
};

type query_param = {
  mac: address,
  token_id: nat,
  owner: address,
};

type param =
  | Query(query_param)
  | Response(list((balance_request, nat)))
  | Default(unit);

let main = (p: param, s: option(state)): (list(operation), option(state)) =>
  switch (p) {
  | Query(q) =>
    let br: balance_request = {owner: q.owner, token_id: q.token_id};
    let bp: balance_of_param = {
      balance_request: [br],
      balance_view: (
        Operation.get_entrypoint("%response", Current.self_address):
          contract(list((balance_request, nat)))
      ),
    };
    let mac: contract(balance_of_param) =
      Operation.get_entrypoint("%balance_of", q.mac);
    let q_op = Operation.transaction(bp, 0mutez, mac);
    ([q_op], s);

  | Response(r) =>
    let new_s =
      switch (r) {
      | [b, ...tl] => {
          owner: b[0].owner,
          token_id: b[0].token_id,
          balance: b[1],
        }
      | [] => (failwith("invalid response"): state)
      };

    ([]: list(operation), Some(new_s));

  | Default(u) => ([]: list(operation), s)
  };
