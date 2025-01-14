/*
   This is stub implementation of `multi_token_receiver` interface which
   accepts transfer of any token.
 */

#include "../multi_token_interface.religo"

type receiver =
  | Multi_token_receiver(multi_token_receiver)
  | Foo(unit);

/*
   This receiver implementation is WIP and may result in locked tokens.
   The implementation needs to include entry points to either initiate tokens
   transfer from this contract and/or add operators who can transfer tokens on
   behalf of this contract.
   Adding additional non `multi_token_receiver` entry points are pending on
   LIGO support for multi entry points
 */
let receiver_stub = (p: receiver, s: unit): (list(operation), unit) => (
  []: list(operation),
  unit
);
