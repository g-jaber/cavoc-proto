Smoke tests for the sequential open composition, driving explore_cli over a
provider (compose/provider.ml, `let f x = x + 1`, with compose/provider.mli as
the shared interface) and its client (compose/client.ml, `let g y = f (f y)`,
exporting g through compose/client.mli).

The provider's interface is internal to the composition: the Opponent is
offered the client's public name alone, never the provider's f.

  $ printf 'no\nexit\n' | cavoc-explore_cli -compose compose/provider.ml compose/provider.mli compose/client.ml compose/client.mli | grep -E '^[0-9]+: '
  1: g(0,c0)
  2: g(1,c0)
  3: g(2,c0)

Calling g runs the client, whose two calls to f are forwarded across the shared
interface to the provider and answered there. Those synchronizations are
internal, so the composite shows only the external answer: g y = f (f y) = y + 2.

  $ printf 'no\n1\nno\nexit\n' | cavoc-explore_cli -compose compose/provider.ml compose/provider.mli compose/client.ml compose/client.mli | grep -oE '[OP]: .*'
  O: g?(0,c0)
  P: c0!(2)

  $ printf 'no\n2\nno\nexit\n' | cavoc-explore_cli -compose compose/provider.ml compose/provider.mli compose/client.ml compose/client.mli | grep -oE '[OP]: .*'
  O: g?(1,c0)
  P: c0!(3)

The vending machine over its till (vending-till/): the credit cell lives in
the till, so every guard of the machine is a call across the shared
interface. With credit 2 a purchase succeeds — covers 1 holds, sub 2 leaves
0, solvent — and a second one is refused by covers and succeeds vacuously.

  $ printf 'no\n3\nno\n4\nno\n4\nno\nexit\n' | cavoc-explore_cli -compose vending-till/till.ml vending-till/till.mli vending-till/vending.ml vending-till/vending.mli | grep -oE '[OP]: .*'
  O: insert?(2,c0)
  P: c0!(())
  O: buy?((),c1)
  P: c1!(())
  O: buy?((),c2)
  P: c2!(())

With credit 1 the miscounted price fires the assertion: covers 1 holds but
sub 2 overdraws the till, solvent fails and the machine halts — buy gets no
answer.

  $ printf 'no\n2\nno\n4\nno\nexit\n' | cavoc-explore_cli -compose vending-till/till.ml vending-till/till.mli vending-till/vending.ml vending-till/vending.mli | grep -oE '[OP]: .*'
  O: insert?(1,c0)
  P: c0!(())
  O: buy?((),c1)

The overview example of the paper (twice-iter2/): the library exports
`twice f x = f (f x)` and its client specializes it as
`iter2 f n = twice (fun x -> f (x + 1)) n`. The composite offers iter2
alone, twice being internal, and currying splits the client's opening
call in two: iter2 answers with the partial application as the fresh
name f1.

  $ printf 'no\nexit\n' | cavoc-explore_cli -compose twice-iter2/twice.ml twice-iter2/twice.mli twice-iter2/iter2.ml twice-iter2/iter2.mli | grep -E '^[0-9]+: '
  1: iter2(f0,c0)

Seeding f1 with 2 sets off the feedback loop: each hidden call of twice to
its argument fun x -> f0 (x + 1) makes the composite call back the
client's f0, so visible actions interleave with the internal
synchronizations rather than waiting for them to finish.

  $ printf 'no\n1\nno\n4\nno\n6\nno\n7\nno\nexit\n' | cavoc-explore_cli -compose twice-iter2/twice.ml twice-iter2/twice.mli twice-iter2/iter2.ml twice-iter2/iter2.mli | grep -oE '[OP]: .*'
  O: iter2?(f0,c0)
  P: c0!(f1)
  O: f1?(2,c1)
  P: f0!(3,c0)
  O: c0?(1)
  P: f0!(2,c1)
  O: c1?(2)
  P: c1!(2)

Composition links two modules through the signature of the first, which a
program does not have, so the two cannot be combined.

  $ cavoc-explore_cli -compose -program compose/provider.ml compose/provider.mli compose/client.ml compose/client.mli
  Error: composition applies to modules, not programs: it links them through the signature of the first, which a program does not have.
