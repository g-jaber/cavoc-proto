Smoke tests for the sequential open composition, driving explore_cli over a
module (compose/module.ml, `let f x = x + 1`, with compose/module.mli as
the shared interface) and its client (compose/client.ml, `let g y = f (f y)`,
exporting g through compose/client.mli).

The module's interface is internal to the composition: the Opponent is
offered the client's public name alone, never the module's f.

  $ printf 'no\nexit\n' | cavoc-explore_cli -compose compose/module.ml compose/module.mli compose/client.ml compose/client.mli | grep -E '^[0-9]+: '
  1: g(0,c0)
  2: g(1,c0)
  3: g(2,c0)

Calling g runs the client, whose two calls to f are forwarded across the shared
interface to the module and answered there. Those synchronizations are
internal, so the composite shows only the external answer: g y = f (f y) = y + 2.

  $ printf 'no\n1\nno\nexit\n' | cavoc-explore_cli -compose compose/module.ml compose/module.mli compose/client.ml compose/client.mli | grep -oE '[OP]: .*'
  O: g?(0,c0)
  P: c0!(2)

  $ printf 'no\n2\nno\nexit\n' | cavoc-explore_cli -compose compose/module.ml compose/module.mli compose/client.ml compose/client.mli | grep -oE '[OP]: .*'
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

A polymorphic import (polymorphic-compose/: the identity behind
val id : 'a -> 'a, and forward.ml's `let use x = id x` exported at the same
type). The Opponent instantiates use at a type name a0 of its own and sends
its argument as the fresh name p0 at it; the client instantiates id at a
type name of its own and boxes p0 behind it, the identity gives that box
back, and the client gives p0 back. The two synchronizations are internal,
so the composite shows the instantiation and the answer only; the type
names the client created for itself explain the gap in the numbering of
the second call.

  $ printf 'no\nexit\n' | cavoc-explore_cli -compose polymorphic-compose/identity.ml polymorphic-compose/identity.mli polymorphic-compose/forward.ml polymorphic-compose/forward.mli | grep -E '^[0-9]+: '
  1: use(a0,p0,c0)

  $ printf 'no\n1\nno\n1\nno\nexit\n' | cavoc-explore_cli -compose polymorphic-compose/identity.ml polymorphic-compose/identity.mli polymorphic-compose/forward.ml polymorphic-compose/forward.mli | grep -oE '[OP]: .*'
  O: use?(a0,p0,c0)
  P: c0!(p0)
  O: use?(a2,p1,c1)
  P: c1!(p1)

Composition links two modules through the signature of the first, which a
program does not have, so the two cannot be combined.

  $ cavoc-explore_cli -compose -program compose/module.ml compose/module.mli compose/client.ml compose/client.mli
  Error: composition applies to modules, not programs: it links them through the signature of the first, which a program does not have.
