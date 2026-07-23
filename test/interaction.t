Smoke tests for the interactive exploration loop, driving explore_cli over
test/private_counter.ml (a counter behind an abstract type: get returns a fresh
token, check asserts a token is in range).

Each turn echoes the move that was played, tagged with who played it: O for
Opponent (the user's input) and P for Proponent (the module's answer). Playing
get twice makes the two sides alternate.

  $ printf 'no\n1\nno\n1\nno\nexit\n' | cavoc-explore_cli private_counter.ml private_counter.mli | grep -oE '[OP]: .*'
  O: get?((),c0)
  P: c0!(p0)
  O: get?((),c1)
  P: c1!(p1)

Each get hands back a token, and every token handed back so far becomes
checkable: after two gets, check is offered on both p0 and p1.

  $ printf 'no\n1\nno\n1\nno\nexit\n' | cavoc-explore_cli private_counter.ml private_counter.mli | grep -E '^[0-9]+: ' | tail -3
  1: get((),c2)
  2: check(p0,c2)
  3: check(p1,c2)

Typing exit at a prompt leaves the game: no move is played after it.

  $ printf 'no\nexit\n' | cavoc-explore_cli private_counter.ml private_counter.mli | grep -oE '[OP]: .*' | wc -l | tr -d ' '
  0

A non-integer answer is rejected and the same choice is asked again, rather than
crashing.

  $ printf 'no\nfoo\n1\nno\nexit\n' | cavoc-explore_cli private_counter.ml private_counter.mli | grep -o 'invalid integer'
  invalid integer
