The reference scenario is test/7-no_visible.ml: g calls its argument f
twice, with two different callbacks. The trace under test makes Opponent
answer Proponent's first call to f, so that Proponent calls f a second
time; visibility must then prevent Opponent from calling the first
callback (it is no longer in its view), while allowing it without the
restriction.

CPS, without visibility: the first callback f1 can still be called after
the second call to f.

  $ printf 'no\n1\nno\n3\nno\nexit\n' | cavoc-explore_cli 7-no_visible.ml 7-no_visible.mli | tail -7
  (yes/no/exit) The possible moves are:
  1: g(f1,c1)
  2: f1((),c1)
  3: f2((),c1)
  4: c1(())
  Choose an integer between 1 and 4 to decide what to do, or type 'exit' to stop.
  (1..4/exit) 

CPS, with visibility: f1 is filtered out.

  $ printf 'no\n1\nno\n3\nno\nexit\n' | cavoc-explore_cli -vis 7-no_visible.ml 7-no_visible.mli | tail -6
  (yes/no/exit) The possible moves are:
  1: g(f1,c1)
  2: f2((),c1)
  3: c1(())
  Choose an integer between 1 and 3 to decide what to do, or type 'exit' to stop.
  (1..3/exit) 

Direct style, without visibility.

  $ printf 'no\n1\nno\n3\nno\nexit\n' | cavoc-explore_cli -no-cps 7-no_visible.ml 7-no_visible.mli | tail -7
  (yes/no/exit) The possible moves are:
  1: g(f1)
  2: f1(())
  3: f2(())
  4: ret(())
  Choose an integer between 1 and 4 to decide what to do, or type 'exit' to stop.
  (1..4/exit) 

Direct style, with visibility: f1 is filtered out.

  $ printf 'no\n1\nno\n3\nno\nexit\n' | cavoc-explore_cli -no-cps -vis 7-no_visible.ml 7-no_visible.mli | tail -6
  (yes/no/exit) The possible moves are:
  1: g(f1)
  2: f2(())
  3: ret(())
  Choose an integer between 1 and 3 to decide what to do, or type 'exit' to stop.
  (1..3/exit) 

Direct style, with visibility: calling the callback f1 and letting it
return must NOT remove f1 from the view (the view at the pending
question is restored when Proponent answers).

  $ printf 'no\n1\nno\n2\nno\nexit\n' | cavoc-explore_cli -no-cps -vis 7-no_visible.ml 7-no_visible.mli | tail -6
  (yes/no/exit) The possible moves are:
  1: g(f1)
  2: f1(())
  3: ret(())
  Choose an integer between 1 and 3 to decide what to do, or type 'exit' to stop.
  (1..3/exit) 

Direct style, with visibility, active start: Proponent's first move is a
toplevel answer, which must restore the initial view.

  $ printf 'no\n1\nno\nexit\n' | cavoc-explore_cli -no-cps -vis -program while.ml | tail -8
  Do you want to print the Proponent configuration?
  (yes/no/exit) The possible moves are:
  1: f0(f1)
  2: f1(0)
  3: f1(1)
  4: f1(2)
  Choose an integer between 1 and 4 to decide what to do, or type 'exit' to stop.
  (1..4/exit) 
