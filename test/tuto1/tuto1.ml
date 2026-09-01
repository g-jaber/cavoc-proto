(*
Your goal is to trigger a failwith, by making an assertion fail.
In this first example the assertion fails on its own: x holds 0, while the
assertion demands that !x = 1.

The interaction starts on its own; click the move you are offered to play
it (or press Enter).
The "Store" panel then shows that x is the location lx, and that lx holds 0.
Focus the second move in the list on the left: the "IEnv" panel highlights
"∙; assert (!x = 1)", which is the code that will run next, checking whether
!x = 1.
Play it, and the assertion fails. You win.
*)
let x = ref 0
let g f = f(); assert (!x = 1)
