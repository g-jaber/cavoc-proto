(*
Your goal is to trigger a failwith.
To achieve this, you must disprove an assumption.
In this simple example, it will trigger itself. Here, x = 0, and the assumption is x = 1.
You just have to start playing by clicking the "Evaluate" button.
Then, click the "select button". It will start the game.
In the "store" panel, you can see that x = lx and lx = 0.
Select the 2nd line on the left. In the "ienv" panel, it will color "∙; assert (!x = 1)".
It means that it will play this line: checking if x = 1 or not.
You won !
*)
let x = ref 0
let g f = f(); assert (!x = 1)
