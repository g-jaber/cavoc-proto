# How to play CAVOC

You play the role of a client of the module shown on screen. The module and its
signature are fixed; you choose how to interact with it.

**Your goal:** trigger a `failwith` or an assertion failure.

## Rules

1. **Read** the two panels: the module code, and the signature it exposes.
2. **Evaluate**: click the "Evaluate" button to start the interaction.
3. **Play**: select one of the available moves, then click "Select".

Repeat step 3 until the module fails, or click "Stop" to leave the game.

## Panels

* **Configuration** shows the current state of the interaction.
* **IEnv** shows the values the module has handed to you so far, which are the
  ones you may use in your next move.
* **Store** shows the contents of the memory.
* **History** shows the moves played so far.
* **Console** shows messages from the evaluator.
