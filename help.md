# How to play CAVOC

You play the role of a client of the module shown on screen. The module and its
signature are fixed; you choose how to interact with it.

**Your goal:** trigger a `failwith` or an assertion failure.

## Rules

1. **Read** the module's card: its code, and the signature it exposes.
2. **Start**: click the "Start" button to begin the interaction.
3. **Play**: click one of the available moves (or focus it with the arrow
   keys and press Enter). Focusing a move highlights the value it addresses
   in the card's interactive environment. If the keyboard has wandered
   elsewhere on the page, click anywhere on the moves panel — not on a move
   — to get it back without playing.

Repeat step 3 until the module fails, or click "Stop" to leave the game.

## The card's live panels

During a run the module's card shows two live panels, which are the state of
the interaction seen from that module:

* **interactive environment** lists the values the module has handed to you
  so far — the ones you may use in your next move — and counts how many the
  last move added. It starts unfolded on the full list.
* **store** counts the module's memory locations, and how many the last
  move changed. It starts folded to its summary strip.

Click a panel's strip to cycle it: folded strip, what the last move
changed, the full state. Each entry expands individually from its one-line
summary to its full text.

## Below the splitter

* **History**, under the card, always shows the moves played so far.
* Under the moves panel, three folded tabs: **Configuration** shows the raw
  state of the interaction, as JSON; **Console** shows messages from the
  evaluator; **Client** shows a program of the language that plays exactly
  the moves you have played, synthesized afresh after each of your moves —
  the module's signature it is written against, then the client itself.
  Click a tab to open it, click it again to fold it away.

The client is only available for the CPS setting, and only for plays inside
the definable fragment: the tab says why when there is no client to show —
a module disclosing its memory, raising an exception or exporting an
abstract type takes the interaction out of it.

## The synthesis page

A synthesis scenario gives only a signature, and no program at all: you play
every move, of both participants, and the two cards show the module and the
client your play defines, rewritten after each move. The choice panel says
whose move you are taking, and offers only the moves that keep the play
definable — so the pair of programs always exists. The LTS options are fixed
there: the concrete CPS stack, with visibility and well-bracketing on.
