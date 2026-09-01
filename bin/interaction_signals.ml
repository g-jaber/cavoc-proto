(* ==================================================
   INTERACTION_SIGNALS: Control-flow types for the UI
   ==================================================
   Describes what the user did with the choice panel.

   This is the page's own vocabulary: it also covers the cases where the page
   is not in the state we expect. Evaluate_code translates it into the
   vocabulary of the interaction loop, Lts.Interactive_build.user_action.
*)

(* Outcome of asking the user to pick one of the rows of the choice panel. *)
type move_choice =
  | Chosen of int (* id of the selected row *)
  | Interrupted (* the user pressed "Stop" *)
  | Missing_buttons (* the page does not provide the choice panel *)

let string_of_move_choice = function
  | Chosen i -> "move " ^ string_of_int i
  | Interrupted -> "interaction interrupted by the user"
  | Missing_buttons -> "the page is missing the choice panel"
