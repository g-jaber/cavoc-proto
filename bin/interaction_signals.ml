(* ==================================================
   INTERACTION_SIGNALS: Control-flow types for the UI
   ==================================================
   Describes what the user did with the move-selection widgets.

   This is the page's own vocabulary: it also covers the cases where the page
   is not in the state we expect. Evaluate_code translates it into the
   vocabulary of the interaction loop, Lts.Interactive_build.user_action.
*)

(* Outcome of asking the user to pick a move in the moves list. *)
type move_choice =
  | Chosen of int (* index of the selected move, within the moves list *)
  | Interrupted (* the user pressed "Stop" or "Load" *)
  | No_selection (* the moves list has no radio button checked *)
  | Missing_buttons (* the page does not provide the expected buttons *)

let string_of_move_choice = function
  | Chosen i -> "move " ^ string_of_int i
  | Interrupted -> "interaction interrupted by the user"
  | No_selection -> "no move selected"
  | Missing_buttons -> "the page is missing the move-selection buttons"
