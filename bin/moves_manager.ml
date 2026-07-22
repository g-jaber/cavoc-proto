(* ====================================================
   MOVES_MANAGER: Interactive evaluation move tracking
   ====================================================
   Tracks and displays the history of execution steps:
   - previous_moves: List of moves chosen by the user
   - add_move: Records a new move and updates display
   - flush_moves: Clears all moves for next evaluation
   - display_previous_moves: Updates the UI with move history
*)

open Js_of_ocaml

(* Kept in reverse chronological order, so that recording a move stays O(1). *)
let previous_moves : string list ref = ref []

let display_previous_moves () : unit =
  let moves_string = String.concat " ; " (List.rev !previous_moves) in
  match Dom_html.getElementById_opt "history" with
  | None -> ()
  | Some move_display ->
      move_display##.textContent := Js.some (Js.string moves_string)

let add_move move =
  previous_moves := move :: !previous_moves;
  display_previous_moves ()

let flush_moves () =
  previous_moves := [];
  display_previous_moves ()

let clear_list () : unit =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string ""
