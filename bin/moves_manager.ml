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

let previous_moves : string list ref = ref []

let display_previous_moves () : unit =
  let moves_string = String.concat " ; " !previous_moves in
  let move_display = Dom_html.getElementById "history" in
  Js.Unsafe.set move_display "textContent" (Js.string moves_string)

let add_move move =
  previous_moves := !previous_moves @ [ move ];
  display_previous_moves ()

let flush_moves () =
  previous_moves := [];
  display_previous_moves ()

let clear_list () : unit =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string ""
