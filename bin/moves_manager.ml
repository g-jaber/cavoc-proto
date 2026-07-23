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

(* Reverse chronological order, so recording a move stays O(1). *)
let previous_moves : (Lts.Interactive_build.player * string) list ref = ref []

let chip_html (player, move) =
  let modifier =
    match player with
    | Lts.Interactive_build.Opponent -> "trace-o"
    | Lts.Interactive_build.Proponent -> "trace-p" in
  Printf.sprintf "<span class=\"trace-move %s\">%s</span>" modifier
    (Ui_helpers.html_escape move)

let display_previous_moves () : unit =
  let html =
    !previous_moves |> List.rev |> List.map chip_html |> String.concat "" in
  match Dom_html.getElementById_opt "history" with
  | None -> ()
  | Some move_display -> move_display##.innerHTML := Js.string html

let add_move player move =
  previous_moves := (player, move) :: !previous_moves;
  display_previous_moves ()

let flush_moves () =
  previous_moves := [];
  display_previous_moves ()

let clear_list () : unit =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string ""
