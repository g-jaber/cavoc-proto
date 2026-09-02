(* ====================================================
   MOVES_MANAGER: Interactive evaluation move tracking
   ====================================================
   Tracks and displays the history of execution steps:
   - previous_moves: List of moves chosen by the user
   - add_move: Records a new move and updates display
   - flush_moves: Clears all moves for next evaluation
   - display_previous_moves: Updates the UI with move history

   The History window shows the moves by row: one flat public lane on the
   single-module page, a public and a shared row on a composition's
   (doc/web.md).
*)

open Js_of_ocaml

(* Whether a move is public or shared, i.e. which row its chip lands on. *)
type lane = Public | Shared

(* Reverse chronological order, so recording a move stays O(1). *)
let previous_moves : (lane * string * string) list ref = ref []

let chip_html chip_class move =
  Printf.sprintf "<span class=\"trace-move %s\">%s</span>" chip_class
    (Ui_helpers.html_escape move)

let flat_public_html () =
  !previous_moves
  |> List.filter_map (fun (lane, chip_class, move) ->
      if lane = Public then Some (chip_html chip_class move) else None)
  |> List.rev |> String.concat ""

(* The trace grid of the compose page: each move takes its own column of the
   shared chronology, on its row, public or shared. *)
let interleaved_grid_html () =
  let label row text =
    Printf.sprintf
      "<span class=\"trace-grid-label\" style=\"grid-row:%d\">%s</span>" row
      text in
  let chip column (lane, chip_class, move) =
    let row = match lane with Public -> 1 | Shared -> 2 in
    Printf.sprintf
      "<span class=\"trace-move %s\" \
       style=\"grid-row:%d;grid-column:%d\">%s</span>"
      chip_class row (column + 2)
      (Ui_helpers.html_escape move) in
  label 1 "public" ^ label 2 "shared"
  ^ (!previous_moves |> List.rev |> List.mapi chip |> String.concat "")

let display_previous_moves () : unit =
  (match Dom_html.getElementById_opt "trace-lane-public-chips" with
  | None -> ()
  | Some chips -> chips##.innerHTML := Js.string (flat_public_html ()));
  match Dom_html.getElementById_opt "trace-grid" with
  | None -> ()
  | Some grid ->
      grid##.innerHTML := Js.string (interleaved_grid_html ());
      (* Keep the newest move in view. *)
      grid##.scrollLeft := Js.number_of_float (float_of_int grid##.scrollWidth)

let add_move player move =
  let chip_class =
    match player with
    | Lts.Interactive_build.Opponent -> "trace-o"
    | Lts.Interactive_build.Proponent -> "trace-p" in
  previous_moves := (Public, chip_class, move) :: !previous_moves;
  display_previous_moves ()

(* A synchronization of the composition: a chip on the shared row. *)
let add_sync_move move =
  previous_moves := (Shared, "trace-tau", move) :: !previous_moves;
  display_previous_moves ()

let flush_moves () =
  previous_moves := [];
  display_previous_moves ()

let clear_list () : unit =
  let moves_list = Dom_html.getElementById "moves-list" in
  moves_list##.innerHTML := Js.string ""
