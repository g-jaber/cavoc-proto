(* =====================================================
   SYNTHESIS_DRIVER: the free-play interaction loop
   =====================================================
   Drives a play in the typing LTS alone, both participants being synthesized
   from the play itself (doc/web.md, the synthesis page). *)
(* There is no program to consult, so the user is asked at every position, on
   either side. *)

module Drive (Arena : Lts_kind.SINGLE_RESULT_ARENA) = struct
  module TypingLTS = Arena.TypingLTS
  module Moves = TypingLTS.Moves

  (* A move's free names live in the context of the participant it is
     addressed to. *)
  let show_name_at position (direction, _) =
    Moves.Renaming.Namectx.show_name_in
      (match direction with
      | Moves.Input -> TypingLTS.get_namectxP position
      | Moves.Output -> TypingLTS.get_namectxO position)

  let player_of (direction, _) =
    match direction with
    | Moves.Input -> Lts.Interactive_build.Opponent
    | Moves.Output -> Lts.Interactive_build.Proponent

  (* The direction is the one of the moves to offer, the arena starting
     passive. *)
  let rec drive ~show_moves_list ~get_move ~show_play ~arena position direction
      played =
    let open Lts.Interactive_build in
    (* The arena has no configuration beyond its position: it is what the
       Configuration tab and the gutter's readout show. *)
    let position_json = TypingLTS.position_to_yojson position in
    Display_config.set_config_editor_text
      (Yojson.Safe.pretty_to_string position_json);
    Display_config.display_composite_position position_json;
    match Arena.offered_moves ~arena position direction played with
    | [] -> Lwt.return Prop_stopped
    | generated -> (
        let json_list =
          List.map
            (fun (move, weakening, _) ->
              Moves.pol_move_to_yojson_in
                ~show_name:(show_name_at position move)
                weakening move)
            generated in
        show_moves_list json_list;
        Moves_display.set_caption
          (match direction with
          | Moves.Input -> "Your move, as the client — click to play"
          | Moves.Output -> "Your move, as the module — click to play");
        let%lwt chosen = get_move (List.length json_list - 1) in
        match chosen with
        | Quit -> Lwt.return User_quit
        | Chose chosen_index ->
            let (move, weakening, target) = List.nth generated chosen_index in
            Moves_manager.add_move (player_of move)
              (Moves.string_of_pol_move_in
                 ~show_name:(show_name_at position move)
                 weakening move);
            let played = played @ [ move ] in
            show_play played;
            let direction =
              match direction with
              | Moves.Input -> Moves.Output
              | Moves.Output -> Moves.Input in
            drive ~show_moves_list ~get_move ~show_play ~arena target direction
              played)
end
