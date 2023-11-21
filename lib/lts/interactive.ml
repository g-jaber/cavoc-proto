module type INT = sig
  (* To be instantiated *)
  module IntLang : Lang.Interactive.LANG

  module Actions :
    Actions.ACTIONS
      with type Moves.name = IntLang.name
       and type Moves.kind = IntLang.kind_interact
  (* *)

  val generate_output_move :
    IntLang.name_type_ctx ->
    IntLang.kind_interact * IntLang.glue_val ->
    Actions.Moves.move * IntLang.interactive_env * IntLang.name_type_ctx

  val generate_input_moves :
    IntLang.name_type_ctx ->
    IntLang.name_type_ctx ->
    (Actions.Moves.move * IntLang.name_type_ctx * IntLang.name_type_ctx)
    IntLang.M.m

  val check_input_move :
    IntLang.name_type_ctx ->
    IntLang.name_type_ctx ->
    Actions.Moves.move ->
    (IntLang.name_type_ctx * IntLang.name_type_ctx) option

  val trigger_computation :
    IntLang.interactive_env ->
    Actions.Moves.move ->
    IntLang.computation * IntLang.interactive_env
end

module type INT_F = functor
  (IntLang : Lang.Interactive.LANG)
  (Moves : Moves.MOVES
             with type name = IntLang.name
              and type kind = IntLang.kind_interact)
  -> sig
  include INT with module IntLang = IntLang and module Actions.Moves = Moves
end

module Make (CpsLang : Lang.Cps.INTLANG) :
  INT
    with type IntLang.name = CpsLang.name
     and type Actions.Moves.name = CpsLang.name = struct
  module IntLang = CpsLang
  module Actions = Actions.Make (CpsLang)
  open Actions

  let generate_output_move namectxO (kind, gval) =
    match CpsLang.abstract_kind (kind, gval) namectxO with
    | Some (a_nf, ienv, namectxP) ->
        (Moves.build (Moves.Output, kind, a_nf), ienv, namectxP)
    | None ->
        failwith
          ("Error: the interaction kind "
          ^ CpsLang.string_of_kind_interact kind
          ^ " is not typeable in the name context "
          ^ CpsLang.string_of_name_type_ctx namectxO
          ^ ". Please report.")

  open IntLang.M

  let generate_input_moves namectxP namectxO =
    Util.Debug.print_debug "Generating O-moves";
    let* ((kind, aval), lnamectx) = IntLang.generate_a_nf namectxP in
    let namectxO' = IntLang.concat_name_type_ctx lnamectx namectxO in
    return (Moves.build (Moves.Input, kind, aval), lnamectx, namectxO')

  let check_input_move namectxP namectxO move =
    match Moves.get_direction move with
    | Moves.Output -> None
    | Moves.Input -> begin
        let kind = Moves.get_kind move in
        let aval = Moves.get_data move in
        let lnamectx_opt =
          CpsLang.type_check_a_nf namectxP namectxO (kind, aval) in
        begin
          match lnamectx_opt with
          | Some lnamectx ->
              let namectxO' = IntLang.concat_name_type_ctx lnamectx namectxO in
              Some (lnamectx, namectxO')
          | None -> None
        end
      end
    | Moves.None -> None

  let trigger_computation ienv input_move =
    let kind = Moves.get_kind input_move in
    let aval = Moves.get_data input_move in
    match (Moves.get_direction input_move, CpsLang.trigger_ienv ienv kind) with
    | (Moves.Input, Some ivalue) ->
        (CpsLang.val_composition ienv ivalue aval, ienv)
    | (Moves.Input, None) ->
        failwith
          ("Error: the move "
          ^ Moves.string_of_move input_move
          ^ " is ill-formed wrt the environment "
          ^ CpsLang.string_of_interactive_env ienv
          ^ ". Please report.")
    | _ ->
        failwith
          ("Error: the move "
          ^ Moves.string_of_move input_move
          ^ " is not an Opponent move. Please report.")
end
