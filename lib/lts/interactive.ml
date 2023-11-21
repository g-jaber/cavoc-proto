module type INT = sig
  (* To be instantiated *)
  module IntLang : Lang.Interactive.LANG

  module Actions :
    Actions.ACTIONS
      with type Moves.name = IntLang.name
  (* *)

  val generate_output_move :
    IntLang.name_type_ctx ->
    IntLang.normal_form ->
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
             with type name = IntLang.name)
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

  let generate_output_move namectxO nf =
    match CpsLang.abstract_kind nf namectxO with
    | Some (a_nf, ienv, namectxP) ->
        (Moves.build (Moves.Output, a_nf), ienv, namectxP)
    | None ->
        failwith
          ("Error: the normal form "
          ^ CpsLang.string_of_nf nf
          ^ " is not typeable in the name context "
          ^ CpsLang.string_of_name_type_ctx namectxO
          ^ ". Please report.")

  open IntLang.M

  let generate_input_moves namectxP namectxO =
    Util.Debug.print_debug "Generating O-moves";
    let* (a_nf, lnamectx) = IntLang.generate_a_nf namectxP in
    let namectxO' = IntLang.concat_name_type_ctx lnamectx namectxO in
    return (Moves.build (Moves.Input, a_nf), lnamectx, namectxO')

  let check_input_move namectxP namectxO move =
    match Moves.get_direction move with
    | Moves.Output -> None
    | Moves.Input -> begin
        let a_nf = Moves.get_kdata move in
        let lnamectx_opt =
          CpsLang.type_check_a_nf namectxP namectxO a_nf in
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
    let (kdata) = Moves.get_kdata input_move in
    match (Moves.get_direction input_move, CpsLang.concretize_a_nf ienv kdata) with
    | (Moves.Input, Some (comp,ienv')) ->
        (comp, ienv')
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
