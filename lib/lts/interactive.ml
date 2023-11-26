module type INT = sig
  (* To be instantiated *)
  module IntLang : Lang.Interactive.LANG
  module Name : Lang.Names.CONT_NAMES with type name = IntLang.Name.name
  module Actions : Actions.ACTIONS with type Moves.name = IntLang.Name.name
  (* *)

  type interactive_ctx

  val replace_namectxP :
    interactive_ctx -> IntLang.name_ctx -> interactive_ctx

  val replace_storectx :
    interactive_ctx -> IntLang.Store.store_ctx -> interactive_ctx

  val init_interactive_ctx :
    IntLang.Store.store_ctx ->
    IntLang.name_ctx ->
    IntLang.name_ctx ->
    interactive_ctx

  val string_of_interactive_ctx : interactive_ctx -> string

  (* generate_output_move Δ nf returns a triple (m,γ,Γ,Δ')
      where the output move m is formed by
      the abstracted normal form a_nf build from nf,
      such that a_nf{γ} = nf and Δ ⊢ γ:Γ and Δ' = Γ *_P Δ.
      We do not take into account disclosure of locations
      currently.
      We return Γ to be able to reset the P-name component of Δ'
     in the POGS LTS. *)
  val generate_output_move :
    interactive_ctx ->
    IntLang.normal_form ->
    Actions.Moves.move
    * IntLang.interactive_env
    * IntLang.name_ctx
    * interactive_ctx

  (* generate_input_move Δ return
      all the pairs (m,Δ') such that
      there exists a name context Γ for the free names of m such that
      Δ ⊢ m ▷ Γ  and Δ' = Γ *_O Δ.
     It uses the branching monad from IntLang.M to do so. *)
  val generate_input_moves :
    interactive_ctx -> (Actions.Moves.move * interactive_ctx) IntLang.M.m

  (* check_input_move Δ m return Some Δ'
     when there exists a name context Γ for the free names of m such that
      Δ ⊢ m ▷ Γ and Δ' = Γ *_O Δ.
     It returns None when m is not well-typed.*)
  val check_input_move :
    interactive_ctx -> Actions.Moves.move -> interactive_ctx option

  (* trigger_computation γ m returns (t,μ,γ).
     We might return an interactive environment γ' distinct of γ
     once taking into account linear resources.*)
  val trigger_computation :
    IntLang.interactive_env ->
    Actions.Moves.move ->
    IntLang.computation * IntLang.Store.store * IntLang.interactive_env
end

module type INT_F = functor
  (IntLang : Lang.Interactive.LANG)
  (Moves : Moves.MOVES with type name = IntLang.Name.name)
  -> sig
  include INT with module IntLang = IntLang and module Actions.Moves = Moves
end

module Make (IntLang : Lang.Interactive.LANG) :
  INT
    with type IntLang.Name.name = IntLang.Name.name
     and type Actions.Moves.name = IntLang.Name.name = struct
  module IntLang = IntLang
  module Name = IntLang.Name
  module Actions = Actions.Make (IntLang)
  open Actions

  type interactive_ctx = {
    storectx: IntLang.Store.store_ctx;
    namectxP: IntLang.name_ctx;
    namectxO: IntLang.name_ctx;
  }

  let string_of_interactive_ctx ictx =
    "<"
    ^ IntLang.string_of_name_ctx ictx.namectxO
    ^ " | "
    ^ IntLang.string_of_name_ctx ictx.namectxP
    ^ ">"

  let replace_namectxP ictx namectxP = { ictx with namectxP }
  let replace_storectx ictx storectx = { ictx with storectx }

  let init_interactive_ctx storectx namectxP namectxO =
    { storectx; namectxP; namectxO }

  let generate_output_move ictx nf =
    match IntLang.abstracting_kind nf ictx.namectxO ictx.storectx with
    | Some (a_nf, ienv, lnamectx) ->
      let namectxP = IntLang.concat_name_ctx lnamectx ictx.namectxP in
        (Moves.build (Moves.Output, a_nf), ienv, lnamectx, { ictx with namectxP })
    | None ->
        failwith
          ("Error: the normal form " ^ IntLang.string_of_nf nf
         ^ " is not typeable in the name context "
          ^ IntLang.string_of_name_ctx ictx.namectxO
          ^ ". Please report.")

  open IntLang.M

  let generate_input_moves ictx =
    Util.Debug.print_debug "Generating O-moves";
    let* (a_nf, lnamectx) = IntLang.generate_a_nf ictx.storectx ictx.namectxP in
    let namectxO = IntLang.concat_name_ctx lnamectx ictx.namectxO in
    return (Moves.build (Moves.Input, a_nf), { ictx with namectxO })

  let check_input_move ictx move =
    match Moves.get_direction move with
    | Moves.Output -> None
    | Moves.Input -> begin
        let a_nf = Moves.get_kdata move in
        let lnamectx_opt =
          IntLang.type_check_a_nf ictx.namectxP ictx.namectxO a_nf in
        begin
          match lnamectx_opt with
          | Some lnamectx ->
            let namectxO = IntLang.concat_name_ctx lnamectx ictx.namectxO in
              Some { ictx with namectxO }
          | None -> None
        end
      end
    | Moves.None -> None

  let trigger_computation ienv input_move =
    let kdata = Moves.get_kdata input_move in
    match
      (Moves.get_direction input_move, IntLang.concretize_a_nf ienv kdata)
    with
    | (Moves.Input, (comp, store, ienv')) -> (comp, store, ienv')
    | _ ->
        failwith
          ("Error: the move "
          ^ Moves.string_of_move input_move
          ^ " is not an Opponent move. Please report.")
end
