module type GAME = sig
  module Moves : Moves.MOVES
  module BranchMonad : Util.Monad.BRANCH

  type name_ctx
  type store_ctx

  (* *)

  type position [@@deriving to_yojson]

  val get_namectxO : position -> name_ctx
  val get_namectxP : position -> name_ctx
  val get_storectx : position -> store_ctx
  val replace_namectxP : position -> name_ctx -> position
  val replace_storectx : position -> store_ctx -> position

  val init_position :
    store_ctx -> name_ctx -> name_ctx -> position

  val string_of_position : position -> string
  val pp_position : Format.formatter -> position -> unit

  (* generate_move Γₓ return
      all the pairs (m,Γₓ') such that
      there exists a name context Δ for the free names of m such that
      Γₓ ⊢ m ▷ Δ  and Γₓ' = Γₓ + Δ.
     It uses the branching monad from BranchMonad to do so. *)
  val generate_moves :
    position -> (Moves.move * position) BranchMonad.m

  (* check_move Γₓ m return Some Δ
     when there exists a name context Γ for the free names of m such that
      Γₓ ⊢ m ▷ Δ.
     It returns None when m is not well-typed.*)
  val check_move : position -> Moves.move -> position option

  val trigger_move :
    position -> Moves.move -> name_ctx -> store_ctx -> position
end

module Make (IntLang : Lang.Interactive.LANG) :
  GAME
    with type Moves.name = IntLang.Name.name
     and type name_ctx = IntLang.name_ctx
     and type store_ctx = IntLang.store_ctx
     and type Moves.kdata = IntLang.abstract_normal_form = struct
  module Moves = Moves.Make (IntLang)
  module BranchMonad = IntLang.BranchMonad

  type name_ctx = IntLang.name_ctx
  type store_ctx = IntLang.store_ctx

  type position = {
    storectx: IntLang.store_ctx;
    namectxP: IntLang.name_ctx;
    namectxO: IntLang.name_ctx;
  }

  let get_namectxO ictx = ictx.namectxO
  let get_namectxP ictx = ictx.namectxP
  let get_storectx ictx = ictx.storectx

  let position_to_yojson ictx =
    `Assoc
      [
        ("storectx", `String (IntLang.string_of_store_ctx ictx.storectx));
        ("namectxP", IntLang.name_ctx_to_yojson ictx.namectxP);
        ("namectxO", IntLang.name_ctx_to_yojson ictx.namectxO);
      ]

  let pp_position fmt ictx =
    Format.fprintf fmt "@[⟨Σ: %a |@, ΔO: %a |@, ΔP: %a⟩@]" IntLang.pp_store_ctx
      ictx.storectx IntLang.pp_name_ctx ictx.namectxO IntLang.pp_name_ctx
      ictx.namectxP

  let string_of_position = Format.asprintf "%a" pp_position
  let replace_namectxP ictx namectxP = { ictx with namectxP }
  let replace_storectx ictx storectx = { ictx with storectx }

  let init_position storectx namectxP namectxO =
    { storectx; namectxP; namectxO }

  let generate_moves ictx =
    Util.Debug.print_debug "Generating moves";
    let open IntLang.BranchMonad in
    let* (a_nf, lnamectx, namectxP) =
      IntLang.generate_a_nf ictx.storectx ictx.namectxP in
    let namectxO = IntLang.concat_name_ctx lnamectx ictx.namectxO in
    Util.Debug.print_debug @@ "New name context :"
    ^ IntLang.string_of_name_ctx namectxP
    ^ " and "
    ^ IntLang.string_of_name_ctx namectxO;
    return (Moves.build (Moves.Input, a_nf), { ictx with namectxP; namectxO })

  let check_move ictx move =
    let a_nf = Moves.get_kdata move in
    let lnamectx_opt =
      match Moves.get_direction move with
      | Moves.Output -> IntLang.type_check_a_nf ictx.namectxO ictx.namectxP a_nf
      | Moves.Input -> IntLang.type_check_a_nf ictx.namectxP ictx.namectxO a_nf
      | Moves.None -> None in
    match lnamectx_opt with
    | Some (lnamectx, namectxP) ->
        let namectxO = IntLang.concat_name_ctx lnamectx ictx.namectxO in
        (* We suppose only dealing with inputs here !! *)
        Some { ictx with namectxO; namectxP }
    | None -> None

  let trigger_move ictx move lnamectx storectx =
    match Moves.get_direction move with
    | Moves.Output ->
        let namectxP = IntLang.concat_name_ctx lnamectx ictx.namectxP in
        { ictx with namectxP; storectx }
    | Moves.Input ->
        let namectxO = IntLang.concat_name_ctx lnamectx ictx.namectxO in
        { ictx with namectxO; storectx }
    | Moves.None ->
        failwith "Trying to trigger an undirected move. Please report."
end
