module type LTS = sig
  module Moves : Moves.POLMOVES
  module BranchMonad : Util.Monad.BRANCH

  type store_ctx
  type position [@@deriving to_yojson]

  val get_namectxO : position -> Moves.Renaming.Namectx.t
  val get_namectxP : position -> Moves.Renaming.Namectx.t
  val get_storectx : position -> store_ctx

  val init_act_pos :
    store_ctx ->
    Moves.Renaming.Namectx.t ->
    Moves.Renaming.Namectx.t ->
    position

  val init_pas_pos :
    store_ctx ->
    Moves.Renaming.Namectx.t ->
    Moves.Renaming.Namectx.t ->
    position

  val string_of_position : position -> string
  val pp_position : Format.formatter -> position -> unit

  (* Every transition returns, with the target position, the weakening of
     the local context of the move. *)

  (* generate_moves Γₓ dir returns all the pairs (m, Γₓ') such that m has
     direction dir and there exists a name context Δ for the free names of m
     with Γₓ ⊢ m ▷ Δ and Γₓ' = Γₓ + Δ, using the branching monad. *)
  (* A position need not record whose turn it is, so the direction is
     supplied. *)
  val generate_moves :
    position ->
    Moves.direction ->
    (Moves.pol_move * Moves.Renaming.t * position) BranchMonad.m

  (* check_move Γₓ m returns None when m is not well-typed at Γₓ. *)
  val check_move :
    position -> Moves.pol_move -> (Moves.Renaming.t * position) option

  (* trigger_move extends the position by m without type checking it. *)
  val trigger_move : position -> Moves.pol_move -> Moves.Renaming.t * position
end

(* The dual of a typing LTS: the same positions with the two contexts read
   swapped, and every move switched. *)
module Dual (T : LTS) :
  LTS
    with module Moves = T.Moves
     and module BranchMonad = T.BranchMonad
     and type store_ctx = T.store_ctx
     and type position = T.position = struct
  module Moves = T.Moves
  module BranchMonad = T.BranchMonad

  type store_ctx = T.store_ctx
  type position = T.position

  let position_to_yojson = T.position_to_yojson
  let get_namectxO = T.get_namectxP
  let get_namectxP = T.get_namectxO
  let get_storectx = T.get_storectx

  (* The dual's active position is T's passive one, contexts exchanged. *)
  let init_act_pos storectx namectxP namectxO =
    T.init_pas_pos storectx namectxO namectxP

  let init_pas_pos storectx namectxP namectxO =
    T.init_act_pos storectx namectxO namectxP

  let string_of_position = T.string_of_position
  let pp_position = T.pp_position

  let switch = function
    | Moves.Input -> Moves.Output
    | Moves.Output -> Moves.Input

  let generate_moves pos dir =
    let open BranchMonad in
    let* (move, weakening, pos') = T.generate_moves pos (switch dir) in
    return (Moves.switch_direction move, weakening, pos')

  let check_move pos move = T.check_move pos (Moves.switch_direction move)
  let trigger_move pos move = T.trigger_move pos (Moves.switch_direction move)
end

(* The tensor of two typing LTS: positions are pairs, and a move is a move
   of either, played there while the other position stays. *)
module Tensor (T1 : LTS) (T2 : LTS with module BranchMonad = T1.BranchMonad) :
  LTS
    with module Moves = Moves.Tensor(T1.Moves)(T2.Moves)
     and module BranchMonad = T1.BranchMonad
     and type store_ctx = T1.store_ctx * T2.store_ctx
     and type position = T1.position * T2.position = struct
  module Moves = Moves.Tensor (T1.Moves) (T2.Moves)
  module BranchMonad = T1.BranchMonad

  type store_ctx = T1.store_ctx * T2.store_ctx
  type position = T1.position * T2.position

  let position_to_yojson (pos1, pos2) =
    `Assoc
      [
        ("left", T1.position_to_yojson pos1);
        ("right", T2.position_to_yojson pos2);
      ]

  let get_namectxO (pos1, pos2) = (T1.get_namectxO pos1, T2.get_namectxO pos2)
  let get_namectxP (pos1, pos2) = (T1.get_namectxP pos1, T2.get_namectxP pos2)
  let get_storectx (pos1, pos2) = (T1.get_storectx pos1, T2.get_storectx pos2)

  let init_act_pos (storectx1, storectx2) (namectxP1, namectxP2)
      (namectxO1, namectxO2) =
    ( T1.init_act_pos storectx1 namectxP1 namectxO1,
      T2.init_act_pos storectx2 namectxP2 namectxO2 )

  let init_pas_pos (storectx1, storectx2) (namectxP1, namectxP2)
      (namectxO1, namectxO2) =
    ( T1.init_pas_pos storectx1 namectxP1 namectxO1,
      T2.init_pas_pos storectx2 namectxP2 namectxO2 )

  let pp_position fmt (pos1, pos2) =
    Format.fprintf fmt "@[%a@ ⊗@ %a@]" T1.pp_position pos1 T2.pp_position pos2

  let string_of_position = Format.asprintf "%a" pp_position

  let direction2 = function
    | Moves.Input -> T2.Moves.Input
    | Moves.Output -> T2.Moves.Output

  (* The side a move is not played in contributes the empty weakening into
     its context of the move's polarity. *)
  let empty_weakening1 pos1 = function
    | Moves.Input ->
        T1.Moves.Renaming.weak_r T1.Moves.Renaming.Namectx.empty
          (T1.get_namectxO pos1)
    | Moves.Output ->
        T1.Moves.Renaming.weak_r T1.Moves.Renaming.Namectx.empty
          (T1.get_namectxP pos1)

  let empty_weakening2 pos2 = function
    | Moves.Input ->
        T2.Moves.Renaming.weak_r T2.Moves.Renaming.Namectx.empty
          (T2.get_namectxO pos2)
    | Moves.Output ->
        T2.Moves.Renaming.weak_r T2.Moves.Renaming.Namectx.empty
          (T2.get_namectxP pos2)

  let generate_moves (pos1, pos2) dir =
    let open BranchMonad in
    para_pair
      (let* ((_, move), weakening1, pos1') = T1.generate_moves pos1 dir in
       return
         ( (dir, Either.Left move),
           (weakening1, empty_weakening2 pos2 dir),
           (pos1', pos2) ))
      (let* ((_, move), weakening2, pos2') =
         T2.generate_moves pos2 (direction2 dir) in
       return
         ( (dir, Either.Right move),
           (empty_weakening1 pos1 dir, weakening2),
           (pos1, pos2') ))

  let check_move (pos1, pos2) ((dir, move) : Moves.pol_move) =
    match move with
    | Either.Left move ->
        Option.map
          (fun (weakening1, pos1') ->
            ((weakening1, empty_weakening2 pos2 dir), (pos1', pos2)))
          (T1.check_move pos1 (dir, move))
    | Either.Right move ->
        Option.map
          (fun (weakening2, pos2') ->
            ((empty_weakening1 pos1 dir, weakening2), (pos1, pos2')))
          (T2.check_move pos2 (direction2 dir, move))

  let trigger_move (pos1, pos2) ((dir, move) : Moves.pol_move) =
    match move with
    | Either.Left move ->
        let (weakening1, pos1') = T1.trigger_move pos1 (dir, move) in
        ((weakening1, empty_weakening2 pos2 dir), (pos1', pos2))
    | Either.Right move ->
        let (weakening2, pos2') = T2.trigger_move pos2 (direction2 dir, move) in
        ((empty_weakening1 pos1 dir, weakening2), (pos1, pos2'))
end

(* T1 ⊸ T2 = T1 ⊗ T2^⊥: the Proponent context of (p1, p2) pairs p1's
   Proponent context with p2's Opponent context. *)
module Lollipop (T1 : LTS) (T2 : LTS with module BranchMonad = T1.BranchMonad) =
  Tensor (T1) (Dual (T2))

(* A renaming ρ : p ↪ q between two positions, one renaming per polarity,
   contravariant on the Opponent side. *)
module Polarized_renaming (Moves : Moves.POLMOVES) = struct
  module Renaming = Moves.Renaming
  module Names = Renaming.Namectx.Names

  type t = { proponent: Renaming.t; opponent: Renaming.t }

  let identity namectxP namectxO =
    { proponent= Renaming.id namectxP; opponent= Renaming.id namectxO }

  (* An Input move at p read at q, an Output move at q read at p. *)
  let rename_move renaming ((dir, move) : Moves.pol_move) : Moves.pol_move =
    let renaming =
      match dir with
      | Moves.Input -> renaming.proponent
      | Moves.Output -> renaming.opponent in
    ( dir,
      Moves.map_free_names
        (fun nn ->
          if Renaming.is_in_dom renaming nn then Renaming.lookup renaming nn
          else
            failwith
              ("Renaming a move whose free name " ^ Names.string_of_name nn
             ^ " is outside the renaming."))
        move )

  (* The renaming of the contexts the names a move introduces land in. *)
  let update_introduced renaming dir f =
    match dir with
    | Moves.Input -> { renaming with opponent= f renaming.opponent }
    | Moves.Output -> { renaming with proponent= f renaming.proponent }

  let pp fmt renaming =
    Format.fprintf fmt "@[⟨P: %a;@ O: %a⟩@]" Renaming.pp renaming.proponent
      Renaming.pp renaming.opponent

  let to_yojson renaming =
    `Assoc
      [
        ("proponent", `String (Renaming.to_string renaming.proponent));
        ("opponent", `String (Renaming.to_string renaming.opponent));
      ]
end

(* The concatenation of the two sides of a position of T ⊸ T, with the
   reading of moves across it. *)
module Concatenation (T : LTS) = struct
  module Lollipop = Lollipop (T) (T)
  module Renaming = T.Moves.Renaming
  module Namectx = Renaming.Namectx
  module Polarized_renaming = Polarized_renaming (T.Moves)

  let concat (namectx_l, namectx_r) = Namectx.concat namectx_l namectx_r
  let get_namectxP position = concat (Lollipop.get_namectxP position)
  let get_namectxO position = concat (Lollipop.get_namectxO position)

  (* The context of T ⊸ T the subject of a move lives in. *)
  let subject_namectx position = function
    | T.Moves.Input -> Lollipop.get_namectxP position
    | T.Moves.Output -> Lollipop.get_namectxO position

  (* A tagged move read at the concatenation. *)
  let join_move position ((dir, tagged) : Lollipop.Moves.pol_move) :
      T.Moves.pol_move =
    let (namectx_l, namectx_r) = subject_namectx position dir in
    ( dir,
      match tagged with
      | Either.Left move ->
          T.Moves.map_free_names
            (Renaming.lookup (Renaming.weak_l namectx_l namectx_r))
            move
      | Either.Right move ->
          T.Moves.map_free_names
            (Renaming.lookup (Renaming.weak_r namectx_r namectx_l))
            move )

  (* A move at the concatenation tagged by the side of its subject. *)
  let split_move position ((dir, move) : T.Moves.pol_move) :
      Lollipop.Moves.pol_move =
    let (namectx_l, namectx_r) = subject_namectx position dir in
    let weak_l = Renaming.weak_l namectx_l namectx_r in
    let weak_r = Renaming.weak_r namectx_r namectx_l in
    let through weakening =
      T.Moves.map_free_names
        (fun nn ->
          match Renaming.lookup_inv weakening nn with
          | Some nn -> nn
          | None ->
              failwith "Splitting a move whose free names are on both sides.")
        move in
    match Renaming.lookup_inv weak_l (T.Moves.get_subject_name move) with
    | Some _ -> (dir, Either.Left (through weak_l))
    | None -> (dir, Either.Right (through weak_r))

  (* ρ ⊕ id Δ after a tagged move introduced Δ, Δ permuted to its side on
     the end of ρ that is the concatenation, the domain or the codomain. *)
  let extend_renaming concatenated_domain position
      ((dir, tagged) : Lollipop.Moves.pol_move) renaming =
    (* The names a move introduces land in the context of the other
       polarity. *)
    let (namectx_l, namectx_r) =
      match dir with
      | T.Moves.Input -> Lollipop.get_namectxO position
      | T.Moves.Output -> Lollipop.get_namectxP position in
    let lnamectx =
      match tagged with
      | Either.Left move | Either.Right move -> T.Moves.get_namectx move in
    Polarized_renaming.update_introduced renaming dir (fun renaming ->
        let extended = Renaming.concat renaming (Renaming.id lnamectx) in
        match tagged with
        | Either.Right _ -> extended
        | Either.Left _ ->
            if concatenated_domain then
              Renaming.compose extended
                (Renaming.concat (Renaming.id namectx_l)
                   (Renaming.sym lnamectx namectx_r))
            else
              Renaming.compose
                (Renaming.concat (Renaming.id namectx_l)
                   (Renaming.sym namectx_r lnamectx))
                extended)
end
