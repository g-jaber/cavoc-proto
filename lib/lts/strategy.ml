(** The [Strategy] module contains the various signatures implemented by the
    LTSs representing interaction. *)

module type LTS = sig
  (* The following field is to be instantiated *)
  module TypingLTS : Typing.LTS
  module EvalMonad : Util.Monad.RUNNABLE

  (** There are two types of configurations.
      - {!type: passive_conf} is obtained after the propnent has played ;
      - {!type: active_conf} is obtained after the opponent has played. *)

  type active_conf
  type passive_conf [@@deriving to_yojson]
  type conf = Active of active_conf | Passive of passive_conf

  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val pp_active_conf : Format.formatter -> active_conf -> unit
  val pp_passive_conf : Format.formatter -> passive_conf -> unit
  val equiv_act_conf : active_conf -> active_conf -> bool
  val get_active_pos : active_conf -> TypingLTS.position
  val get_passive_pos : passive_conf -> TypingLTS.position

  val p_trans :
    active_conf -> (TypingLTS.Moves.pol_move * passive_conf) EvalMonad.m
  (** [p_trans] is used to generate the next configuration from a given
      {!type: active_conf}. Depending on the type {!type: EvalMonad.r},
      [p_trans] may produce more than one configuration. *)

  val o_trans : passive_conf -> TypingLTS.Moves.pol_move -> active_conf option

  val o_trans_gen :
    passive_conf ->
    (TypingLTS.Moves.pol_move * active_conf) TypingLTS.BranchMonad.m
  (** [o_trans_gen] is used to generate all possible moves from a given
      {!type: passive_conf} *)
end

(** [LTS_WITH_INIT] is a {!module-type: LTS} capable of parsing its initial
    configuration from lexing buffers. *)
module type LTS_WITH_INIT = sig
  include LTS

  val lexing_init_aconf : Lexing.lexbuf -> active_conf

  val lexing_init_pconf :
    ?opponent_signature:Lexing.lexbuf ->
    Lexing.lexbuf ->
    Lexing.lexbuf ->
    passive_conf
  (** [lexing_init_pconf] takes two lexing buffers, respectively containing the
      implementation and the signature of a module, and return an initial
      {{!type: Lts.Strategy.LTS.passive_conf}passive configuration}

      [opponent_signature], when provided, is the signature of names the module
      may use but does not implement: they become Opponent names of the
      configuration. *)
end

module type LTS_WITH_INIT_BIN = sig
  include LTS

  val lexing_init_aconf : Lexing.lexbuf -> Lexing.lexbuf -> active_conf

  val lexing_init_pconf :
    Lexing.lexbuf -> Lexing.lexbuf -> Lexing.lexbuf -> passive_conf
end

(* The tensor of two strategies, over the tensor of their typing LTSs.
  At most one of the two is active at a time. *)
module Tensor
    (S1 : LTS)
    (S2 :
      LTS
        with module EvalMonad = S1.EvalMonad
         and module TypingLTS.BranchMonad = S1.TypingLTS.BranchMonad) : sig
  type active_conf =
    | ActiveLeft of S1.active_conf * S2.passive_conf
    | ActiveRight of S1.passive_conf * S2.active_conf

  type passive_conf = S1.passive_conf * S2.passive_conf

  (* The two configurations are substituted into the signature to stay
     concrete. *)
  include
    LTS
      with module TypingLTS = Typing.Tensor(S1.TypingLTS)(S2.TypingLTS)
       and module EvalMonad = S1.EvalMonad
       and type active_conf := active_conf
       and type passive_conf := passive_conf
end = struct
  module TypingLTS = Typing.Tensor (S1.TypingLTS) (S2.TypingLTS)
  module EvalMonad = S1.EvalMonad

  type active_conf =
    | ActiveLeft of S1.active_conf * S2.passive_conf
    | ActiveRight of S1.passive_conf * S2.active_conf

  type passive_conf = S1.passive_conf * S2.passive_conf
  type conf = Active of active_conf | Passive of passive_conf

  let passive_conf_to_yojson (pas1, pas2) =
    `Assoc
      [
        ("left", S1.passive_conf_to_yojson pas1);
        ("right", S2.passive_conf_to_yojson pas2);
      ]

  let pp_active_conf fmt = function
    | ActiveLeft (act1, pas2) ->
        Format.fprintf fmt "@[⟨%a@ ⊗@ %a⟩@]" S1.pp_active_conf act1
          S2.pp_passive_conf pas2
    | ActiveRight (pas1, act2) ->
        Format.fprintf fmt "@[⟨%a@ ⊗@ %a⟩@]" S1.pp_passive_conf pas1
          S2.pp_active_conf act2

  let pp_passive_conf fmt (pas1, pas2) =
    Format.fprintf fmt "@[⟨%a@ ⊗@ %a⟩@]" S1.pp_passive_conf pas1
      S2.pp_passive_conf pas2

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let equiv_act_conf aconf aconf' =
    match (aconf, aconf') with
    | (ActiveLeft (act1, _), ActiveLeft (act1', _)) ->
        S1.equiv_act_conf act1 act1'
    | (ActiveRight (_, act2), ActiveRight (_, act2')) ->
        S2.equiv_act_conf act2 act2'
    | _ -> false

  let get_active_pos = function
    | ActiveLeft (act1, pas2) ->
        (S1.get_active_pos act1, S2.get_passive_pos pas2)
    | ActiveRight (pas1, act2) ->
        (S1.get_passive_pos pas1, S2.get_active_pos act2)

  let get_passive_pos (pas1, pas2) =
    (S1.get_passive_pos pas1, S2.get_passive_pos pas2)

  let direction2 = function
    | TypingLTS.Moves.Input -> S2.TypingLTS.Moves.Input
    | TypingLTS.Moves.Output -> S2.TypingLTS.Moves.Output

  let direction_of2 = function
    | S2.TypingLTS.Moves.Input -> TypingLTS.Moves.Input
    | S2.TypingLTS.Moves.Output -> TypingLTS.Moves.Output

  let p_trans aconf =
    let open EvalMonad in
    match aconf with
    | ActiveLeft (act1, pas2) ->
        let* ((dir, move), pas1) = S1.p_trans act1 in
        return ((dir, Either.Left move), (pas1, pas2))
    | ActiveRight (pas1, act2) ->
        let* ((dir, move), pas2) = S2.p_trans act2 in
        return ((direction_of2 dir, Either.Right move), (pas1, pas2))

  let o_trans (pas1, pas2) ((dir, move) : TypingLTS.Moves.pol_move) =
    match move with
    | Either.Left move ->
        Option.map
          (fun act1 -> ActiveLeft (act1, pas2))
          (S1.o_trans pas1 (dir, move))
    | Either.Right move ->
        Option.map
          (fun act2 -> ActiveRight (pas1, act2))
          (S2.o_trans pas2 (direction2 dir, move))

  let o_trans_gen (pas1, pas2) =
    let open TypingLTS.BranchMonad in
    para_pair
      (let* ((dir, move), act1) = S1.o_trans_gen pas1 in
       return ((dir, Either.Left move), ActiveLeft (act1, pas2)))
      (let* ((dir, move), act2) = S2.o_trans_gen pas2 in
       return ((direction_of2 dir, Either.Right move), ActiveRight (pas1, act2)))
end

(* The split of a strategy over T into a strategy over T ⊸ T.
  Its configurations carry a renaming from the
   concatenation of the two sides to its own position. *)
module Split
    (T : Typing.LTS)
    (S : LTS with module TypingLTS.Moves = T.Moves) : sig
  include
    LTS
      with module TypingLTS = Typing.Lollipop(T)(T)
       and module EvalMonad = S.EvalMonad

  type renaming = Typing.Position_renaming(T.Moves).t

  val identity_renaming : TypingLTS.position -> renaming

  val split_pconf :
    S.passive_conf -> TypingLTS.position -> renaming -> passive_conf

  val split_aconf :
    S.active_conf -> TypingLTS.position -> renaming -> active_conf

  val renaming_of_passive_conf : passive_conf -> renaming
  val renaming_of_active_conf : active_conf -> renaming
end = struct
  module TypingLTS = Typing.Lollipop (T) (T)
  module EvalMonad = S.EvalMonad
  module Renaming = Typing.Position_renaming (T.Moves)
  module Concatenation = Typing.Concatenation (T.Moves)
  module Namectx = T.Moves.Renaming.Namectx

  type renaming = Renaming.t

  type active_conf = {
    act: S.active_conf;
    act_position: TypingLTS.position;
    act_renaming: renaming;
  }

  type passive_conf = {
    pas: S.passive_conf;
    pas_position: TypingLTS.position;
    pas_renaming: renaming;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let passive_conf_to_yojson pconf =
    `Assoc
      [
        ("strategy", S.passive_conf_to_yojson pconf.pas);
        ("position", TypingLTS.position_to_yojson pconf.pas_position);
        ("renaming", Renaming.to_yojson pconf.pas_renaming);
      ]

  let pp_active_conf fmt aconf =
    Format.fprintf fmt "@[⟨%a@ @[Pos: %a@]@ @[Renaming: %a@]⟩@]"
      S.pp_active_conf aconf.act TypingLTS.pp_position aconf.act_position
      Renaming.pp aconf.act_renaming

  let pp_passive_conf fmt pconf =
    Format.fprintf fmt "@[⟨%a@ @[Pos: %a@]@ @[Renaming: %a@]⟩@]"
      S.pp_passive_conf pconf.pas TypingLTS.pp_position pconf.pas_position
      Renaming.pp pconf.pas_renaming

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf
  let equiv_act_conf aconf aconf' = S.equiv_act_conf aconf.act aconf'.act
  let get_active_pos aconf = aconf.act_position
  let get_passive_pos pconf = pconf.pas_position
  let renaming_of_passive_conf pconf = pconf.pas_renaming
  let renaming_of_active_conf aconf = aconf.act_renaming

  let same_context namectx namectx' =
    Namectx.to_pmap namectx = Namectx.to_pmap namectx'

  let identity_renaming position =
    Renaming.identity
      (Concatenation.of_sides (TypingLTS.get_namectxP position))
      (Concatenation.of_sides (TypingLTS.get_namectxO position))

  (* The renaming must land in the strategy's own contexts. *)
  let check_renaming own_position (renaming : renaming) =
    assert (
      same_context
        (S.TypingLTS.get_namectxP own_position)
        (T.Moves.Renaming.im renaming.proponent)
      && same_context
           (S.TypingLTS.get_namectxO own_position)
           (T.Moves.Renaming.im renaming.opponent))

  let split_pconf pas pas_position pas_renaming =
    check_renaming (S.get_passive_pos pas) pas_renaming;
    { pas; pas_position; pas_renaming }

  let split_aconf act act_position act_renaming =
    check_renaming (S.get_active_pos act) act_renaming;
    { act; act_position; act_renaming }

  (* The sides of the context a move's subject lives in, and of the one the
     names it introduces land in. *)
  let subject_sides position = function
    | TypingLTS.Moves.Input -> TypingLTS.get_namectxP position
    | TypingLTS.Moves.Output -> TypingLTS.get_namectxO position

  let introduced_sides position = function
    | TypingLTS.Moves.Input -> TypingLTS.get_namectxO position
    | TypingLTS.Moves.Output -> TypingLTS.get_namectxP position

  let extend renaming position dir tagged =
    Renaming.update_introduced renaming dir (fun renaming ->
        Concatenation.extend_renaming renaming
          (introduced_sides position dir)
          tagged)

  let outside_the_fragment own_action msg =
    failwith
      ("Splitting outside the fragment: the move "
      ^ T.Moves.string_of_pol_move own_action
      ^ " " ^ msg)

  let p_trans aconf =
    let open EvalMonad in
    let* (((dir, _) as own_action), pas) = S.p_trans aconf.act in
    let position = aconf.act_position in
    let at_concatenation =
      match Renaming.rename_move_inv aconf.act_renaming own_action with
      | Some (_, move) -> move
      | None ->
          outside_the_fragment own_action "has a free name outside the renaming"
    in
    let tagged =
      match Concatenation.tag (subject_sides position dir) at_concatenation with
      | Some tagged -> tagged
      | None -> outside_the_fragment own_action "has free names on both sides"
    in
    let action = (dir, tagged) in
    let (_, pas_position) = TypingLTS.trigger_move position action in
    return
      ( action,
        {
          pas;
          pas_position;
          pas_renaming= extend aconf.act_renaming position dir tagged;
        } )

  let o_trans pconf ((dir, tagged) as action : TypingLTS.Moves.pol_move) =
    match TypingLTS.check_move pconf.pas_position action with
    | None -> None
    | Some (_, act_position) ->
        let position = pconf.pas_position in
        let at_concatenation =
          Concatenation.untag (subject_sides position dir) tagged in
        let own_action =
          Renaming.rename_move pconf.pas_renaming (dir, at_concatenation) in
        Option.map
          (fun act ->
            {
              act;
              act_position;
              act_renaming= extend pconf.pas_renaming position dir tagged;
            })
          (S.o_trans pconf.pas own_action)

  let o_trans_gen pconf =
    let open TypingLTS.BranchMonad in
    let* (action, _, _) =
      TypingLTS.generate_moves pconf.pas_position TypingLTS.Moves.Input in
    match o_trans pconf action with
    | None -> fail ()
    | Some aconf -> return (action, aconf)
end

(* The join of a strategy over T ⊸ T into a strategy over T', at a single
   position starting as the concatenations of the two sides. *)
module Join
    (T : Typing.LTS)
    (T' : Typing.LTS with module Moves = T.Moves)
    (S : LTS with module TypingLTS = Typing.Lollipop(T)(T)) : sig
  include LTS with module TypingLTS = T' and module EvalMonad = S.EvalMonad

  type renaming = Typing.Position_renaming(T.Moves).t

  val join_pconf : T'.store_ctx -> S.passive_conf -> passive_conf
  val join_aconf : T'.store_ctx -> S.active_conf -> active_conf
  val renaming_of_passive_conf : passive_conf -> renaming
  val renaming_of_active_conf : active_conf -> renaming
end = struct
  module TypingLTS = T'
  module EvalMonad = S.EvalMonad
  module Renaming = Typing.Position_renaming (T.Moves)
  module Concatenation = Typing.Concatenation (T.Moves)

  type renaming = Renaming.t

  type active_conf = {
    act: S.active_conf;
    act_position: T'.position;
    act_renaming: renaming;
  }

  type passive_conf = {
    pas: S.passive_conf;
    pas_position: T'.position;
    pas_renaming: renaming;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let passive_conf_to_yojson pconf =
    `Assoc
      [
        ("strategy", S.passive_conf_to_yojson pconf.pas);
        ("position", T'.position_to_yojson pconf.pas_position);
        ("renaming", Renaming.to_yojson pconf.pas_renaming);
      ]

  let pp_active_conf fmt aconf =
    Format.fprintf fmt "@[⟨%a@ @[Pos: %a@]@ @[Renaming: %a@]⟩@]"
      S.pp_active_conf aconf.act T'.pp_position aconf.act_position Renaming.pp
      aconf.act_renaming

  let pp_passive_conf fmt pconf =
    Format.fprintf fmt "@[⟨%a@ @[Pos: %a@]@ @[Renaming: %a@]⟩@]"
      S.pp_passive_conf pconf.pas T'.pp_position pconf.pas_position Renaming.pp
      pconf.pas_renaming

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf
  let equiv_act_conf aconf aconf' = S.equiv_act_conf aconf.act aconf'.act
  let get_active_pos aconf = aconf.act_position
  let get_passive_pos pconf = pconf.pas_position
  let renaming_of_passive_conf pconf = pconf.pas_renaming
  let renaming_of_active_conf aconf = aconf.act_renaming

  let joined init storectx position =
    let namectxP = Concatenation.of_sides (S.TypingLTS.get_namectxP position) in
    let namectxO = Concatenation.of_sides (S.TypingLTS.get_namectxO position) in
    (init storectx namectxP namectxO, Renaming.identity namectxP namectxO)

  let join_pconf storectx pas =
    let (pas_position, pas_renaming) =
      joined T'.init_pas_pos storectx (S.get_passive_pos pas) in
    { pas; pas_position; pas_renaming }

  let join_aconf storectx act =
    let (act_position, act_renaming) =
      joined T'.init_act_pos storectx (S.get_active_pos act) in
    { act; act_position; act_renaming }

  let subject_sides position = function
    | T'.Moves.Input -> S.TypingLTS.get_namectxP position
    | T'.Moves.Output -> S.TypingLTS.get_namectxO position

  let introduced_sides position = function
    | T'.Moves.Input -> S.TypingLTS.get_namectxO position
    | T'.Moves.Output -> S.TypingLTS.get_namectxP position

  let extend renaming position dir tagged =
    Renaming.update_introduced renaming dir (fun renaming ->
        Concatenation.extend_renaming renaming
          (introduced_sides position dir)
          tagged)

  let p_trans aconf =
    let open EvalMonad in
    let* (((dir, tagged) as action), pas) = S.p_trans aconf.act in
    let position = S.get_active_pos aconf.act in
    let at_concatenation =
      Concatenation.untag (subject_sides position dir) tagged in
    let own_action =
      Renaming.rename_move aconf.act_renaming (dir, at_concatenation) in
    let (_, pas_position) = T'.trigger_move aconf.act_position own_action in
    ignore action;
    return
      ( own_action,
        {
          pas;
          pas_position;
          pas_renaming= extend aconf.act_renaming position dir tagged;
        } )

  let o_trans pconf ((dir, _) as own_action : T'.Moves.pol_move) =
    match T'.check_move pconf.pas_position own_action with
    | None -> None
    | Some (_, act_position) -> begin
        let position = S.get_passive_pos pconf.pas in
        match Renaming.rename_move_inv pconf.pas_renaming own_action with
        | None -> None
        | Some (_, at_concatenation) -> begin
            match
              Concatenation.tag (subject_sides position dir) at_concatenation
            with
            | None -> None
            | Some tagged ->
                Option.map
                  (fun act ->
                    {
                      act;
                      act_position;
                      act_renaming=
                        extend pconf.pas_renaming position dir tagged;
                    })
                  (S.o_trans pconf.pas (dir, tagged))
          end
      end

  let o_trans_gen pconf =
    let open T'.BranchMonad in
    let* (own_action, _, _) =
      T'.generate_moves pconf.pas_position T'.Moves.Input in
    match o_trans pconf own_action with
    | None -> fail ()
    | Some aconf -> return (own_action, aconf)
end
