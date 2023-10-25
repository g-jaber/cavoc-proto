module type CONT_NAMES = sig
  (* to be instantiated *)
  type name
  (* *)
  type cont_name
  val inj_cont_name : cont_name -> name
  val get_cont_name : name -> cont_name option
  val string_of_cont_name : cont_name -> string
end

module type LANG = sig
  include Language.LANG
  include CONT_NAMES with type name := name
end

module type MOVES = sig
  (* to be instantiated *)
  type kind
  type data
  type player = Proponent | Opponent
  (* *)

  type move = player * kind * data
  val string_of_move : move -> string
  val get_kind : move -> kind
  val get_data : move -> data
  val get_player : move -> player
  val dual : move -> move
  module ContNames : CONT_NAMES
  val get_transmitted_continuation_names : move -> ContNames.cont_name list
  val get_active_continuation_name : move -> ContNames.cont_name option
end

module Moves_Make (Lang:LANG) = struct
  type kind = Lang.name
  type data = Lang.nup
  type player = Proponent | Opponent

  let string_of_player = function
    | Proponent -> "P"
    | Opponent -> "O"

  let switch_player = function
    | Proponent -> Opponent
    | Opponent -> Proponent

  type move = player * kind * data

  let string_of_move (player,nn,nup) = (Lang.string_of_name nn) ^"(" ^ (Lang.string_of_nup nup) ^")_" ^ (string_of_player player)
  
  let get_data (_,_,d) = d
  let get_kind (_,k,_) = k
  let get_player (p,_,_) = p

  let dual (p,k,d) = (switch_player p,k,d)

  (* Should we use a sub-module Cont_Names in Lang instead ?*)
  module ContNames = struct
    type name = Lang.name
    type cont_name = Lang.cont_name
    let inj_cont_name = Lang.inj_cont_name
    let get_cont_name = Lang.get_cont_name
    let string_of_cont_name = Lang.string_of_cont_name
  end
  (* *)
  let get_transmitted_continuation_names (_,_,nup) = 
    let names_list = Lang.names_of_nup nup in
    List.filter_map Lang.get_cont_name names_list
  let get_active_continuation_name (_,nn,_)= Lang.get_cont_name nn
end

module type INT = sig
  module Moves : MOVES
  module Actions : Interactive.ACTIONS  with type move = Moves.move and type Lang.name = Moves.ContNames.name
end


module Int_Make (Lang:LANG) : INT with type Moves.ContNames.name = Lang.name  = struct

  module Moves = Moves_Make(Lang)

  module Actions = struct
    module Lang = Lang
    type move = Moves.move
    type action =
      | PDiv
      | Vis of Moves.move

    let get_move_from_action = function
      | Vis move -> Some move
      | PDiv -> None

    let diverging_action = PDiv

    let string_of_action = function
      | PDiv -> "Div"
      | Vis move -> Moves.string_of_move move

    let generate_input_action namectx = 
      Debug.print_debug "Generating O-moves";
      let aux (id,ty) = 
        let nups = Lang.generate_nup (Lang.neg_type ty) in
        List.map (fun (nup,namectx') -> (Vis (Moves.Opponent,id,nup),namectx')) nups
      in List.flatten (Pmap.map_list aux namectx)

    let generate_output_action namectxO nn value =
      let ty_option = Pmap.lookup nn namectxO in
        begin match ty_option with
          | Some ty ->
            let nty = Lang.neg_type ty in
            let (nup,ienv,namectxP) = Lang.abstract_ival value nty in
            (Vis (Moves.Proponent,nn,nup),ienv,namectxP)
          | None -> 
            failwith ("Error: the name " 
              ^ (Lang.string_of_name nn) 
              ^ " is not in the name context "
              ^ (Lang.string_of_name_type_ctx namectxO)
              ^ ". Please report.")
        end

    let generate_computation ienv omove = match omove with
      | Vis (Moves.Opponent,name,nup) -> 
        begin match Lang.lookup_ienv name ienv with
        | Some value -> Lang.val_composition value nup
        | None -> 
          failwith ("Error: the action " ^ (string_of_action omove) ^ " is ill-formed: the name "
            ^ (Lang.string_of_name name) ^ " is not in the environment " ^ (Lang.string_of_ienv ienv) 
            ^ ". Please report.")
        end
      | _ -> failwith ("Error: the action " ^ (string_of_action omove) ^ " is not an Opponent move. Please report.")

    let unify_move span move1 move2 =
      match (move1,move2) with
        | ((Moves.Proponent,nn1,nup1),(Moves.Proponent,nn2,nup2))
          | ((Moves.Opponent,nn1,nup1),(Moves.Opponent,nn2,nup2))
            -> if (Namespan.is_in_dom_im (nn1,nn2) span) 
              then Lang.unify_nup span nup1 nup2
              else None
        | _ -> None 

    let synch_move span move1 move2 =
      match (move1,move2) with
        | ((Moves.Proponent,nn1,nup1),(Moves.Opponent,nn2,nup2))
          | ((Moves.Opponent,nn1,nup1),(Moves.Proponent,nn2,nup2))
            -> if (Namespan.is_in_dom_im (nn2,nn1) span) 
              then Lang.unify_nup span nup1 nup2
              else None
        | _ -> None 

    let unify_action span act1 act2 = 
      match (act1,act2) with
        | (Vis move1,Vis move2) -> unify_move span move1 move2
        | (PDiv,PDiv) -> Some span
        | (Vis _,PDiv) | (PDiv, Vis _) -> None

    let synch_action span act1 act2 = 
      match (act1,act2) with
        | (Vis move1,Vis move2) -> synch_move span move1 move2
        | (PDiv,PDiv) -> Some span
        | (Vis _,PDiv) | (PDiv, Vis _) -> None

  end
end
