module type MOVES = functor (Lang:Language.LANG) ->
  sig
  type player = Proponent | Opponent

  type move

  type action =
    | PDiv
    | Vis of move
    (*| PRecCall of Lang.id *)

  val generate_pmove : Lang.name_type_ctx -> Lang.name -> Lang.interactive_val -> (action * Lang.interactive_env * Lang.name_type_ctx)

  val generate_omove : Lang.name_type_ctx -> (action * Lang.name_type_ctx) list

  val generate_term : Lang.interactive_env -> action -> Lang.computation

  val unify_move : Lang.name Namespan.namespan -> move -> move -> Lang.name Namespan.namespan option

  val synch_move : Lang.name Namespan.namespan -> move -> move -> Lang.name Namespan.namespan option

  val unify_action : Lang.name Namespan.namespan -> action -> action -> Lang.name Namespan.namespan option

  val string_of_action : action -> string
end

module Moves : MOVES = functor (Lang:Language.LANG) -> 
  struct

type player = Proponent | Opponent

let string_of_player = function
  | Proponent -> "P"
  | Opponent -> "O"


type move = player * Lang.name * Lang.nup

let string_of_move (player,nn,nup) = (Lang.string_of_name nn) ^"(" ^ (Lang.string_of_nup nup) ^")_" ^ (string_of_player player) 

type action =
  | PDiv
  | Vis of move
    (*| PRecCall of Lang.id *)

let string_of_action = function
  | PDiv -> "Div"
  (*| PRecCall id -> "Rec" ^ (Lang.string_of_id id) *)
  | Vis move -> string_of_move move

let generate_omove namectx = 
  let aux (id,ty) = 
    let nups = Lang.generate_nup (Lang.neg_type ty) in
    List.map (fun (nup,namectx') -> (Vis (Opponent,id,nup),namectx')) nups
  in List.flatten (Pmap.map_list aux namectx)

let generate_pmove namectxO nn value =
  let ty_option = Pmap.lookup nn namectxO in
    begin match ty_option with
      | Some ty ->
        let nty = Lang.neg_type ty in
        let (nup,ienv,namectxP) = Lang.abstract_ival value nty in
        (Vis (Proponent,nn,nup),ienv,namectxP)
      | None -> 
        failwith ("Error: the name " 
          ^ (Lang.string_of_name nn) 
          ^ " is not in the name context "
          ^ (Lang.string_of_name_type_ctx namectxO)
          ^ ". Please report.")
    end

let generate_term ienv omove = match omove with
  | Vis (Opponent,name,nup) -> 
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
    | ((Proponent,nn1,nup1),(Proponent,nn2,nup2))
      | ((Opponent,nn1,nup1),(Opponent,nn2,nup2))
        -> if (Namespan.is_in_dom_im (nn1,nn2) span) 
           then Lang.unify_nup span nup1 nup2
           else None
    | _ -> None 

let synch_move span move1 move2 =
  match (move1,move2) with
    | ((Proponent,nn1,nup1),(Opponent,nn2,nup2))
      | ((Opponent,nn1,nup1),(Proponent,nn2,nup2))
        -> if (Namespan.is_in_dom_im (nn1,nn2) span) 
           then Lang.unify_nup span nup1 nup2
           else None
    | _ -> None 

let unify_action span act1 act2 = 
  match (act1,act2) with
    | (Vis move1,Vis move2) -> unify_move span move1 move2
    | (PDiv,PDiv) -> Some span
    (*| (PRecCall _,_) | (_,PRecCall _) -> failwith "We do not support unification of recursive call actions yet." *)
    | (Vis _,PDiv) | (PDiv, Vis _) -> None

end