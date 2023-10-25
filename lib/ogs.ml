module OgsLtsF = functor (M:Monad.LISTMONAD) (Int:Interactive.INT) -> struct

  module M=M
  include M
  module Int=Int
  type action = Int.Actions.action
  type move = Int.Moves.move
  let get_move_from_action = Int.Actions.get_move_from_action

  type active_conf = 
    { computation : Int.Actions.Lang.computation;
      heap : Int.Actions.Lang.resources;
      ienv : Int.Actions.Lang.interactive_env;
      namectxO : Int.Actions.Lang.name_type_ctx;
      namectxP : Int.Actions.Lang.name_type_ctx }

  type passive_conf = 
    { heap : Int.Actions.Lang.resources;
      ienv : Int.Actions.Lang.interactive_env;
      namectxO : Int.Actions.Lang.name_type_ctx;
      namectxP : Int.Actions.Lang.name_type_ctx }

  type conf = 
    | Active of active_conf
    | Passive of passive_conf

  let string_of_active_conf aconf =
    "<" 
    ^ Int.Actions.Lang.string_of_computation aconf.computation 
    ^ " | " 
    ^ Int.Actions.Lang.string_of_resources aconf.heap
    ^ " | " 
    ^ Int.Actions.Lang.string_of_ienv aconf.ienv 
    ^ " | " 
    ^ Int.Actions.Lang.string_of_name_type_ctx aconf.namectxO
    ^ " | " 
    ^ Int.Actions.Lang.string_of_name_type_ctx aconf.namectxP
    ^ ">"

  let string_of_passive_conf pconf =
    "<"   
    ^ Int.Actions.Lang.string_of_resources pconf.heap
    ^ " | " 
    ^ Int.Actions.Lang.string_of_ienv pconf.ienv 
    ^ " | " 
    ^ Int.Actions.Lang.string_of_name_type_ctx pconf.namectxO
    ^ " | " 
    ^ Int.Actions.Lang.string_of_name_type_ctx pconf.namectxP
    ^ ">"

  let p_trans aconf =
    let opconf_option = Int.Actions.Lang.compute_nf (aconf.computation,aconf.heap) in
    match opconf_option with
      | None -> (Int.Actions.diverging_action, None)
      | Some ((_,heap) as opconf) -> 
        let (nn,value) = Int.Actions.Lang.decompose_nf opconf in
        let (move,ienv',namectxP') = Int.Actions.generate_output_action aconf.namectxO nn value in
        (move,Some {heap; 
              ienv = Int.Actions.Lang.concat_ienv ienv' aconf.ienv; 
              namectxP = Pmap.concat namectxP' aconf.namectxP; 
              namectxO = aconf.namectxO})

  let o_trans pconf  =
    let* (omove,lnamectx) = M.para_list (Int.Actions.generate_input_action pconf.namectxP) in
    let computation = Int.Actions.generate_computation pconf.ienv omove in
    return (omove,
            {computation; heap = pconf.heap; ienv = pconf.ienv;
            namectxO = Pmap.concat lnamectx pconf.namectxO;
            namectxP = pconf.namectxP})

  let init_aconf computation namectxO =
    {computation;
    heap = Int.Actions.Lang.empty_resources;
    ienv = Int.Actions.Lang.empty_ienv;
    namectxO;
    namectxP = Pmap.empty}

  let init_pconf ienv namectxP namectxO =
    {heap = Int.Actions.Lang.empty_resources;
    ienv;
    namectxO ;
    namectxP}

  let equiv_aconf aconf aconfb =
    (aconf.computation = aconfb.computation) && (aconf.heap = aconfb.heap) 
    
end