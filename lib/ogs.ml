module OGS = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) -> struct

include M
module Moves = Moves.Moves(Lang)

type active_conf = 
  { computation : Lang.computation;
    heap : Lang.resources;
    ienv : Lang.interactive_env;
    namectxO : Lang.name_type_ctx;
    namectxP : Lang.name_type_ctx }

type passive_conf = 
  { heap : Lang.resources;
    ienv : Lang.interactive_env;
    namectxO : Lang.name_type_ctx;
    namectxP : Lang.name_type_ctx }

let string_of_active_conf aconf =
  "<" 
  ^ Lang.string_of_computation aconf.computation 
  ^ " | " 
  ^ Lang.string_of_resources aconf.heap
  ^ " | " 
  ^ Lang.string_of_ienv aconf.ienv 
  ^ " | " 
  ^ Lang.string_of_name_type_ctx aconf.namectxO
  ^ " | " 
  ^ Lang.string_of_name_type_ctx aconf.namectxP
  ^ ">"

let string_of_passive_conf pconf =
  "<"   
  ^ Lang.string_of_resources pconf.heap
  ^ " | " 
  ^ Lang.string_of_ienv pconf.ienv 
  ^ " | " 
  ^ Lang.string_of_name_type_ctx pconf.namectxO
  ^ " | " 
  ^ Lang.string_of_name_type_ctx pconf.namectxP
  ^ ">"

let p_trans aconf =
  let opconf_option = Lang.compute_nf (aconf.computation,aconf.heap) in
  match opconf_option with
    | None -> (Moves.PDiv, None)
    | Some ((_,heap) as opconf) -> 
      let (nn,value) = Lang.decompose_nf opconf in
      let (move,ienv',namectxP') = Moves.generate_pmove aconf.namectxO nn value in
      (move,Some {heap; 
            ienv = Lang.concat_ienv ienv' aconf.ienv; 
            namectxP = Pmap.concat namectxP' aconf.namectxP; 
            namectxO = aconf.namectxO})

let o_trans pconf  =
  let* (omove,lnamectx) = M.para_list (Moves.generate_omove pconf.namectxP) in
  let computation = Moves.generate_term pconf.ienv omove in
  return (omove,
          {computation; heap = pconf.heap; ienv = pconf.ienv;
           namectxO = Pmap.concat lnamectx pconf.namectxO;
           namectxP = pconf.namectxP})

let init_aconf computation namectxO =
  {computation;
   heap = Lang.empty_resources;
   ienv = Lang.empty_ienv;
   namectxO;
   namectxP = Pmap.empty}

let init_pconf ienv namectxP namectxO =
  {heap = Lang.empty_resources;
   ienv;
   namectxO ;
   namectxP}

let equiv_aconf aconf aconfb =
  (aconf.computation = aconfb.computation) && (aconf.heap = aconfb.heap) 
  
end