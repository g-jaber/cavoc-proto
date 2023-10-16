module POGS = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) -> struct

include M
module Moves = Moves.Moves(Lang)

type active_conf = 
  { computation : Lang.computation;
    heap : Lang.resources;
    loc_ctx : Lang.resources_type_ctx; 
    namectxO : Lang.name_type_ctx }

type passive_conf = 
  { ienv : Lang.interactive_env;
    loc_ctx : Lang.resources_type_ctx; 
    namectxO : Lang.name_type_ctx;
    namectxP : Lang.name_type_ctx }

let string_of_active_conf aconf =
  "<" 
  ^ Lang.string_of_computation aconf.computation
  ^ " | " 
  ^ Lang.string_of_resources aconf.heap
  ^ " | " 
  ^ Lang.string_of_name_type_ctx aconf.namectxO
  ^ ">"

let string_of_passive_conf pconf =
  "<" 
  ^ Lang.string_of_ienv pconf.ienv 
  ^ " | " 
  ^ Lang.string_of_resources_type_ctx pconf.loc_ctx
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
      let loc_ctx = Lang.resources_type_ctx_of_resources heap in
      let (move,ienv,namectxP) = Moves.generate_pmove aconf.namectxO nn value in
      (move,Some {loc_ctx; ienv; namectxP; namectxO = aconf.namectxO})

let o_trans pconf  =
  let* (omove,lnamectx) = M.para_list (Moves.generate_omove pconf.namectxP) in
  let* heap = M.para_list (Lang.generate_resources pconf.loc_ctx) in
  let computation = Moves.generate_term pconf.ienv omove in
  return (omove,
          {computation; heap;
           loc_ctx = pconf.loc_ctx;
           namectxO = Pmap.concat lnamectx pconf.namectxO})

let init_aconf computation namectxO =
  {computation;
   heap = Lang.empty_resources;
   loc_ctx = Lang.empty_resources_type_ctx; 
   namectxO}

let init_pconf ienv namectxO namectxP =
  {loc_ctx = Lang.empty_resources_type_ctx;
   ienv;
   namectxO ;
   namectxP}

let equiv_aconf aconf aconfb =
  (aconf.computation = aconfb.computation) && (aconf.heap = aconfb.heap) 
  
end