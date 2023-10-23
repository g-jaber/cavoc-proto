module type GRAPH  = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) (BILTSM : Ogssig.BILTS) -> sig
  type state
  val string_of_state : state -> string
  type transition
  val string_of_transition : transition -> string
  type graph = {
    states : state list;
    edges : transition list;
  }
  val string_of_graph : graph -> string
  val empty_graph : graph
  val compute_graph : BILTSM(Lang)(M).active_conf -> graph
end

module Graph : GRAPH = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) (BILTSM : Ogssig.BILTS) -> struct

module BILTS = BILTSM(Lang)(M)
module Moves = Moves.Moves(Lang)

type id_state = int

let string_of_id_state = string_of_int

let count_id_state = ref 0

let fresh_id_state () =
  let x = !count_id_state in
  count_id_state := !count_id_state + 1;x

type state = 
  | ActState of BILTS.active_conf*id_state
  | PasState of BILTS.passive_conf*id_state

let string_of_state = function
  | ActState (aconf,id) -> (BILTS.string_of_active_conf aconf) ^ "_" ^ (string_of_id_state id)
  | PasState (pconf,id) -> (BILTS.string_of_passive_conf pconf) ^ "_" ^ (string_of_id_state id)

let idstring_of_state = function
| ActState (_,id) | PasState (_,id) -> (string_of_id_state id)

type transition =
| PublicTrans of state * (Moves.action) *state
| Divergent of state

let string_of_transition = function
| Divergent st ->
  (idstring_of_state st) ^"-> Boom"
| PublicTrans (st1,act,st2) ->
  (idstring_of_state st1) ^ " -"^ (Moves.string_of_action act) ^"-> " ^ (idstring_of_state st2)

type graph = { states : state list;
             edges : transition list}

let string_of_graph {states; edges} =
  let states_string = String.concat ",\n " (List.map string_of_state states) in
  let edges_string =  String.concat ", " (List.map string_of_transition edges) in
  "{"^ states_string ^"}\n" ^ "{"^ edges_string ^"}\n"

let empty_graph = { states = []; edges = []}

include Monad.LStMonad(struct type t = graph end)


let equiv_act_state act_conf act_state =
  match act_state with
  | ActState (act_conf',_) ->  BILTS.equiv_aconf act_conf act_conf'
  | PasState _ -> false


let find_equiv_aconf act_conf : (state option) m = 
  let* graph = get () in
  return (List.find_opt (equiv_act_state act_conf) graph.states)

let add_state st : unit m = 
  let* graph = get () in
  set {graph with states = st::(graph.states)}

let add_edge edge : unit m =
  let* graph = get () in
  set {graph with edges = edge::(graph.edges)}

(* The computation of the graph is always called on an active state*)

let rec compute_graph_monad act_conf : unit m =
  Debug.print_debug "Computing the GRAPH";
  let id = fresh_id_state () in
  let act_state = ActState (act_conf,id) in
  let* () = add_state act_state in
  Debug.print_debug ("Adding the active state: " ^ (string_of_state act_state));
  let (pmove,pas_conf_option) = BILTS.p_trans act_conf in
  match pas_conf_option with
    | None -> 
      Debug.print_debug "We are diverging!";
      add_edge (Divergent act_state)
    | Some pas_conf ->
      let id' = fresh_id_state () in
      let pas_state = PasState (pas_conf,id') in
      Debug.print_debug ("Adding the passive configuration: " ^ (string_of_state pas_state));
      let* () = add_state pas_state in
      let edge = PublicTrans (act_state,pmove,pas_state) in
      Debug.print_debug ("Adding the transition: " ^ (string_of_transition edge));
      let* () = add_edge edge in
      let* (omove,act_conf') = para_list (M.run (BILTS.o_trans pas_conf)) in
      let* act_state_option = find_equiv_aconf act_conf' in
      begin match act_state_option with
        | None -> 
          let id'' = fresh_id_state () in
          let act_state' = ActState (act_conf',id'') in
          let edge = PublicTrans(pas_state,omove,act_state') in
          let* () = add_edge edge in
          compute_graph_monad act_conf'
        | Some act_state'' ->
          Debug.print_debug ("Loop detected: \n   " ^ (BILTS.string_of_active_conf act_conf') ^ "\n  " ^   (string_of_state act_state''));
          let edge = PublicTrans (pas_state,omove,act_state'') in
          add_edge edge
      end


let compute_graph aconf =
  let comp = compute_graph_monad aconf in
  let (_,graph) = runState comp empty_graph in
  graph
end