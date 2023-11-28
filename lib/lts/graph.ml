module type GRAPH  = functor (IntLTS : Bipartite.LTS) -> sig
  (* to be instantiated*)

  (* *)
  type id_state = int

  type state =
    | ActState of IntLTS.active_conf * id_state
    | PasState of IntLTS.passive_conf * id_state

  val string_of_state : state -> string

  type transition =
    | PublicTrans of state * IntLTS.Actions.Moves.move * state
    | Divergent of state

  val string_of_transition : transition -> string

  type graph = {
    states: state list;
    failed_states: state list;
    edges: transition list;
  }

  val string_of_graph : graph -> string
  val empty_graph : graph
  val compute_graph : IntLTS.active_conf -> graph
end

module Graph :
  GRAPH = functor (IntLTS : Bipartite.LTS) -> struct

  type id_state = int

  let string_of_id_state = string_of_int
  let count_id_state = ref 0

  let fresh_id_state () =
    let x = !count_id_state in
    count_id_state := !count_id_state + 1;
    x

  type state =
    | ActState of IntLTS.active_conf * id_state
    | PasState of IntLTS.passive_conf * id_state

  let dotstring_of_state failed_states = function
    | ActState (_, id) as state when List.mem state failed_states ->
        let id_string = string_of_id_state id in
        id_string ^ "[shape = diamond, label=\"" ^ id_string ^ "\"];"
    | ActState (_, id) ->
        let id_string = string_of_id_state id in
        id_string ^ "[shape = circle, color=blue, label=\"" ^ id_string ^ "\"];"
    | PasState (_, id) ->
        let id_string = string_of_id_state id in
        id_string ^ "[shape = circle, color=red, label=\"" ^ id_string ^ "\"];"

  let string_of_state = function
    | ActState (aconf, id) ->
        IntLTS.string_of_active_conf aconf ^ "_" ^ string_of_id_state id
    | PasState (pconf, id) ->
        IntLTS.string_of_passive_conf pconf ^ "_" ^ string_of_id_state id

  let idstring_of_state = function
    | ActState (_, id) | PasState (_, id) -> string_of_id_state id

  type transition =
    | PublicTrans of state * IntLTS.Actions.Moves.move * state
    | Divergent of state

  let string_of_transition = function
    | Divergent _ -> "" (*idstring_of_state st ^ "-> Boom"*)
    | PublicTrans (st1, act, st2) ->
        idstring_of_state st1 ^ " -> " ^ idstring_of_state st2
        ^ "[color=blue, label=\""
        ^ IntLTS.Actions.Moves.string_of_move act
        ^ "\"];"

  type graph = {
    states: state list;
    failed_states: state list;
    edges: transition list;
  }

  let string_of_graph { states; failed_states; edges } =
    let states_string =
      String.concat "\n" (List.map (dotstring_of_state failed_states) states)
    in
    let edges_string =
      String.concat "\n" (List.map string_of_transition edges) in
    "//DOT \n digraph R {\n" ^ states_string ^ "\n" ^ edges_string ^ "\n}\n"

  let empty_graph = { states= []; failed_states= []; edges= [] }

  include Util.Monad.BranchState (struct type t = graph end)

  let equiv_act_state act_conf act_state =
    match act_state with
    | ActState (act_conf', _) -> IntLTS.equiv_act_conf act_conf act_conf'
    | PasState _ -> false

  let find_equiv_act_conf act_conf : state option m =
    let* graph = get () in
    return (List.find_opt (equiv_act_state act_conf) graph.states)

  let add_state state =
    let* graph = get () in
    set { graph with states= state :: graph.states }

  let add_failed_state state =
    let* graph = get () in
    set { graph with failed_states= state :: graph.failed_states }

  let add_act_state act_conf =
    let id = fresh_id_state () in
    let act_state = ActState (act_conf, id) in
    let* () = add_state act_state in
    return act_state

  let add_pas_state act_conf =
    let id = fresh_id_state () in
    let act_state = PasState (act_conf, id) in
    let* () = add_state act_state in
    return act_state

  let add_edge edge : unit m =
    let* graph = get () in
    set { graph with edges= edge :: graph.edges }

  (* The computation of the graph is always called on an active state*)

  let rec compute_graph_monad = function
    | ActState (act_conf, _) as act_state ->
        let (action, pas_conf_option) = IntLTS.p_trans act_conf in
        begin
          match (action, pas_conf_option) with
          | (PDiv, None) ->
              let* act_state = add_act_state act_conf in
              add_edge (Divergent act_state)
          | (PError, None) -> add_failed_state act_state
          | (Vis pmove, Some pas_conf) ->
              let* pas_state = add_pas_state pas_conf in
              let edge = PublicTrans (act_state, pmove, pas_state) in
              Util.Debug.print_debug
                ("Adding the transition: " ^ string_of_transition edge);
              let* () = add_edge edge in
              compute_graph_monad pas_state
          | (PDiv, Some _) | (PError, Some _) | (Vis _, None) ->
              failwith
                "Error: impossible transition in the graph. Please report."
        end
    | PasState (pas_conf, _) as pas_state ->
        let* (input_move, act_conf) =
          para_list (IntLTS.M.run (IntLTS.o_trans_gen pas_conf)) in
        let* act_state_option = find_equiv_act_conf act_conf in
        begin
          match act_state_option with
          | None ->
              let* act_state = add_act_state act_conf in
              let edge = PublicTrans (pas_state, input_move, act_state) in
              let* () = add_edge edge in
              compute_graph_monad act_state
          | Some act_state ->
              Util.Debug.print_debug
                ("Loop detected: \n   "
                ^ IntLTS.string_of_active_conf act_conf
                ^ "\n  " ^ string_of_state act_state);
              let edge = PublicTrans (pas_state, input_move, act_state) in
              add_edge edge
        end

  let compute_graph_m init_aconf =
    let* init_state = add_act_state init_aconf in
    compute_graph_monad init_state

  let compute_graph init_aconf =
    let comp = compute_graph_m init_aconf in
    let (_, graph) = runState comp empty_graph in
    graph
end
