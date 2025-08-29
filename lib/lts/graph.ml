module type GRAPH = sig
  (* To be instanciated *)
  type conf
  type move

  (* *)
  type graph

  val string_of_graph : graph -> string

  val compute_graph :
    show_conf:(string -> unit) ->
    show_moves_list:(string list -> unit) ->
    get_move:(int -> int) ->
    conf ->
    graph
end

module Make (IntLTS : Bipartite.LTS) :
  GRAPH with type conf = IntLTS.conf and type move = IntLTS.Moves.pol_move =
struct
  type conf = IntLTS.conf
  type move = IntLTS.Moves.pol_move
  type id_state = int

  let string_of_id_state = string_of_int
  let count_id_state = ref 0

  let fresh_id_state () =
    let x = !count_id_state in
    count_id_state := !count_id_state + 1;
    x

  type state = IntLTS.conf * id_state
  (* | ActState of IntLTS.active_conf * id_state
       | PasState of IntLTS.passive_conf * id_state*)

  let dotstring_of_state failed_states = function
    | (IntLTS.Active _, id) as state when List.mem state failed_states ->
        let id_string = string_of_id_state id in
        id_string ^ "[shape = diamond, label=\"" ^ id_string ^ "\"];"
    | (IntLTS.Active _, id) ->
        let id_string = string_of_id_state id in
        id_string ^ "[shape = circle, color=blue, label=\"" ^ id_string ^ "\"];"
    | (IntLTS.Passive _, id) ->
        let id_string = string_of_id_state id in
        id_string ^ "[shape = circle, color=red, label=\"" ^ id_string ^ "\"];"

  let string_of_state = function
    | (IntLTS.Active aconf, id) ->
        IntLTS.string_of_active_conf aconf ^ "_" ^ string_of_id_state id
    | (IntLTS.Passive pconf, id) ->
        IntLTS.string_of_passive_conf pconf ^ "_" ^ string_of_id_state id

  let idstring_of_state (_, id) = string_of_id_state id

  type transition = PublicTrans of state * IntLTS.Moves.pol_move * state

  let string_of_transition = function
    | PublicTrans (st1, act, st2) ->
        idstring_of_state st1 ^ " -> " ^ idstring_of_state st2
        ^ "[color=blue, label=\""
        ^ IntLTS.Moves.string_of_pol_move act
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

  let equiv_act_state act_conf state =
    match state with
    | (IntLTS.Active act_conf', _) -> IntLTS.equiv_act_conf act_conf act_conf'
    | (IntLTS.Passive _, _) -> false

  let find_equiv_act_conf act_conf : state option m =
    let* graph = get () in
    return (List.find_opt (equiv_act_state act_conf) graph.states)

  let add_state state =
    let* graph = get () in
    set { graph with states= state :: graph.states }

  let add_failed_state state =
    let* graph = get () in
    set { graph with failed_states= state :: graph.failed_states }

  let add_conf conf =
    let id = fresh_id_state () in
    let act_state = (conf, id) in
    let* () = add_state act_state in
    return act_state

  let add_act_state act_conf =
    let id = fresh_id_state () in
    let act_state = (IntLTS.Active act_conf, id) in
    let* () = add_state act_state in
    return act_state

  let add_pas_state act_conf =
    let id = fresh_id_state () in
    let act_state = (IntLTS.Passive act_conf, id) in
    let* () = add_state act_state in
    return act_state

  let add_edge edge : unit m =
    let* graph = get () in
    set { graph with edges= edge :: graph.edges }

  (* The computation of the graph is always called on an active state*)
  (* TODO: Why ? *)
  let rec compute_graph_monad ~show_conf ~show_moves_list ~get_move = function
    | (IntLTS.Active act_conf, _) as act_state -> begin
        match IntLTS.EvalMonad.run (IntLTS.p_trans act_conf) with
        | None -> add_failed_state act_state
        | Some (pmove, pas_conf) ->
            let* pas_state = add_pas_state pas_conf in
            let edge = PublicTrans (act_state, pmove, pas_state) in
            Util.Debug.print_debug
              ("Adding the transition: " ^ string_of_transition edge);
            let* () = add_edge edge in
            compute_graph_monad ~show_conf ~show_moves_list ~get_move pas_state
      end
    | (IntLTS.Passive pas_conf, _) as pas_state ->
        let* (input_move, act_conf) =
          para_list (IntLTS.OBranchingMonad.run (IntLTS.o_trans_gen pas_conf))
        in
        let* act_state_option = find_equiv_act_conf act_conf in
        begin
          match act_state_option with
          | None ->
              let* act_state = add_act_state act_conf in
              let edge = PublicTrans (pas_state, input_move, act_state) in
              let* () = add_edge edge in
              compute_graph_monad ~show_conf ~show_moves_list ~get_move
                act_state
          | Some act_state ->
              Util.Debug.print_debug
                ("Loop detected: \n   "
                ^ IntLTS.string_of_active_conf act_conf
                ^ "\n  " ^ string_of_state act_state);
              let edge = PublicTrans (pas_state, input_move, act_state) in
              add_edge edge
        end

  let compute_graph_m ~show_conf ~show_moves_list ~get_move init_conf =
    let* init_state = add_conf init_conf in
    compute_graph_monad ~show_conf ~show_moves_list ~get_move init_state

  let compute_graph ~show_conf ~show_moves_list ~get_move init_conf =
    let comp = compute_graph_m ~show_conf ~show_moves_list ~get_move init_conf in
    let (_, graph) = runState comp empty_graph in
    graph
end
