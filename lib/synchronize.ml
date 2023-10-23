module Make = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) (BILTSM : Ogssig.BILTS) -> struct

module Moves = Moves.Moves(Lang)
include Monad.LWMonad(struct type t = (Moves.action) end)
module BILTS = BILTSM(Lang)(M)

let rec synchronize nspan act_conf pas_conf =
  Debug.print_debug "One step of synchronize";
  let (pmove,pas_conf_option) = BILTS.p_trans act_conf in
  match pas_conf_option with
    | None -> Debug.print_debug "Stopping synchronization"; print pmove
    | Some pas_conf' ->
      let* (omove,act_conf') = para_list (M.run (BILTS.o_trans pas_conf)) in
      Debug.print_debug ("Synching moves " ^ (Moves.string_of_action pmove) ^" and " ^ (Moves.string_of_action omove)); 
      let nspan_option = Moves.synch_action nspan pmove omove in
      begin match nspan_option with
        | None -> Debug.print_debug ("Synching failed in namespan " ^ (Pmap.string_of_pmap "" "," Lang.string_of_name Lang.string_of_name) nspan);
          return ()
        | Some nspan' -> Debug.print_debug "Synching succeeded"; let* () = print pmove in (synchronize nspan' act_conf' pas_conf')
      end

let get_traces namespan act_conf pas_conf =
  let result = synchronize namespan act_conf pas_conf in
  let trace_list = get_output result in
  List.map (fun outputl -> String.concat "," @@ List.map Moves.string_of_action outputl) trace_list
end