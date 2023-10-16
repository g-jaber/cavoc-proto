module Make = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) (OGSM : Ogssig.OGS) -> struct

module Moves = Moves.Moves(Lang)
include Monad.LWMonad(struct type t = (Moves.action) end)
module OGS = OGSM(Lang)(M)

let rec synchronize nspan act_conf pas_conf =
  let (pmove,pas_conf_option) = OGS.p_trans act_conf in
  match pas_conf_option with
    | None -> print pmove
    | Some pas_conf' ->
      let* (omove,act_conf') = para_list (M.run (OGS.o_trans pas_conf)) in
      let nspan_option = Moves.unify_action nspan pmove omove in
      begin match nspan_option with
        | None -> fail ()
        | Some nspan' -> let* () = print pmove in (synchronize nspan' act_conf' pas_conf')
      end

let get_traces act_conf pas_conf =
  let result = synchronize Namespan.empty_nspan act_conf pas_conf in
  let trace_list = get_output result in
  List.map (fun outputl -> String.concat "," @@ List.map Moves.string_of_action outputl) trace_list
end