(* A view scope is a typed, order-preserving inclusion of the names visible in
   the view into the current Proponent context.  Its domain gives the
   canonical view-local levels; its image gives their meaning in that context. *)

module Make (Weakening : Lang.Renaming.WEAKENING) = struct
  module Thinning = Lang.Renaming.MakeThin (Weakening)
  module Namectx = Thinning.Namectx

  type t = Thinning.t

  let pp = Thinning.pp
  let to_string = Thinning.to_string
  let local_ctx = Thinning.dom
  let context = Thinning.im
  let empty context = Thinning.of_support context []
  let full = Thinning.id
  let of_support = Thinning.of_support
  let to_local = Thinning.to_local
  let to_context = Thinning.lookup

  let support view =
    List.map (to_context view) (Namectx.get_names (local_ctx view))

  let contains view context_name = Option.is_some (to_local view context_name)

  let to_yojson view =
    `Assoc
      [
        ("localContext", Namectx.to_yojson (local_ctx view));
        ("context", Namectx.to_yojson (context view));
        ("support", `List (List.map Namectx.Names.name_to_yojson (support view)));
      ]

  let check_retained_types ~context view =
    let context_names = Namectx.get_names context in
    List.for_all
      (fun local ->
        let context_name = to_context view local in
        List.mem context_name context_names
        && Namectx.lookup_exn (local_ctx view) local
           = Namectx.lookup_exn context context_name)
      (Namectx.get_names (local_ctx view))

  let change_context ~context view =
    if not (check_retained_types ~context view) then
      failwith "View.change_context: the context does not preserve the view";
    Thinning.of_support context (support view)

  let extend_visible_support ~fresh view =
    Thinning.of_support (context view) (support view @ fresh)
end
