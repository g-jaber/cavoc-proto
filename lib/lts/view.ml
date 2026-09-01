(* A view is an order-preserving inclusion of the names visible in it into
   the participant's current context. *)
(* Its domain gives the canonical view-local levels, its image their meaning
   in that context. *)

module Make (Thinning : sig
  include Lang.Renaming.THINNING

  val of_support : Namectx.t -> Namectx.Names.name list -> t
end) =
struct
  module Namectx = Thinning.Namectx

  type t = Thinning.t

  let support view =
    List.map (Thinning.lookup view) (Namectx.get_names (Thinning.dom view))

  let contains view context_name =
    Option.is_some (Thinning.lookup_inv view context_name)

  let to_yojson view =
    `Assoc
      [
        ("localContext", Namectx.to_yojson (Thinning.dom view));
        ("context", Namectx.to_yojson (Thinning.im view));
        ("support", `List (List.map Namectx.Names.name_to_yojson (support view)));
      ]

  (* The view's visible names read in [context]: the view post-composed with
     the inclusion of its image into [context]. *)
  (* Its domain is rebuilt from [context], so display hints may differ between
     the two. *)
  let transport_to_context ~context view =
    Thinning.of_support context (support view)

  let extend_visible_support ~fresh view =
    Thinning.of_support (Thinning.im view) (support view @ fresh)

  (* The view map records, at each name, the view of the other participant
     current when the name was introduced. *)
  type view_map = (Namectx.Names.name, Thinning.t) Util.Pmap.pmap

  let pp_view_map fmt view_map =
    let pp_empty fmt () = Format.pp_print_char fmt '.' in
    let pp_pair fmt (name, view) =
      Format.fprintf fmt "%a ↦ %a" Namectx.Names.pp_name name Thinning.pp view
    in
    Util.Pmap.pp_pmap ~pp_empty pp_pair fmt view_map

  let view_map_to_yojson view_map =
    let to_entry (name, view) =
      (Namectx.Names.string_of_name name, to_yojson view) in
    `Assoc (List.map to_entry (Util.Pmap.to_list view_map))

  let init_view_map view names =
    Util.Pmap.list_to_pmap @@ List.map (fun name -> (name, view)) names

  let record_view_at_introduction view_map view fresh =
    Util.Pmap.concat view_map (init_view_map view fresh)

  let restore_view recorded_view ~context ~fresh =
    extend_visible_support ~fresh (transport_to_context ~context recorded_view)

  let restore_view_at_subject view_map subject ~context ~fresh =
    match Util.Pmap.lookup subject view_map with
    | Some recorded_view -> restore_view recorded_view ~context ~fresh
    | None ->
        Util.Error.failwithf
          "Error: the name %a is not in the view map %a. Please report."
          Namectx.Names.pp_name subject pp_view_map view_map
end
