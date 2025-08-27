module type RENAMING = sig
  module Namectx : Typectx.TYPECTX

  type t

  val id : Namectx.t -> t
  val dom : t -> Namectx.t
  val im : t -> Namectx.t

  (* weak_l Δ Γ : Δ → Δ + Γ*)
  val weak_l : Namectx.t -> Namectx.t -> t

  (* weak_r Δ Γ : Γ → Δ + Γ*)
  val weak_r : Namectx.t -> Namectx.t -> t

  (* sym Δ Γ : Δ + Γ → Γ + Δ*)
  val sym : Namectx.t -> Namectx.t -> t
  val lookup : t -> Namectx.name -> Namectx.name
  val fold : ('a -> Namectx.name * Namectx.name -> 'a) -> 'a -> t -> 'a
end

module Make (Namectx : Typectx.TYPECTX) :
  RENAMING with module Namectx = Namectx = struct
  module Namectx = Namectx

  type t = {
    map: (Namectx.name, Namectx.name) Util.Pmap.pmap;
    dom: Namectx.t;
    im: Namectx.t;
  }

  let id namectx =
    let names_l = Namectx.get_names namectx in
    let map = Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, nn)) names_l in
    { map; dom= namectx; im= namectx }

  let dom renam = renam.dom
  let im renam = renam.im

  let weak_l namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Namectx.concat namectx_l namectx_r }

  let weak_r namectx_l namectx_r =
    let renam = id namectx_r in
    { renam with im= Namectx.concat namectx_l namectx_r }

  let sym _namectx_l _namectx_r = failwith ""
  let lookup renam nn = Util.Pmap.lookup_exn nn renam.map
  let fold f v renam = Util.Pmap.fold f v renam.map
end
