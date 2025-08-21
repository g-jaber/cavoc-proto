module type RENAMING = sig
  module Namectx : Typectx.TYPECTX

  type t

  val id : Namectx.t -> t
  val dom : t -> Namectx.t
  val im : t -> Namectx.t
  val weak_l : Namectx.t -> Namectx.t -> t
  val weak_r : Namectx.t -> Namectx.t -> t
  val sym : Namectx.t -> Namectx.t -> t

  val lookup : t -> Namectx.name -> Namectx.name
end

module Make (Namectx : Typectx.TYPECTX_PMAP) : RENAMING = struct
  module Namectx = Namectx

  type t = {
    map: (Namectx.name, Namectx.name) Util.Pmap.pmap;
    dom: Namectx.t;
    im: Namectx.t;
  }

  let id namectx =
    let map = Util.Pmap.map (fun (nn, _) -> (nn, nn)) namectx in
    { map; dom= namectx; im= namectx }

  let dom renam = renam.dom
  let im renam = renam.im

  let weak_l namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Util.Pmap.concat namectx_l namectx_r }

  let weak_r namectx_l namectx_r =
    let renam = id namectx_r in
    { renam with im= Util.Pmap.concat namectx_l namectx_r }

  let sym _namectx_l _namectx_r = failwith ""

  let lookup renam nn = Util.Pmap.lookup_exn nn renam.map
end
