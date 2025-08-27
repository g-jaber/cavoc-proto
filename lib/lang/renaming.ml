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
end

module Make (Namectx : Typectx.TYPECTX with type name = int * string) :
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
    let names_l = Namectx.get_names namectx_r in
    let length = List.length @@ Util.Pmap.to_list @@ Namectx.to_pmap namectx_r in
    Util.Debug.print_debug @@ "Calling weak_r with an offset of "
    ^ string_of_int length;
    let map =
      Util.Pmap.list_to_pmap
      @@ List.map (fun ((i, str) as nn) -> (nn, (i + length, str))) names_l
    in
    { map; dom= namectx_l; im= Namectx.concat namectx_r namectx_l }

  let sym _namectx_l _namectx_r = failwith "TODO"

  let lookup renam ((i, str) as nn) =
    try Util.Pmap.lookup_exn nn renam.map
    with Not_found ->
      Util.Debug.print_debug @@ "The name " ^ string_of_int i ^ str
      ^ " was not found";
      nn
end

module MakeAggregate (Namectx1 : Typectx.TYPECTX) (Namectx2 : Typectx.TYPECTX) :
  RENAMING
    with type Namectx.name = (Namectx1.name, Namectx2.name) Either.t
     and type Namectx.t = Namectx1.t * Namectx2.t = struct
  module Namectx = Typectx.Aggregate (Namectx1) (Namectx2)

  type t = {
    map_l: (Namectx1.name, Namectx1.name) Util.Pmap.pmap;
    map_r: (Namectx2.name, Namectx2.name) Util.Pmap.pmap;
    dom: Namectx.t;
    im: Namectx.t;
  }

  let id ((namectx1, namectx2) as namectx) =
    let names1_list = Namectx1.get_names namectx1 in
    let names2_list = Namectx2.get_names namectx2 in
    let map_l =
      Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, nn)) names1_list in
    let map_r =
      Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, nn)) names2_list in
    { map_l; map_r; dom= namectx; im= namectx }

  let dom renam = renam.dom
  let im renam = renam.im

  let weak_l namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Namectx.concat namectx_l namectx_r }

  let weak_r namectx_l namectx_r =
    let renam = id namectx_r in
    { renam with im= Namectx.concat namectx_l namectx_r }

  let sym _namectx_l _namectx_r = failwith "TODO"

  let lookup renam nn =
    try
      match nn with
      | Either.Left nn' -> Either.Left (Util.Pmap.lookup_exn nn' renam.map_l)
      | Either.Right nn' -> Either.Right (Util.Pmap.lookup_exn nn' renam.map_r)
    with Not_found -> nn
end

module Aggregate (Renam1 : RENAMING) (Renam2 : RENAMING) :
  RENAMING
    with type Namectx.name = (Renam1.Namectx.name, Renam2.Namectx.name) Either.t
     and type Namectx.t = Renam1.Namectx.t * Renam2.Namectx.t
     and type t = Renam1.t * Renam2.t = struct
  module Namectx = Typectx.Aggregate (Renam1.Namectx) (Renam2.Namectx)

  type t = Renam1.t * Renam2.t

  let id (namectx1, namectx2) =
    let id1 = Renam1.id namectx1 in
    let id2 = Renam2.id namectx2 in
    (id1, id2)

  let dom (renam1, renam2) = (Renam1.dom renam1, Renam2.dom renam2)
  let im (renam1, renam2) = (Renam1.im renam1, Renam2.im renam2)

  let weak_l (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    let map1 = Renam1.weak_l namectx1_l namectx1_r in
    let map2 = Renam2.weak_l namectx2_l namectx2_r in
    (map1, map2)

  let weak_r (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    let map1 = Renam1.weak_r namectx1_l namectx1_r in
    let map2 = Renam2.weak_r namectx2_l namectx2_r in
    (map1, map2)

  let sym _namectx_l _namectx_r = failwith "TODO"

  let lookup (renam1, renam2) nn =
    try
      match nn with
      | Either.Left nn' -> Either.Left (Renam1.lookup renam1 nn')
      | Either.Right nn' -> Either.Right (Renam2.lookup renam2 nn')
    with Not_found -> nn
end
