module type IENV = sig
  module Renaming : Renaming.RENAMING

  type value

  val embed_name : Renaming.Namectx.Names.name -> value

  type t [@@deriving to_yojson]

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val empty : t
  val dom : t -> Renaming.Namectx.t
  val im : t -> Renaming.Namectx.t

  (* Taking γ₁ : Γ₁ → Δ and γ₂ : Γ₂ → Δ, pairing γ₁ γ₂ : (Γ₁ + Γ₂) → Δ  *)
  val copairing : t -> t -> t

  (* Taking γ : Γ → Δ and Θ, then weaken γ Θ : Γ → Δ + Θ *)
  (*val weaken : t -> Namectx.t -> t*)
  val lookup_exn : t -> Renaming.Namectx.Names.name -> value

  val add_fresh :
    t ->
    string ->
    Renaming.Namectx.typ ->
    value ->
    Renaming.Namectx.Names.name * t

  val map : (value -> value) -> t -> t
  val fold : ('a -> Renaming.Namectx.Names.name * value -> 'a) -> 'a -> t -> 'a
end

module Make_PMAP
    (Renaming : Renaming.RENAMING)
    (Value : sig
      type t [@@deriving to_yojson]

      val embed_name : Renaming.Namectx.Names.name -> t
      val pp : Format.formatter -> t -> unit
    end) : IENV with module Renaming = Renaming and type value = Value.t =
struct
  module Renaming = Renaming

  type value = Value.t

  let embed_name = Value.embed_name

  type t = {
    map: (Renaming.Namectx.Names.name, Value.t) Util.Pmap.pmap;
    dom: Renaming.Namectx.t;
    im: Renaming.Namectx.t;
  }

  let empty =
    {
      map= Util.Pmap.empty;
      dom= Renaming.Namectx.empty;
      im= Renaming.Namectx.empty;
    }

  let dom ienv = ienv.dom
  let im ienv = ienv.im

  let copairing ienv1 ienv2 =
    assert (ienv1.im = ienv2.im);
    {
      map= Util.Pmap.concat ienv1.map ienv2.map;
      dom= Renaming.Namectx.concat ienv1.dom ienv2.dom;
      im= ienv1.im;
    }

  let pp fmt ienv =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, value) =
      Format.fprintf fmt "%a ↦ %a" Renaming.Namectx.Names.pp_name n Value.pp
        value in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt ienv.map

  let to_string = Format.asprintf "%a" pp

  let to_yojson ienv =
    let to_string (nn, value) =
      (Renaming.Namectx.Names.string_of_name nn, Value.to_yojson value) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string ienv.map)

  let lookup_exn ienv nn = Util.Pmap.lookup_exn nn ienv.map

  (* We do not check that nn is indeed the last name in the next function.*)
  let add_fresh ienv str ty value =
    let (nn, dom) = Renaming.Namectx.add_fresh ienv.dom str ty in
    let map = Util.Pmap.add (nn, value) ienv.map in
    (nn, { ienv with dom; map })

  let map f ienv =
    let map = Util.Pmap.map_im f ienv.map in
    { ienv with map }

  let fold f value ienv = Util.Pmap.fold f value ienv.map
end

module Make_List
    (Renaming : Renaming.RENAMING_LIST)
    (Values : sig
      type t [@@deriving to_yojson]

      val embed_name : Renaming.Namectx.Names.name -> t
      val pp : Format.formatter -> t -> unit
    end) : IENV with module Renaming = Renaming and type value = Values.t =
struct
  module Renaming = Renaming

  type value = Values.t

  let embed_name = Values.embed_name

  type t = {
    map: Values.t list;
    dom: Renaming.Namectx.t;
    im: Renaming.Namectx.t;
  }

  let empty =
    { map= []; dom= Renaming.Namectx.empty; im= Renaming.Namectx.empty }

  let dom ienv = ienv.dom
  let im ienv = ienv.im

  let copairing ienv1 ienv2 =
    assert (ienv1.im = ienv2.im);
    {
      map= List.append ienv1.map ienv2.map;
      dom= Renaming.Namectx.concat ienv1.dom ienv2.dom;
      im= ienv1.im;
    }

  let pp fmt ienv =
    match ienv.map with
    | [] -> Format.fprintf fmt "⋅"
    | map' ->
        let pp_sep fmt () = Format.fprintf fmt ", " in
        Format.pp_print_list ~pp_sep Values.pp fmt map'

  let to_string = Format.asprintf "%a" pp

  let to_yojson ienv =
    `List
      (List.mapi (fun i typ -> `List [ `Int i; Values.to_yojson typ ]) ienv.map)

  let lookup_exn ienv (i, _) = List.nth ienv.map i

  let add_fresh ienv str ty value =
    let (nn, dom) = Renaming.Namectx.add_fresh ienv.dom str ty in
    let map = ienv.map @ [ value ] in
    (nn, { ienv with map; dom })

  let map f ienv =
    let map = List.map f ienv.map in
    { ienv with map }

  let fold (f : 'a -> Renaming.Namectx.Names.name * value -> 'a) (v : 'a)
      (ienv : t) =
    List.fold_left f v (List.mapi (fun i ty -> ((i, ""), ty)) ienv.map)
end

module Aggregate
    (IEnv1 : IENV)
    (IEnv2 : IENV)
    (Renaming :
      Renaming.RENAMING
        with type Namectx.Names.name =
          ( IEnv1.Renaming.Namectx.Names.name,
            IEnv2.Renaming.Namectx.Names.name )
          Either.t
         and type Namectx.typ =
          (IEnv1.Renaming.Namectx.typ, IEnv2.Renaming.Namectx.typ) Either.t
         and type Namectx.t =
          IEnv1.Renaming.Namectx.t * IEnv2.Renaming.Namectx.t) :
  IENV
    with module Renaming = Renaming
     and type value = (IEnv1.value, IEnv2.value) Either.t
     and type t = IEnv1.t * IEnv2.t = struct
  module Renaming = Renaming

  type value = (IEnv1.value, IEnv2.value) Either.t

  let embed_name = function
    | Either.Left nn -> Either.Left (IEnv1.embed_name nn)
    | Either.Right nn -> Either.Right (IEnv2.embed_name nn)

  type t = IEnv1.t * IEnv2.t [@@deriving to_yojson]

  let empty = (IEnv1.empty, IEnv2.empty)
  let dom (ienv1, ienv2) = (IEnv1.dom ienv1, IEnv2.dom ienv2)
  let im (ienv1, ienv2) = (IEnv1.im ienv1, IEnv2.im ienv2)

  let copairing (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.copairing ienv11 ienv21, IEnv2.copairing ienv12 ienv22)

  let pp fmt (ienv1, ienv2) =
    Format.fprintf fmt "(%a,%a)" IEnv1.pp ienv1 IEnv2.pp ienv2

  let to_string = Format.asprintf "%a" pp

  let lookup_exn (ienv1, ienv2) nn =
    match nn with
    | Either.Left nn' -> Either.Left (IEnv1.lookup_exn ienv1 nn')
    | Either.Right nn' -> Either.Right (IEnv2.lookup_exn ienv2 nn')

  let add_fresh (ienv1, ienv2) str ty value =
    match (ty, value) with
    | (Either.Left ty', Either.Left val') ->
        let (nn, ienv1') = IEnv1.add_fresh ienv1 str ty' val' in
        (Either.left nn, (ienv1', ienv2))
    | (Either.Right ty', Either.Right val') ->
        let (nn, ienv2') = IEnv2.add_fresh ienv2 str ty' val' in
        (Either.Right nn, (ienv1, ienv2'))
    | _ ->
        failwith
          "Error while performing a last test on an aggregated context. Please \
           report."

  let map f (ienv1, ienv2) =
    let f1 ty =
      match f (Either.Left ty) with
      | Either.Left ty' -> ty'
      | Either.Right _ty' ->
          failwith
            "Using a map with a function that does not preserve components. \
             Please report." in
    let f2 ty =
      match f (Either.Right ty) with
      | Either.Right ty' -> ty'
      | Either.Left _ty' ->
          failwith
            "Using a map with a function that does not preserve components. \
             Please report." in
    (IEnv1.map f1 ienv1, IEnv2.map f2 ienv2)

  let fold f a (ienv1, ienv2) =
    let f_lift1 a (nn, value) = f a (Either.Left nn, Either.Left value) in
    let f_lift2 a (nn, value) = f a (Either.Right nn, Either.Right value) in
    let a' = IEnv1.fold f_lift1 a ienv1 in
    IEnv2.fold f_lift2 a' ienv2
end

module AggregateCommon
    (IEnv1 : IENV)
    (IEnv2 :
      IENV
        with type Renaming.Namectx.typ = IEnv1.Renaming.Namectx.typ
         and type value = IEnv1.value)
    (Renaming :
      Renaming.RENAMING
        with type Namectx.Names.name =
          ( IEnv1.Renaming.Namectx.Names.name,
            IEnv2.Renaming.Namectx.Names.name )
          Either.t
         and type Namectx.typ = IEnv1.Renaming.Namectx.typ
         and type Namectx.t =
          IEnv1.Renaming.Namectx.t * IEnv2.Renaming.Namectx.t)
    (EmbedNames : sig
      val embed1 :
        IEnv1.Renaming.Namectx.Names.name -> Renaming.Namectx.Names.name

      val embed2 :
        IEnv2.Renaming.Namectx.Names.name -> Renaming.Namectx.Names.name

      val extract1 :
        Renaming.Namectx.Names.name -> IEnv1.Renaming.Namectx.Names.name option

      val extract2 :
        Renaming.Namectx.Names.name -> IEnv2.Renaming.Namectx.Names.name option
    end)
    (ClassifyTyp : sig
      val classify : IEnv1.Renaming.Namectx.typ -> bool
    end) :
  IENV
    with module Renaming = Renaming
     and type value = IEnv1.value
     and type t = IEnv1.t * IEnv2.t = struct
  module Renaming = Renaming

  type value = IEnv1.value

  let embed_name nn =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (Some nn', None) -> IEnv1.embed_name nn'
    | (None, Some nn') -> IEnv2.embed_name nn'
    | _ ->
        failwith
          "Error while performing a lookup on an aggregated context. Please \
           report."

  type t = IEnv1.t * IEnv2.t [@@deriving to_yojson]

  let empty = (IEnv1.empty, IEnv2.empty)
  let dom (ienv1, ienv2) = (IEnv1.dom ienv1, IEnv2.dom ienv2)
  let im (ienv1, ienv2) = (IEnv1.im ienv1, IEnv2.im ienv2)

  let copairing (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.copairing ienv11 ienv21, IEnv2.copairing ienv12 ienv22)

  let pp fmt (ienv1, ienv2) =
    Format.fprintf fmt "(%a,%a)" IEnv1.pp ienv1 IEnv2.pp ienv2

  let to_string = Format.asprintf "%a" pp

  let lookup_exn (ienv1, ienv2) nn =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (Some nn', None) -> IEnv1.lookup_exn ienv1 nn'
    | (None, Some nn') -> IEnv2.lookup_exn ienv2 nn'
    | _ ->
        failwith
          "Error while performing a lookup on an aggregated context. Please \
           report."

  let add_fresh (ienv1, ienv2) str ty value =
    match ClassifyTyp.classify ty with
    | true ->
        let (nn, ienv1') = IEnv1.add_fresh ienv1 str ty value in
        (EmbedNames.embed1 nn, (ienv1', ienv2))
    | false ->
        let (nn, ienv2') = IEnv2.add_fresh ienv2 str ty value in
        (EmbedNames.embed2 nn, (ienv1, ienv2'))

  let map f (ienv1, ienv2) = (IEnv1.map f ienv1, IEnv2.map f ienv2)

  let fold f a (ienv1, ienv2) =
    let f_lift1 a (nn, value) = f a (EmbedNames.embed1 nn, value) in
    let f_lift2 a (nn, value) = f a (EmbedNames.embed2 nn, value) in
    let a' = IEnv1.fold f_lift1 a ienv1 in
    IEnv2.fold f_lift2 a' ienv2
end
