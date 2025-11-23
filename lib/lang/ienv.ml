module type IENV = sig
  module Renaming : Renaming.RENAMING

  type value

  val embed_name : Renaming.Namectx.Names.name -> value

  type t [@@deriving to_yojson]

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit

  (* empty Δ : ⋅ → Δ *)
  val empty : Renaming.Namectx.t -> t
  val dom : t -> Renaming.Namectx.t
  val im : t -> Renaming.Namectx.t
  val embed_renaming : Renaming.t -> t

  (* Taking γ₁ : Γ₁ → Δ and γ₂ : Γ₂ → Δ, copairing γ₁ γ₂ : (Γ₁ + Γ₂) → Δ  *)
  val copairing : t -> t -> t

  (* Taking γ : Γ → Δ and Θ, then weaken_r γ Θ : Γ → Δ + Θ *)
  val weaken_r : t -> Renaming.Namectx.t -> t

  (* Taking γ : Γ → Δ and Θ, then weaken_l γ Θ : Γ → Θ + Δ *)
  val weaken_l : t -> Renaming.Namectx.t -> t

  (* Taking γ₁ : Γ₁ → Δ₁ and γ₂ : Γ₂ → Δ₂, pairing γ₁ γ₂ : (Γ₁ + Γ₂) → (Δ₁ + Δ₂)  *)
  val tensor : t -> t -> t
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

module type IENV_ORDERED = sig
  include IENV

  val get_last : t -> (value * t) option
end

module Make_PMAP
    (Renaming : Renaming.RENAMING)
    (Value : sig
      type t [@@deriving to_yojson]

      val embed_name : Renaming.Namectx.Names.name -> t
      val renam_act : Renaming.t -> t -> t
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

  let empty im = { map= Util.Pmap.empty; dom= Renaming.Namectx.empty; im }
  let dom ienv = ienv.dom
  let im ienv = ienv.im

  let embed_renaming renam =
    let dom = Renaming.dom renam in
    let im = Renaming.im renam in
    let names_l = Renaming.Namectx.get_names dom in
    let map =
      Util.Pmap.list_to_pmap
      @@ List.map
           (fun nn -> (nn, embed_name @@ Renaming.lookup renam nn))
           names_l in
    (* This is highly ineficient*)
    { map; dom; im }

  let copairing ienv1 ienv2 =
    assert (ienv1.im = ienv2.im);
    {
      map= Util.Pmap.concat ienv1.map ienv2.map;
      dom= Renaming.Namectx.concat ienv1.dom ienv2.dom;
      im= ienv1.im;
    }

  (* Taking γ : Γ → Δ and Θ, then weaken_r γ Θ : Γ → Δ + Θ *)
  let weaken_r ienv namectx =
    let renam = Renaming.weak_l ienv.im namectx in
    (* renam : Δ → Δ + Θ *)
    let map = Util.Pmap.map_im (Value.renam_act renam) ienv.map in
    { map; dom= ienv.dom; im= Renaming.im renam }

  let weaken_l ienv namectx =
    let renam = Renaming.weak_r ienv.im namectx in
    let map = Util.Pmap.map_im (Value.renam_act renam) ienv.map in
    { map; dom= ienv.dom; im= Renaming.im renam }

  let tensor ienv1 ienv2 =
    let ienv1' = weaken_r ienv1 (im ienv2) in
    let ienv2' = weaken_l ienv2 (im ienv1) in
    copairing ienv1' ienv2'

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
    (Value : sig
      type t [@@deriving to_yojson]

      val renam_act : Renaming.t -> t -> t
      val embed_name : Renaming.Namectx.Names.name -> t
      val pp : Format.formatter -> t -> unit
    end) : IENV with module Renaming = Renaming and type value = Value.t =
struct
  module Renaming = Renaming

  type value = Value.t

  let embed_name = Value.embed_name

  type t = {
    map: (string * Value.t) list;
    dom: Renaming.Namectx.t;
    im: Renaming.Namectx.t;
  }

  let pp fmt ienv =
    match ienv.map with
    | [] -> Format.fprintf fmt "⋅"
    | map' ->
        let pp_sep fmt () = Format.fprintf fmt ", " in
        Format.pp_print_list ~pp_sep
          (fun fmt (str, v) -> Format.fprintf fmt "%s:%a" str Value.pp v)
          fmt map'

  let to_string = Format.asprintf "%a" pp

  let to_yojson ienv =
    `Assoc
      (List.mapi
         (fun i (str, v) ->
           let str = Renaming.Namectx.Names.string_of_name (i, str) in
           (str, Value.to_yojson v))
         ienv.map)

  let empty im = { map= []; dom= Renaming.Namectx.empty; im }
  let dom ienv = ienv.dom
  let im ienv = ienv.im

  let embed_renaming renam =
    let dom = Renaming.dom renam in
    let im = Renaming.im renam in
    let names_l = Renaming.Namectx.get_names dom in
    let map =
      List.map
        (fun ((_i, str) as nn) -> (str, embed_name @@ Renaming.lookup renam nn))
        names_l in
    (* This rely on the fact that names_l is in the right order *)
    { map; dom; im }

  let copairing ienv1 ienv2 =
    Util.Debug.print_debug @@ "Copairing " ^ to_string ienv1 ^ " and "
    ^ to_string ienv2 ^ " of image "
    ^ Renaming.Namectx.to_string ienv1.im
    ^ " and "
    ^ Renaming.Namectx.to_string ienv2.im;
    assert (ienv1.im = ienv2.im);
    {
      map= List.append ienv1.map ienv2.map;
      dom= Renaming.Namectx.concat ienv1.dom ienv2.dom;
      im= ienv1.im;
    }

  (* Taking γ : Γ → Δ and Θ, then weaken_r γ Θ : Γ → Δ + Θ *)
  let weaken_r ienv namectx =
    let renam = Renaming.weak_l ienv.im namectx in
    let map =
      List.map (fun (str, v) -> (str, Value.renam_act renam v)) ienv.map in
    { map; dom= ienv.dom; im= Renaming.im renam }

  (* Taking γ : Γ → Δ and Θ, then weaken_l γ Θ : Γ → Θ + Δ *)
  let weaken_l ienv namectx =
    Util.Debug.print_debug @@ "weaken_l on " ^ to_string ienv
    ^ " of image context "
    ^ Renaming.Namectx.to_string ienv.im
    ^ " with namectx "
    ^ Renaming.Namectx.to_string namectx;
    let renam = Renaming.weak_r ienv.im namectx in
    (* renam :  Δ → Θ + Δ *)
    Util.Debug.print_debug @@ "Creating renam in weak_l: "
    ^ Renaming.to_string renam;
    let map =
      List.map (fun (str, v) -> (str, Value.renam_act renam v)) ienv.map in
    { map; dom= ienv.dom; im= Renaming.im renam }

  let tensor ienv1 ienv2 =
    let ienv1' = weaken_r ienv1 (im ienv2) in
    let ienv2' = weaken_l ienv2 (im ienv1) in
    copairing ienv1' ienv2'

  let lookup_exn ienv (i, _) = snd @@ List.nth ienv.map i

  let add_fresh ienv str ty value =
    let (nn, dom) = Renaming.Namectx.add_fresh ienv.dom str ty in
    let map = ienv.map @ [ (str, value) ] in
    (nn, { ienv with map; dom })

  let map f ienv =
    let map = List.map (fun (str, v) -> (str, f v)) ienv.map in
    { ienv with map }

  let fold (f : 'a -> Renaming.Namectx.Names.name * value -> 'a) (v : 'a)
      (ienv : t) =
    List.fold_left f v (List.mapi (fun i (str, v) -> ((i, str), v)) ienv.map)
end

module Make_Stack
    (Renaming : Renaming.RENAMING with type Namectx.Names.name = unit)
    (Value : sig
      type t [@@deriving to_yojson]

      val renam_act : Renaming.t -> t -> t
      val embed_name : Renaming.Namectx.Names.name -> t
      val pp : Format.formatter -> t -> unit
    end) :
  IENV_ORDERED with module Renaming = Renaming and type value = Value.t = struct
  module Renaming = Renaming

  type value = Value.t

  let embed_name = Value.embed_name

  type t = {
    stack: Value.t list;
    dom: Renaming.Namectx.t;
    im: Renaming.Namectx.t;
  }

  let pp fmt ienv =
    match ienv.stack with
    | [] -> Format.fprintf fmt "⋅"
    | _ ->
        let pp_sep fmt () = Format.fprintf fmt ", " in
        Format.pp_print_list ~pp_sep
          (fun fmt v -> Format.fprintf fmt "%a" Value.pp v)
          fmt ienv.stack

  let to_string = Format.asprintf "%a" pp
  let to_yojson ienv = `List (List.map Value.to_yojson ienv.stack)
  let empty im = { stack= []; dom= Renaming.Namectx.empty; im }
  let dom ienv = ienv.dom
  let im ienv = ienv.im

  let embed_renaming _renam =
    failwith "Embed renaming is impossible for stacks. Please report."

  let copairing ienv1 ienv2 =
    assert (ienv1.im = ienv2.im);
    {
      stack= List.append ienv1.stack ienv2.stack;
      dom= Renaming.Namectx.concat ienv1.dom ienv2.dom;
      im= ienv1.im;
    }

  (* Taking γ : Γ → Δ and Θ, then weaken_r γ Θ : Γ → Δ + Θ *)
  let weaken_r ienv namectx =
    let renam = Renaming.weak_l ienv.im namectx in
    let stack = List.map (Value.renam_act renam) ienv.stack in
    { stack; dom= ienv.dom; im= Renaming.im renam }

  let weaken_l ienv namectx =
    let renam = Renaming.weak_r ienv.im namectx in
    let stack = List.map (Value.renam_act renam) ienv.stack in
    { stack; dom= ienv.dom; im= Renaming.im renam }

  let tensor ienv1 ienv2 =
    let ienv1' = weaken_r ienv1 (im ienv2) in
    let ienv2' = weaken_l ienv2 (im ienv1) in
    copairing ienv1' ienv2'

  let lookup_exn ienv () =
    match ienv.stack with
    | [] -> failwith "Empty stack ienv. Please report"
    | v :: _ -> v

  let add_fresh ienv str ty value =
    let ((), dom) = Renaming.Namectx.add_fresh ienv.dom str ty in
    let stack = value :: ienv.stack in
    ((), { ienv with stack; dom })

  let map f ienv =
    let stack = List.map f ienv.stack in
    { ienv with stack }

  let fold (f : 'a -> Renaming.Namectx.Names.name * value -> 'a) (v : 'a)
      (ienv : t) =
    List.fold_left f v (List.map (fun v -> ((), v)) ienv.stack)

  let get_last ienv =
    match ienv.stack with
    | [] -> None
    | v :: stack -> Some (v, { ienv with stack })
  (* This is wrong as we do not restrict the domain !*)
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
          IEnv1.Renaming.Namectx.t * IEnv2.Renaming.Namectx.t
         and type t = IEnv1.Renaming.t * IEnv2.Renaming.t) :
  IENV
    with module Renaming = Renaming
     and type value = (IEnv1.value, IEnv2.value) Either.t
     and type t = IEnv1.t * IEnv2.t = struct
  module Renaming = Renaming

  type value = (IEnv1.value, IEnv2.value) Either.t

  let embed_name = function
    | Either.Left nn -> Either.Left (IEnv1.embed_name nn)
    | Either.Right nn -> Either.Right (IEnv2.embed_name nn)

  type t = IEnv1.t * IEnv2.t

  let pp fmt (ienv1, ienv2) =
    Format.fprintf fmt "(%a,%a)" IEnv1.pp ienv1 IEnv2.pp ienv2

  let to_string = Format.asprintf "%a" pp

  let to_yojson (ienv1, ienv2) =
    match (IEnv1.to_yojson ienv1, IEnv2.to_yojson ienv2) with
    | (`Assoc ienv1_l, `Assoc ienv2_l) -> `Assoc (ienv1_l @ ienv2_l)
    | (ienv1_yojson,ienv2_yojson) -> `Tuple [ienv1_yojson; ienv2_yojson]

  let empty (im1, im2) = (IEnv1.empty im1, IEnv2.empty im2)
  let dom (ienv1, ienv2) = (IEnv1.dom ienv1, IEnv2.dom ienv2)
  let im (ienv1, ienv2) = (IEnv1.im ienv1, IEnv2.im ienv2)

  let embed_renaming (renam1, renam2) =
    (IEnv1.embed_renaming renam1, IEnv2.embed_renaming renam2)

  let copairing (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.copairing ienv11 ienv21, IEnv2.copairing ienv12 ienv22)

  let weaken_r (ienv1, ienv2) (namectx1, namectx2) =
    let ienv1' = IEnv1.weaken_r ienv1 namectx1 in
    let ienv2' = IEnv2.weaken_r ienv2 namectx2 in
    (ienv1', ienv2')

  let weaken_l (ienv1, ienv2) (namectx1, namectx2) =
    let ienv1' = IEnv1.weaken_l ienv1 namectx1 in
    let ienv2' = IEnv2.weaken_l ienv2 namectx2 in
    (ienv1', ienv2')

  let tensor ienv1 ienv2 =
    Util.Debug.print_debug @@ "Tensoring Aggregate " ^ to_string ienv1 ^ " and "
    ^ to_string ienv2;
    let ienv1' = weaken_r ienv1 (im ienv2) in
    let ienv2' = weaken_l ienv2 (im ienv1) in
    copairing ienv1' ienv2'

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
          IEnv1.Renaming.Namectx.t * IEnv2.Renaming.Namectx.t
         and type t = IEnv1.Renaming.t * IEnv2.Renaming.t)
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

  type t = IEnv1.t * IEnv2.t

  let empty (im1, im2) = (IEnv1.empty im1, IEnv2.empty im2)
  let dom (ienv1, ienv2) = (IEnv1.dom ienv1, IEnv2.dom ienv2)
  let im (ienv1, ienv2) = (IEnv1.im ienv1, IEnv2.im ienv2)

  let embed_renaming (renam1, renam2) =
    (IEnv1.embed_renaming renam1, IEnv2.embed_renaming renam2)

  let copairing (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.copairing ienv11 ienv21, IEnv2.copairing ienv12 ienv22)

  let weaken_r (ienv1, ienv2) (namectx1, namectx2) =
    let ienv1' = IEnv1.weaken_r ienv1 namectx1 in
    let ienv2' = IEnv2.weaken_r ienv2 namectx2 in
    (ienv1', ienv2')

  let weaken_l (ienv1, ienv2) (namectx1, namectx2) =
    let ienv1' = IEnv1.weaken_l ienv1 namectx1 in
    let ienv2' = IEnv2.weaken_l ienv2 namectx2 in
    (ienv1', ienv2')

  let tensor ienv1 ienv2 =
    let ienv1' = weaken_r ienv1 (im ienv2) in
    let ienv2' = weaken_l ienv2 (im ienv1) in
    copairing ienv1' ienv2'

  let pp fmt (ienv1, ienv2) =
    Format.fprintf fmt "(%a,%a)" IEnv1.pp ienv1 IEnv2.pp ienv2

  let to_string = Format.asprintf "%a" pp

  let to_yojson (ienv1, ienv2) =
    match (IEnv1.to_yojson ienv1, IEnv2.to_yojson ienv2) with
    | (`Assoc ienv1_l, `Assoc ienv2_l) -> `Assoc (ienv1_l @ ienv2_l)
    | (ienv1_yojson,ienv2_yojson) -> `Tuple [ienv1_yojson; ienv2_yojson]

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
