module type IENV = sig
  type name
  type namectx
  type value
  type t [@@deriving to_yojson]

  val empty : t
  val dom : t -> namectx
  val im : t -> namectx
  val concat : t -> t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val lookup_exn : t -> name -> value
  val add_last_check : t -> name -> value -> t
  val map : (value -> value) -> t -> t
  val fold : ('a -> name * value -> 'a) -> 'a -> t -> 'a
end

module Make_PMAP
    (Names : Names.NAMES)
    (Namectx : Typectx.TYPECTX with type name = Names.name)
    (Value : sig
      type t [@@deriving to_yojson]

      val pp : Format.formatter -> t -> unit
    end) :
  IENV
    with type name = Namectx.name
     and type value = Value.t
     and type namectx = Namectx.t = struct
  type name = Namectx.name
  type value = Value.t
  type namectx = Namectx.t
  type t = { map: (name, Value.t) Util.Pmap.pmap; dom: namectx; im: namectx }

  let empty = { map= Util.Pmap.empty; dom= Namectx.empty; im= Namectx.empty }
  let dom ienv = ienv.dom
  let im ienv = ienv.im

  let concat ienv1 ienv2 =
    {
      map= Util.Pmap.concat ienv1.map ienv2.map;
      dom= Namectx.concat ienv1.dom ienv2.dom;
      im= Namectx.concat ienv1.im ienv2.im;
    }

  let pp fmt ienv =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, value) =
      Format.fprintf fmt "%a : %a" Names.pp_name n Value.pp value in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt ienv.map

  let to_string = Format.asprintf "%a" pp

  let to_yojson ienv =
    let to_string (nn, value) =
      (Names.string_of_name nn, Value.to_yojson value) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string ienv.map)

  let lookup_exn ienv nn = Util.Pmap.lookup_exn nn ienv.map

  (* We do not check that nn is indeed the last name in the next function.*)
  let add_last_check ienv nn value =
    let map = Util.Pmap.add (nn, value) ienv.map in
    { ienv with map }

  let map f ienv =
    let map = Util.Pmap.map_im f ienv.map in
    { ienv with map }

  let fold f value ienv = Util.Pmap.fold f value ienv.map
end

module Make_List
    (Namectx : Typectx.TYPECTX)
    (Values : sig
      type t [@@deriving to_yojson]

      val pp : Format.formatter -> t -> unit
    end) : IENV with type name = int * string and type value = Values.t = struct
  type name = int * string
  type value = Values.t
  type namectx = Namectx.t
  type t = { map: Values.t list; dom: namectx; im: namectx }

  let empty = { map= []; dom= Namectx.empty; im= Namectx.empty }
  let dom ienv = ienv.dom
  let im ienv = ienv.im

  let concat ienv1 ienv2 =
    {
      map= List.append ienv1.map ienv2.map;
      dom= Namectx.concat ienv1.dom ienv2.dom;
      im= Namectx.concat ienv1.im ienv2.im;
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

  let add_last_check ienv (i, str) value =
    let rec aux j = function
      | [] when i = j -> [ value ]
      | [] ->
          failwith
            ("The name " ^ str ^ string_of_int i
           ^ "is not at the end. Please report")
      | hd :: tl -> hd :: aux (j + 1) tl in
    let map = aux 0 ienv.map in
    { ienv with map }

  let map f ienv =
    let map = List.map f ienv.map in
    { ienv with map }

  let fold (f : 'a -> name * value -> 'a) (v : 'a) (ienv : t) =
    List.fold_left f v (List.mapi (fun i ty -> ((i, ""), ty)) ienv.map)
end

module Aggregate (IEnv1 : IENV) (IEnv2 : IENV) :
  IENV
    with type name = (IEnv1.name, IEnv2.name) Either.t
     and type value = (IEnv1.value, IEnv2.value) Either.t
     and type namectx = IEnv1.namectx * IEnv2.namectx
     and type t = IEnv1.t * IEnv2.t = struct
  type t = IEnv1.t * IEnv2.t [@@deriving to_yojson]
  type name = (IEnv1.name, IEnv2.name) Either.t
  type value = (IEnv1.value, IEnv2.value) Either.t
  type namectx = IEnv1.namectx * IEnv2.namectx

  let empty = (IEnv1.empty, IEnv2.empty)
  let dom (ienv1, ienv2) = (IEnv1.dom ienv1, IEnv2.dom ienv2)
  let im (ienv1, ienv2) = (IEnv1.im ienv1, IEnv2.im ienv2)

  let concat (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.concat ienv11 ienv21, IEnv2.concat ienv12 ienv22)

  let pp fmt (ienv1, ienv2) =
    Format.fprintf fmt "(%a,%a)" IEnv1.pp ienv1 IEnv2.pp ienv2

  let to_string = Format.asprintf "%a" pp

  let lookup_exn (ienv1, ienv2) nn =
    match nn with
    | Either.Left nn' -> Either.Left (IEnv1.lookup_exn ienv1 nn')
    | Either.Right nn' -> Either.Right (IEnv2.lookup_exn ienv2 nn')

  let add_last_check (ienv1, ienv2) nn value =
    match (nn, value) with
    | (Either.Left nn', Either.Left val') ->
        let ienv1' = IEnv1.add_last_check ienv1 nn' val' in
        (ienv1', ienv2)
    | (Either.Right nn', Either.Right val') ->
        let ienv2' = IEnv2.add_last_check ienv2 nn' val' in
        (ienv1, ienv2')
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
    (IEnv2 : IENV with type value = IEnv1.value)
    (EmbedNames : sig
      type t

      val embed1 : IEnv1.name -> t
      val embed2 : IEnv2.name -> t
      val extract1 : t -> IEnv1.name option
      val extract2 : t -> IEnv2.name option
    end) :
  IENV
    with type name = EmbedNames.t
     and type value = IEnv1.value
     and type namectx = IEnv1.namectx * IEnv2.namectx
     and type t = IEnv1.t * IEnv2.t = struct
  type t = IEnv1.t * IEnv2.t [@@deriving to_yojson]
  type name = EmbedNames.t
  type value = IEnv1.value
  type namectx = IEnv1.namectx * IEnv2.namectx

  let empty = (IEnv1.empty, IEnv2.empty)
  let dom (ienv1, ienv2) = (IEnv1.dom ienv1, IEnv2.dom ienv2)
  let im (ienv1, ienv2) = (IEnv1.im ienv1, IEnv2.im ienv2)

  let concat (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.concat ienv11 ienv21, IEnv2.concat ienv12 ienv22)

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

  let add_last_check (ienv1, ienv2) nn value =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (None, Some nn') ->
        let ienv2' = IEnv2.add_last_check ienv2 nn' value in
        (ienv1, ienv2')
    | (Some nn', None) ->
        let ienv1' = IEnv1.add_last_check ienv1 nn' value in
        (ienv1', ienv2)
    | _ ->
        failwith
          "Error while performing a last test on an aggregated context. Please \
           report."

  let map f (ienv1, ienv2) = (IEnv1.map f ienv1, IEnv2.map f ienv2)

  let fold f a (ienv1, ienv2) =
    let f_lift1 a (nn, value) = f a (EmbedNames.embed1 nn, value) in
    let f_lift2 a (nn, value) = f a (EmbedNames.embed2 nn, value) in
    let a' = IEnv1.fold f_lift1 a ienv1 in
    IEnv2.fold f_lift2 a' ienv2
end
