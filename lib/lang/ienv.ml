module type IENV = sig
  type name
  type value
  type t [@@deriving to_yojson]

  val empty : t
  val concat : t -> t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val get_names : t -> name list
  val lookup_exn : t -> name -> value
  val add_last_check : t -> name -> value -> t
  val map : (value -> value) -> t -> t
  val fold : ('a -> name * value -> 'a) -> 'a -> t -> 'a
end

module Make_PMAP
    (Names : Names.NAMES)
    (Value : sig
      type t [@@deriving to_yojson]

      val pp : Format.formatter -> t -> unit
    end) :
  IENV
    with type name = Names.name
     and type value = Value.t
(*     and type t = (Names.name, Value.t) Util.Pmap.pmap *) = struct
  type name = Names.name
  type value = Value.t
  type t = (name, Value.t) Util.Pmap.pmap

  let empty = Util.Pmap.empty
  let concat = Util.Pmap.concat

  let pp fmt ienv =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, value) =
      Format.fprintf fmt "%a : %a" Names.pp_name n Value.pp value in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt ienv

  let to_string = Format.asprintf "%a" pp
  let get_names = Util.Pmap.dom

  let to_yojson ienv =
    let to_string (nn, value) =
      (Names.string_of_name nn, Value.to_yojson value) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string ienv)

  let lookup_exn ienv nn = Util.Pmap.lookup_exn nn ienv
  let add_last_check ienv nn value = Util.Pmap.add (nn, value) ienv
  let map = Util.Pmap.map_im
  let fold = Util.Pmap.fold
end

module AggregateDisjoint
    (IEnv1 : IENV)
    (IEnv2 : IENV)
    (EmbedNames : sig
      type t

      val embed1 : IEnv1.name -> t
      val embed2 : IEnv2.name -> t
      val extract1 : t -> IEnv1.name option
      val extract2 : t -> IEnv2.name option
    end)
    (EmbedValues : sig
      type t [@@deriving to_yojson]

      val embed1 : IEnv1.value -> t
      val embed2 : IEnv2.value -> t
      val extract1 : t -> IEnv1.value option
      val extract2 : t -> IEnv2.value option
    end) :
  IENV
    with type name = EmbedNames.t
     and type value = EmbedValues.t
     and type t = IEnv1.t * IEnv2.t = struct
  type t = IEnv1.t * IEnv2.t [@@deriving to_yojson]
  type name = EmbedNames.t
  type value = EmbedValues.t

  let empty = (IEnv1.empty, IEnv2.empty)

  let concat (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.concat ienv11 ienv21, IEnv2.concat ienv12 ienv22)

  let pp fmt (ienv1, ienv2) =
    Format.fprintf fmt "(%a,%a)" IEnv1.pp ienv1 IEnv2.pp ienv2

  let to_string = Format.asprintf "%a" pp

  let get_names (ienv1, ienv2) =
    List.map (fun nn -> EmbedNames.embed1 nn) (IEnv1.get_names ienv1)
    @ List.map (fun nn -> EmbedNames.embed2 nn) (IEnv2.get_names ienv2)

  let lookup_exn (ienv1, ienv2) nn =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (Some nn', None) -> EmbedValues.embed1 @@ IEnv1.lookup_exn ienv1 nn'
    | (None, Some nn') -> EmbedValues.embed2 @@ IEnv2.lookup_exn ienv2 nn'
    | _ ->
        failwith
          "Error while performing a lookup on an aggregated context. Please \
           report."

  let add_last_check (ienv1, ienv2) nn value =
    match
      ( EmbedNames.extract1 nn,
        EmbedValues.extract1 value,
        EmbedNames.extract2 nn,
        EmbedValues.extract2 value )
    with
    | (None, None, Some nn', Some val') ->
        let ienv2' = IEnv2.add_last_check ienv2 nn' val' in
        (ienv1, ienv2')
    | (Some nn', Some val', None, None) ->
        let ienv1' = IEnv1.add_last_check ienv1 nn' val' in
        (ienv1', ienv2)
    | _ ->
        failwith
          "Error while performing a last test on an aggregated context. Please \
           report."

  let map f (ienv1, ienv2) =
    let f_lift1 value =
      match EmbedValues.extract1 @@ f (EmbedValues.embed1 value) with
      | Some value' -> value'
      | None -> failwith "Map does not preserve type embeding. Please report"
    in
    let f_lift2 value =
      match EmbedValues.extract2 @@ f (EmbedValues.embed2 value) with
      | Some value' -> value'
      | None -> failwith "Map does not preserve type embeding. Please report"
    in
    (IEnv1.map f_lift1 ienv1, IEnv2.map f_lift2 ienv2)

  let fold f a (ienv1, ienv2) =
    let f_lift1 a (nn, value) =
      f a (EmbedNames.embed1 nn, EmbedValues.embed1 value) in
    let f_lift2 a (nn, value) =
      f a (EmbedNames.embed2 nn, EmbedValues.embed2 value) in
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
     and type t = IEnv1.t * IEnv2.t = struct
  type t = IEnv1.t * IEnv2.t [@@deriving to_yojson]
  type name = EmbedNames.t
  type value = IEnv1.value

  let empty = (IEnv1.empty, IEnv2.empty)

  let concat (ienv11, ienv12) (ienv21, ienv22) =
    (IEnv1.concat ienv11 ienv21, IEnv2.concat ienv12 ienv22)

  let pp fmt (ienv1, ienv2) =
    Format.fprintf fmt "(%a,%a)" IEnv1.pp ienv1 IEnv2.pp ienv2

  let to_string = Format.asprintf "%a" pp

  let get_names (ienv1, ienv2) =
    List.map (fun nn -> EmbedNames.embed1 nn) (IEnv1.get_names ienv1)
    @ List.map (fun nn -> EmbedNames.embed2 nn) (IEnv2.get_names ienv2)

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
