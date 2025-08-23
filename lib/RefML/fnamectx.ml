include Lang.Typectx.Make_PMAP (Names.FNames) (struct
        type t = Types.negative_type
        let to_yojson = Types.negative_type_to_yojson
        let pp = Types.pp_negative_type
      end)