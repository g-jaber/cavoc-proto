module Renaming : Lang.Renaming.RENAMING with module Namectx = Namectx.Namectx =
  Lang.Renaming.Make (Namectx.Namectx)
