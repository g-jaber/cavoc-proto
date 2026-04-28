module Make : functor (M : Util.Monad.MONAD) (IntLTS : Strategy.LTS) ->
  Interactive_build.IBUILD with module M = M and type conf = IntLTS.conf
