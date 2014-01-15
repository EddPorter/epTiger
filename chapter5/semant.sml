(*-------------------------------------
  This section ©2014 by Edd Porter
  -------------------------------------*)

structure Semant :
  sig
    val transProg : Absyn.exp -> unit
  end
=
struct
  fun transProg _ = ()
end