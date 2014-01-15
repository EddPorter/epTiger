(*-------------------------------------
  This section ©2014 by Edd Porter
  -------------------------------------*)

structure Compiler
=
struct 
  fun compile filename =
    let
      val absyn = Parse.parse filename
    in
      Semant.transProg absyn;
      absyn
    end
end