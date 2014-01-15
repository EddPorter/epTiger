(*-------------------------------------
  This section ©2014 by Edd Porter
  -------------------------------------*)
  
structure Env : ENV
=
struct
  type access = int
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  
  (* The predefined type environment: int and string. *)
  val base_tenv =
    let val t = Symbol.enter (Symbol.empty, Symbol.symbol "int", Types.INT)
    in Symbol.enter (t, Symbol.symbol "string", Types.STRING) end
  
  (* The predefined function environment, consisting of the standard library. *)
  val base_venv = 
    let
      val lib = [ ("print", [Types.STRING], Types.UNIT),
                  ("flush", [], Types.UNIT),
                  ("getchar", [], Types.STRING),
                  ("ord", [Types.STRING], Types.INT),
                  ("chr", [Types.INT], Types.STRING),
                  ("size", [Types.STRING], Types.INT),
                  ("substring", [Types.STRING, Types.INT, Types.INT], Types.STRING),
                  ("concat", [Types.STRING, Types.STRING], Types.STRING),
                  ("not", [Types.INT], Types.INT),
                  ("exit", [Types.INT], Types.UNIT) ]
      fun f ((name,formals,result), t) = Symbol.enter (t, Symbol.symbol name, FunEntry {formals=formals, result=result})
    in
      List.foldl f Symbol.empty lib
    end
end