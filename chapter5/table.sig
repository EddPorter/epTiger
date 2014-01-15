(*-------------------------------------
  This section ©1998 by Andrew W. Appel
  -------------------------------------*)

signature TABLE = 
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
end

