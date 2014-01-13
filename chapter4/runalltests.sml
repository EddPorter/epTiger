(*-------------------------------------
  This section ©2014 by Edd Porter
  -------------------------------------*)

(* run CM.make "sources.cm"; first *)
let
  fun run n = Parse.parse ("../testcases/test"^Int.toString(n)^".tig");
  fun runner n ns = if n < 49 then let in runner (n+1) ((n, run n)::ns) end else ns;
in 
  runner 1 []
end