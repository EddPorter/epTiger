type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
(* Example program inputs to interpret *)
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))
val prog2 = PrintStm[IdExp "a", IdExp "b", IdExp "c"]
val prog3 = PrintStm[EseqExp(PrintStm[IdExp "a", IdExp "b", IdExp "c"], NumExp 1), IdExp "2"]
val prog4 = PrintStm[EseqExp(PrintStm[IdExp "a"], NumExp 1), IdExp "b", IdExp "c", IdExp "d"]
val prog5 = PrintStm[NumExp 5];
val prog6 = PrintStm[NumExp 5, NumExp 6];
val prog7 = AssignStm("a", NumExp 2)
val prog8 = AssignStm("b", OpExp(EseqExp(PrintStm[IdExp "a", IdExp "b", IdExp "c"], NumExp 4), Plus, IdExp "d"))

(* maxargs : stm->int *)
(* returns the maximum number of arguments of any `print` statement with
   a given `stm` statement *)
fun maxargs s =
  let
    fun maxargss (CompoundStm(s1, s2)) = Int.max(maxargss(s1), maxargss(s2))
      | maxargss (AssignStm(_, e))     = maxargse(e)
      | maxargss (PrintStm l)          = pargs(l)
    and maxargse (EseqExp(s1,e))  = Int.max(maxargss(s1), maxargse(e))
      | maxargse (OpExp(e1,_,e2)) = Int.max(maxargse(e1), maxargse(e2))
      | maxargse _                = 0
    and pargs (EseqExp(s,e)::tail) = Int.max(Int.max(maxargss(s), maxargse(e)), 1 + pargs(tail))
      | pargs (_::tail)            = 1 + pargs(tail)
      | pargs _                    = 0
  in
    maxargss(s)
  end
 
(* interp : stm -> unit *)
(* "Inteprets" a program in the specified straight-line language. *)
fun interp s =
  let
    datatype table = Table of (id * int) list
    (* interpStm : stm * table -> table *)
    fun interpStm (CompoundStm (s1, s2), Table t) =
                   let
                     val nt = interpStm(s1, Table t)
                   in
                     interpStm(s2, nt)
                   end
      | interpStm (AssignStm (i, e), Table t) =
                   let
                     val (v, Table nt) = interpExp(e, Table t)
                   in
                     Table ((i, v)::nt)
                   end
      | interpStm (PrintStm l, Table t) = printout(l, Table t)
    (* interpExp : exp * table -> int * table *)
    and interpExp (IdExp e, t) = (lookup(t, e), t)
      | interpExp (NumExp n, t) = (n, t)
      | interpExp (OpExp (e1, Plus, e2), t) =
                   let val (v1, nt) = interpExp(e1, t); val (v2, nt2) = interpExp(e2, nt)
                   in (v1 + v2, nt2)
                   end
      | interpExp (OpExp (e1, Minus, e2), t) =
                   let val (v1, nt) = interpExp(e1, t); val (v2, nt2) = interpExp(e2, nt)
                   in (v1 - v2, nt2)
                   end
      | interpExp (OpExp (e1, Times, e2), t) =
                   let val (v1, nt) = interpExp(e1, t); val (v2, nt2) = interpExp(e2, nt)
                   in (v1 * v2, nt2)
                   end
      | interpExp (OpExp (e1, Divide, e2), t) =
                   let val (v1, nt) = interpExp(e1, t); val (v2, nt2) = interpExp(e2, nt)
                   in (Int.div(v1, v2), nt2)
                   end
      | interpExp (EseqExp (s, e), t) =
                   let val nt = interpStm(s, t)
                   in interpExp(e, nt)
                   end
    (* lookup : table * id -> int *)
    and lookup (Table [], _)              = 0
      | lookup (Table ((tn, v)::tail), n) = if tn = n then v else lookup(Table tail, n)
    (* printout : exp list * table -> table *)
    and printout ([], t)                 = let in print("\n"); t end
      | printout ((IdExp i)::tail, t)  = 
                 let
                 in
                   print(Int.toString(lookup(t, i))); print(" ");
                   printout(tail, t)
                 end
      | printout ((NumExp n)::tail, t) =
                 let 
                 in
                   print(Int.toString(n)); print(" ");
                   printout(tail, t)
                 end
      | printout (OpExp (o1,b,o2)::tail, t) =
                 let 
                   val (n, nt) = interpExp(OpExp (o1,b,o2), t)
                 in
                   print(Int.toString(n)); print(" ");
                   printout(tail, nt)
                 end
      | printout (EseqExp (s, e)::tail, t) =
                 let
                  val (n, nt) = interpExp(EseqExp (s, e), t)
                 in
                   print(Int.toString(n)); print(" ");
                   printout(tail, nt)
                 end
  in
    interpStm(s, Table[])
  end
  