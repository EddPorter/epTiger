(*-------------------------------------
  This section ©1998 by Andrew W. Appel
  -------------------------------------*)

type key = string
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert(key,LEAF) = TREE(LEAF,key,LEAF)
  | insert(key,TREE(l,k,r)) = 
               if key<k
                 then TREE(insert(key,l),k,r)
               else if key>k
                 then TREE(l,k,insert(key,r))
               else TREE(l,key,r)


(*-------------------------------------
  This section ©2013 by Edd Porter
  -------------------------------------*)

(* Exercise 1.1 a. - Returns true if item is found, else false. *)         
fun member(find,LEAF) = false
  | member(find,TREE(l,k,r)) = 
         if find<k
           then member(find,l)
         else if find>k
           then member(find,r)
         else true
     
val test = insert("george",insert("zachary",insert("bob",insert("fred",empty))))

(* Exercise 1.1 c. a) - Demonstrates insertion of letter t s p i p f b s t into a tree structure.
   Note that the unique letters are inserted in reverse alphabetical order, so the tree is one-sided. *)
val ex11ca = insert("t",insert("s",insert("b",insert("f",insert("p",insert("i",insert("p",insert("s",insert("t",empty)))))))))

(* Exercise 1.1 c. b) - Demonstrates insertion of letter a b c d e f g h i into a tree structure.
   All values are unique but are inserted in alphabetical order, so the tree is one-sided. *)
val ex11cb = insert("i",insert("h",insert("g",insert("f",insert("e",insert("d",insert("c",insert("b",insert("a",empty)))))))))

fun depth(LEAF) = 0
  | depth(TREE(l,_,r)) = Int.max(depth(l), depth(r)) + 1