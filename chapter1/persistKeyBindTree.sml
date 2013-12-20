(*-------------------------------------
  This section ©1998 by Andrew W. Appel
  -------------------------------------*)

(* Exercise 1.1 b. - Create a tree structure allows mapping of keys to bindings.
    datatype 'a tree = ...
    insert: 'a tree * key * 'a -> 'a tree
    lookup: 'a tree * key -> 'a
*)


(*-------------------------------------
  This section ©2013 by Edd Porter
  -------------------------------------*)

type key = string
datatype 'a tree = LEAF | TREE of 'a tree * (key * 'a) * 'a tree

val empty = LEAF

fun insert(LEAF,key,bind) = TREE(LEAF,(key, bind),LEAF)
  | insert(TREE(l,(k,b),r),key,bind) = 
         if key<k
           then TREE(insert(l,key,bind),(k,b),r)
         else if key>k
           then TREE(l,(k,b),insert(r,key,bind))
         else TREE(l,(key,bind),r)

fun lookup(TREE(l,(k,b),r),key) = 
         if key<k
           then lookup(l,key)
         else if key>k
           then lookup(r,key)
         else b
     
val test = insert(insert(insert(insert(insert(empty,"fred",4),"bob",3),"al",2),"harry",5),"george",6)