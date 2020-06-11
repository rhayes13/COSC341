(* CODE *)

(* 1 *)
(* FUNCTION NAME: subString *)
(* DESCRIPTION: checks whether the first string is a substring of the second, 
                returns first occ index (from 0) or -1 otherwise *)

fun isPrefix(nil, nil) = true
  | isPrefix(nil, _) = true
  | isPrefix(_, nil) = false
  | isPrefix(x::xs, y::ys) =
      if x = y then isPrefix(xs, ys)
      else false;

fun helper(nil, _) = 0
  | helper(_, nil) = ~1
  | helper(A, B) =
      if isPrefix(A, B) then 0
      else 1 + helper(A, tl B);

fun subString(s1,s2) = helper(explode(s1), explode(s2));

subString("aaa", "aaaa");
subString("bc", "abcabc");



(* 2 *)
(* FUNCTION NAME: permu *)
(* DESCRIPTION: generates the permutation of the identity list from 1 to n *)

fun createList(0) = nil
  | createList(n) = n::createList(n-1);

fun appen(nil, L) = L
  | appen(x::xs, L) = x::appen(xs,L);

fun appendAll(nil) = nil
  | appendAll(z::zs) = 
      appen(z, appendAll(zs));

fun interleave(x) nil = [[x]]
  | interleave(x) (y::ys) =
    (x::y::ys) :: (map (fn u => y::u) (interleave x ys));

fun perm(nil) = [nil]
  | perm(x::xs) =
      appendAll (map (interleave x) (perm xs));



fun permu(n) =
  if n = 0 then [[0]]
  else perm(createList(n));

permu(3);



(* 3 *)
(* FUNCTION NAME: plist *)
(* DESCRIPTION: lists all prime numbers up to a given number *)

fun myfilter(_) nil = nil
  | myfilter f(x::xs) =
      if f(x) then x::myfilter f(xs)
      else myfilter f(xs);

fun sift(nil) = nil
  | sift(x::xs) =
      x::sift(myfilter (fn y => y mod x > 0) xs);

fun generate(x, y) =
  if x > y then nil
  else x::generate (x + 1, y);

fun plist(n) =
  sift(generate (2, n));

plist(20);



(* 4.1 *)
(* FUNCTION NAME: toBST *)
(* DESCRIPTION: generates a BST from an integer list *)

datatype 'data tree = Empty 
  | Node of 'data tree * 'data * 'data tree;

datatype 'd btree = NIL 
  | Node of 'd btree * 'd * 'd btree;

exception Err;
datatype intree = Empty
  | Node of intree * int * intree;

fun insert(x, Empty) = Node(Empty, x, Empty)
  | insert (x, Node(left, root, right)) =
      if x <= root then Node(insert(x, left), root, right)
      else Node(left, root, insert(x,right));

fun int2str(n) = str(chr(n+ord(#"0")));
fun dupBlank(0) = (print "")
  | dupBlank(n) = (print " "; dupBlank(n-1));
fun show(Empty, _) = (print "\n")
  | show(Node(left, root, right), space) =
      (  show(right, space+4);
         dupBlank(space);
         print(int2str(root));
         show(left, space+4)
      );

fun disp(intree) = show(intree, 0);

fun revBST(nil) = Empty
  | revBST(x::xs) = insert(x, (revBST xs));

fun toBST(nil) = Empty
  | toBST(L) = revBST(rev(L));

disp(toBST([4,8,1,9,7]));



(* 4.2 *)
(* FUNCTION NAME: isBST *)
(* DESCRIPTION: checks if a given tree is a BST *)

fun isBST(Empty) = true
  | isBST(Node(left, root, right)) =
      let
        fun check(_) Empty = true
          | check this (n as Node(_, root, _)) = this root andalso isBST(n)
        in
          check (fn root' => root' <= root) left andalso
          check (fn root' => root' > root) right
        end;

val tree = Node(Node(Empty, 2, Empty), 3, Node(Empty, 5, Empty));

isBST(tree);
