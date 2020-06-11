(* PATTERN-STYLE *)

(* 1 *)
(* FUNCTION NAME: delnthc *)
(* DESCRIPTION: delete the n-th character of a string *)

fun temp(nil, _) = nil
  | temp(x::ys, 1) = ys
  | temp(x::ys, z) = x::temp(ys, z-1);

fun delnthc(s,n) =
  implode(temp(explode(s),n));
  
delnthc("abcdef", 4);


(* 2 *)
(* FUNCTION NAME: dispnthc *)
(* DESCRIPTION: display the n-th character of a string *)

fun dispnthChar(x::ys,1) = x
  | dispnthChar(x::ys, n) = dispnthChar(ys,n-1);

fun dispnthc(s, n) =
  dispnthChar(explode(s), n);

dispnthc("abcdef", 4);


(* 3 *)
(* FUNCTION NAME: multin *)
(* DESCRIPTION: takes a list of three [a,b,c] and multiplies (a by b) c times *)

fun multin(x::y::[0]) = [x]
  | multin(x::y::[1]) = x::multin([x*y, y, 0])
  | multin(x::y::[z]) = x::multin([x*y, y, z-1]);

multin([2,3,5]);


(* 4 *)
(* FUNCTION NAME: remv *)
(* DESCRIPTION: remove elements from a list (including all multiple appearances) *)

fun remv(_, nil) = nil
  | remv(a, x::ys) =
      if a = x then remv(a, ys)
      else x::remv(a, ys);

remv("a", ["a", "b", "a", "c"]);


(* 5 *)
(* FUNCTION NAME: remvdub *)
(* DESCRIPTION: remove duplicate elements from a list *)

fun remvdub (nil) = nil
  | remvdub(x::xs) = x::remvdub(remv(x,xs));

remvdub(["a", "b", "a", "c", "b", "a"]);


(* 6 *)
(* FUNCTION NAME: int2str *)
(* DESCRIPTION: convert a positive integer to a string; ASCII conversion *)

fun int2str(0) = ""
  | int2str(n) = int2str(n div 10) ^ implode([chr((n mod 10) + 48)]);

int2str(1234);


(* 7 *)
(* FUNCTION NAME: str2int *)
(* DESCRIPTION: convert a string to a positive integer; ASCII conversion *)

fun toTenth(0) = 1
  | toTenth(x) = 10 * toTenth(x-1);

fun char2int(c) =
  ord(c) - 48;

fun char2intL(nil) = 0
  | char2intL(x::ys) = char2int(x) * toTenth(length(ys)) + char2intL(ys);

fun str2int(s) =
  char2intL(explode(s));

str2int("1234");


(* 8 *)
(* FUNCTION NAME: inde *)
(* DESCRIPTION: return the index (start from 1) of the occurence of a given value *)

fun indeRev(_, nil) = nil
  | indeRev(n, x::xs) =
      if x = n then (length(xs) + 1)::indeRev(n, xs)
      else indeRev(n, xs);

fun inde(n, L) =
  rev(indeRev(n, rev(L)));

inde(1, [1,2,1,1,2,2,1]);


(* 9 *)
(* FUNCTION NAME: nele *)
(* DESCRIPTION: repeats each element in a list n times *)

fun repeat(_, 0) = nil
  | repeat(x, n) = x::repeat(x, n-1);

fun appen(nil, L) = L
  | appen(x::xs, L) = x::appen(xs,L);

fun nele(nil, _) = nil
  | nele(x::xs, n) = appen(repeat(x,n), nele(xs,n));

nele([1, 2], 3);


(* 10  *)
(* FUNCTION NAME: ntrin *)
(* DESCRIPTION: generates a list of n triangular numbers from 1 *)

fun add(0) = 0
  | add(n) = n + add(n-1);

fun ntrin(0) = nil
  | ntrin(n) = appen(ntrin(n-1),[add(n)]);

ntrin(7);


(* 11 *)
(* FUNCTION NAME: isfact *)
(* DESCRIPTION: determines if a positive integer is a factorial number *)

fun help(1, _) = true
  | help(n, x) = 
      if(n mod(x)) = 0 then help(n div(x), x + 1)
      else false;

fun isfact(n) =
  if help(n, 2) then true
  else false;

isfact(120);


(* 12 *)
(* FUNCTION NAME: ratadd *)
(* DESCRIPTION: adds two rational numbers and returns its simplest form *)

fun gcd(0,n) = n
  | gcd(m,n) = gcd(n mod m, m);

fun reduce(_,0) = (0,0)
  | reduce(p,q) =
      let
        val d = gcd(p,q)
      in 
        (p div d, q div d)
      end;

fun ratadd((_,0),(_,0)) = (0,0)
  | ratadd((a,b),(c,d)) =
      let
        val first = a*d
        val second = b*c
        val third = b*d
      in 
        reduce((first + second),third)
      end;

ratadd((2,3),(1,12));


(* 13 *)
(* FUNCTION NAME: occr *)
(* DESCRIPTION: displays the occurrence of an element of a list; nil if empty *)

fun count(_, nil) = 0
  | count(a, x::xs) = 
      if x = a then 1 + count(a, xs)
      else count(a, xs);

fun occr(nil) = nil
  | occr(x::xs) = (x, count(x, x::xs))::occr(remv(x,xs));

occr([1, 2, 1, 2, 3, 2]);


(* 14 *)
(* FUNCTION NAME: insfront *)
(* DESCRIPTION: higher order; insert an element as the head of eacch a=element of a list *)

fun comb(_, _, nil) = nil
  | comb(f, n, x::xs) = f(n, x)::comb(f, n, xs);

fun insfront(n, L) = 
  comb(fn(a, b) => a::b, n, L);

insfront(1, [[1,2], nil, [3]]);


(* 15 *)
(* FUNCTION NAME: inseach *)
(* DESCRIPTION: inserts an element to each position of a list *)
fun appen(nil, L) = L
  | appen(x::xs, L) = x::appen(xs,L);


fun loc(n, 1, L) = n::L
  | loc(n, x, y::ys) = y::loc(n, x-1, ys);

fun create(_, 0, _) = nil
  | create(n, x, L) = appen(create(n, x-1, L), [loc(n, x, L)]);

fun inseach(n, L) =
  create(n, (length(L)+1), L);


inseach(4, [1,2,3]);
