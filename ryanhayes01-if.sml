(* IF-THEN-ELSE STYLE *)

(* 1 *)
(* FUNCTION NAME: delnthc *)
(* DESCRIPTION: delete the n-th character of a string *)

fun delnthChar(L, n) =
  if n = 1 then tl(L)
  else hd(L)::delnthChar(tl(L), n-1);

fun delnthc(s, n) =
  implode(delnthChar(explode(s), n));

delnthc("abcdef", 4);


(* 2 *)
(* FUNCTION NAME: dispnthc *)
(* DESCRIPTION: display the n-th character of a string *)

fun dispnthChar(L, n) =
  if n = 1 then hd(L)
  else dispnthChar(tl(L), n-1);

fun dispnthc(s, n) =
  dispnthChar(explode(s), n);

dispnthc("abcdef", 4);


(* 3 *)
(* FUNCTION NAME: multin *)
(* DESCRIPTION: takes a list of three [a,b,c] and multiplies (a by b) c times *)

fun multin(L) =
    if hd(tl(tl(L))) = 0 then [hd(L)]
    else let
      val a = hd(L)
      val b = hd(tl(L))
      val count = hd(tl(tl(L)))
      val ab = a*b
    in
      a::multin([ab, b, count-1])
    end;

multin([2,3,5]);


(* 4 *)
(* FUNCTION NAME: remv *)
(* DESCRIPTION: remove elements from a list (including all multiple appearances) *)

fun remv (x, L) = 
  if null L then nil
  else if hd(L) = x then remv(x, tl(L))
  else hd(L)::remv(x, tl(L));

remv("a", ["a", "b", "a", "c"]);


(* 5 *)
(* FUNCTION NAME: remvdub *)
(* DESCRIPTION: remove duplicate elements from a list *)

fun remvdub (L) = 
  if null L then nil
  else hd(L)::remvdub(remv(hd(L), tl(L)));

remvdub(["a", "b", "a", "c", "b", "a"]);


(* 6 *)
(* FUNCTION NAME: int2str *)
(* DESCRIPTION: convert a positive integer to a string; ASCII conversion *)

fun int2str(n) =
  if n = 0 then ""
  else let
    val posInt = (n mod(10))
  in
    int2str(n div(10))^implode([chr(posInt + 48)])
  end;

int2str(1234);


(* 7 *)
(* FUNCTION NAME: str2int *)
(* DESCRIPTION: convert a string to a positive integer; ASCII conversion *)

fun toTenth(num) =
  if num = 0 then 1
  else 10*toTenth(num-1);

fun char2int(c) =
  ord(c) - 48;

fun char2intL(L: char list) = 
  if null L then 0
  else let
    val conv = toTenth(length(tl(L)))
  in
    char2int(hd(L))*conv + char2intL(tl(L))
  end;

fun str2int(s) =
  char2intL(explode(s));

str2int("1234");


(* 8 *)
(* FUNCTION NAME: inde *)
(* DESCRIPTION: return the index (start from 1) of the occurence of a given value *)

fun indeRev(n, L: int list) =
  if null L then nil
  else if hd(L) = n then length(L)::indeRev(n, tl(L))
  else indeRev(n, tl(L));

fun inde(n, L) =
  rev(indeRev(n, rev(L)));

inde(1, [1,2,1,1,2,2,1]);


(* 9 *)
(* FUNCTION NAME: nele *)
(* DESCRIPTION: repeats each element in a list n times *)

fun repeat(x, n) =
  if n = 0 then nil
  else x::repeat(x, n-1);

fun nele(L, n) =
  if null L then nil
  else repeat(hd(L), n) @ nele(tl(L), n);

nele([1, 2], 3);


(* 10  *)
(* FUNCTION NAME: ntrin *)
(* DESCRIPTION: generates a list of n triangular numbers from 1 *)

fun add(n) =
  if n = 0 then 0
  else n + add(n-1);

fun ntrin(n) =
  if n = 0 then nil
  else ntrin(n-1) @ [add(n)];

ntrin(7);


(* 11 *)
(* FUNCTION NAME: isfact *)
(* DESCRIPTION: determines if a positive integer is a factorial number *)

fun help(n, x) =
  if n = 1 then true
  else if (n mod(x)) = 0 then help(n div(x),(x + 1))
  else false;

fun isfact(n) =
  if help(n, 2) then true
  else false;

isfact(120);
    

(* 12 *)
(* FUNCTION NAME: ratadd *)
(* DESCRIPTION: adds two rational numbers and returns its simplest form *)

fun gcd(m,n) =
  if m = 0 then n
  else gcd(n mod(m), m);

fun reduce(p,q) =
  let
    val d = gcd(p,q)
  in 
    (p div d, q div d)
  end;

fun ratadd((a,b),(c,d)) =
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

fun count(x:int, L) =
  if null L then 0
  else if (hd(L)) = x then 1 + count(x,tl(L))
  else count(x, tl(L));

fun occr(L) =
  if null L then nil
  else(hd(L), count(hd(L),L)) :: occr(remv(hd(L),L));

occr([1, 2, 1, 2, 3, 2]);


(* 14 *)
(* FUNCTION NAME: insfront *)
(* DESCRIPTION: higher order; insert an element as the head of eacch a=element of a list *)

fun comb(f, n, L) =
  if null L then nil
  else f(n, hd(L))::comb(f, n, tl(L));

fun insfront(n, L) = 
  comb(fn(a, b) => a::b, n, L);

insfront(1, [[1,2], nil, [3]]);


(* 15 *)
(* FUNCTION NAME: inseach *)
(* DESCRIPTION: inserts an element to each position of a list *)

fun loc(n, x, L) =
  if x = 1 then n::L
  else hd(L)::loc(n, x-1, tl(L));

fun create(n, x, L) =
  if x = 0 then nil
  else create(n, x-1, L) @ [loc(n, x, L)];

fun inseach(n, L) =
  create(n, (length(L)+1), L);

inseach(4, [1,2,3]);
