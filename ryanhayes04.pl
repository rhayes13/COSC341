/* FUNCTION NAME: dispnth */
/* DESCRIPTION: displays the n-th element of a list */
/* NOTES: #1, assume that the input list always has n or more elements */

dispnth([H|_], 1, H).
dispnth([_|T], N, Ans):-
  X is N-1,
  dispnth(T, X, Ans).


  

/* FUNCTION NAME: delnth */
/* DESCRIPTION: deletes the n-th element of a list */
/* NOTES: #2, assume that the input list is always longer than n */

delnth([], _, []):- !.
delnth([H|T], 1, T):- !.
delnth([H|T], N, [H|T1]):-
  N1 is N-1,
  delnth(T, N1, T1).




/* FUNCTION NAME: remv */
/* DESCRIPTION: remove given single elements from a list */
/* NOTES: #3, includes removal of multiple appearances */

remv(_, [], []).
remv(A, [A|L], X):-
  !,
  remv(A, L, X).
remv(A, [B|L], [B|X]):-
  remv(A, L, X).




/* FUNCTION NAME: remvdub */
/* DESCRIPTION: remove duplicate elements from a list */
/* NOTES: #4 */

remvdub([], []).
remvdub([H|T], Ans):-
  remv(H, [H|T], X1),
  remvdub(X1, X2),
  Ans = [H|X2].




/* FUNCTION NAME: maxl */
/* DESCRIPTION: predicate to find max integer of an int list */
/* NOTES: #5 */

max2([], X, X).
max2([H|T], X, Ans):-
  H > X,
  max2(T, H, Ans).
max2([H|T], X, Ans):-
  max2(T, X, Ans).
  
maxl(L, Ans):-
  max2(L, 0, Ans).




/* FUNCTION NAME: sum1 */
/* DESCRIPTION: returns the sum of an int list */
/* NOTES: #6, use atomic to test whether the input is a list */

is_atomic([]).
is_atomic([H|T]):-
  atomic(H),
  is_atomic(T).

suml([], 0).
suml([H|T], Ans):-
  is_atomic(H),
  suml(H, Ans1),
  suml(T, Ans2),
  Ans is Ans1 + Ans2.
suml([H|T], Ans):-
  suml(T, Ans1),
  Ans is Ans1 + H.



  
/* FUNCTION NAME: inde */
/* DESCRIPTION: returns the index (from 1) of the occurrence of a given value */
/* NOTES: #7; increment V each call if null L then [] */

inde(_, [], _, []).
inde(X, L, Ans):-
  inde(X, L, 1, Ans).
inde(X, [X|XS], V, Ans):-
  V2 is V+1,
  inde(X, XS, V2, Ans2),
  Ans = [V|Ans2].
inde(X, [_|XS], V, Ans):-
  V2 is V+1,
  inde(X, XS, V2, Ans2),
  Ans = Ans2.




/* FUNCTION NAME: nele */
/* DESCRIPTION: repeats each element in a list n times */
/* NOTES: #8 */

nele2([], _, _, []).
nele2([H|T], X, Y, Ans):-
  X = 1,
  nele2(T, Y, Y, Z),
  Ans = [H|Z].
nele2([H|T], X, Y, Ans):-
  X1 is X-1,
  nele2([H|T], X1, Y, Z),
  Ans = [H|Z].
  
nele([], _, []).
nele(L, N, Ans):-
  nele2(L, N, N, Ans).




/* FUNCTION NAME: primeton */
/* DESCRIPTION: finds all prime numbers from 2 to n */
/* NOTES: #9 */

make(X, X, [X]):- !.
make(H, N, [H|T]):-
  Next is H+1,
  make(Next, N, T).
  
sieve([], []).
sieve([X|T], [X|T1]):-
  sift(X, T, S),
  sieve(S, T1).
  
sift(_, [], []):- !.
sift(X, [H|T], S):-
  0 is H mod X,
  !,
  sift(X, T, S).
  
sift(X, [H|T], [H|S]):-
  sift(X, T, S).

primeton(N, X):-
  make(2, N, L),
  sieve(L, X).




/* FUNCTION NAME: mergesort */
/* DESCRIPTION: mergesort function */
/* NOTES: #11 */

halve([], [], []).
halve([X], [X], []).
halve([E1 | [E2|Half]], [E1|R1], [E2|R2]):-
  halve(Half, R1, R2).
  
merge([], L, L).
merge(L, [], L).
merge([E1|Half1], [E2|Half2], R):-
  E1 < E2,
  merge(Half1, [E2|Half2], R1),
  R = [E1|R1].
merge([E1|Half1], [E2|Half2], R):-
  E1 >= E2,
  merge([E1|Half1], Half2, R1),
  R = [E2|R1].
  
mergesort([], []).
mergesort([X], [X]).
mergesort(L, R):-
  halve(L, L1, L2),
  mergesort(L1, R1),
  mergesort(L2, R2),
  merge(R1, R2, R), !.
