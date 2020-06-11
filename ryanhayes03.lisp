;;; FUNCTION NAME: dispnth
;;; DESCRIPTION: displays the n-th element of a list
;;; NOTES: #1, assume that the input list always has n or more elements

(defun dispnth(L n)
  (if (eql n 1) (first L)
  (dispnth (rest L) (- n 1))))

(dispnth '(1 (2 3) 4 5) 2)




;;; FUNCTION NAME: delnth
;;; DESCRIPTION: deletes the n-th element of a list
;;; NOTES: #2, assume that the input list is always longer than n

(defun delnth(L n)
  (if (eql n 1) (rest L)
  (cons (first L) (delnth (rest L) (- n 1)))))

(delnth '(1 2 (3 4) 5) 3)




;;; FUNCTION NAME: remv
;;; DESCRIPTION: remove given single elements from a list
;;; NOTES: #3, includes removal of multiple appearances

(defun remv(x L)
  (if (null L) NIL
    (if (eql (first L) x) (remv x (rest L))
    (cons (first L) (remv x (rest L))))))

(remv 'a '(a (b) a c))




;;; FUNCTION NAME: remv2
;;; DESCRIPTION: remove given list elements from a list
;;; NOTES: #4, includes removal of multiple appearances

(defun remv2 (z L)
    (cond ((null L) nil)
    ((and (listp (first L)) (equal z (first L))) (remv2 z (rest L)))
    (t (cons (first L) (remv2 z (rest L))))))
      
  
(remv2 '(a b) '(a b (a b) c))




;;; FUNCTION NAME: remvdub
;;; DESCRIPTION: remove duplicate elements from a list
;;; NOTES: #5

(defun remvdub(L)
  (if (null L) NIL
  (cons (first L) (remvdub (remv (first L) (rest L))))))

(remvdub '(a b a c b a))




;;; FUNCTION NAME: remvdub2
;;; DESCRIPTION: remove duplicate elements (single elem or lists) from a list
;;; NOTES: #6

(defun helpremvdub2(a L)
  (cond ((null L) NIL)
    ((equal a (first L)) (helpremvdub2 a (rest L)))
    (t (cons (first L) (helpremvdub2 a (rest L))))))
    
(defun remvdub2(L)
  (cond ((null L) NIL)
  (t (cons (first L) (remvdub2 (helpremvdub2 (first L) (rest L)))))))
  
(remvdub2 '(a b (a) c b (a)))



;;; FUNCTION NAME: min2
;;; DESCRIPTION: computes the second smallest number in an int list
;;; NOTES: #7, assume list has at least 2 numbers


(defun helpmin2(L)
  (if (null (rest L)) (first L)
    (if (< (first L) (first (rest L))) (helpmin2 (cons (first L) (rest (rest L))))
    (helpmin2 (rest L)))))

   
(defun min2(L)
  (helpmin2 L))

(min2 '(1 3 2 5 4))




;;; FUNCTION NAME: inde
;;; DESCRIPTION: returns the index (from 1) of the occurrence of a given value
;;; NOTES: #8

(defun len(L)
  (if (null L) 0
    (+ 1 (len (rest L)))))

(defun rev(L)
  (if (null L) NIL
  (append (rev (rest L)) (list (first L)))))

(defun inderev(index L)
  (if (null L) nil
    (if (eql (first L) index) (cons (len L) (inderev index (rest L)))
    (inderev index (rest L)))))

(defun inde(index L)
  (rev (inderev index (rev L))))

(inde 1 '(1 2 1 1 2 2 1))




;;; FUNCTION NAME: nele
;;; DESCRIPTION: repeats each element in a list n times
;;; NOTES: #9

(defun repeat(x n)
  (if (eql n 0) NIL
  (cons x (repeat x (- n 1)))))
  
(defun nele(L n)
  (if (null L) NIL
  (append (repeat (first L) n) (nele (rest L) n))))
  
(nele '(1 2) 3)




;;; FUNCTION NAME: istrin
;;; DESCRIPTION: determine if a positive integer is a triangular number
;;; NOTES: #10, do not use n*(n-1) / 2

(defun helpistrin(n m)
  (cond ((equal n 0) t)
    ((< n m) NIL)
    (t (helpistrin (- n m) (+ m 1)))))

(defun istrin(n)
  (helpistrin n 0))
  
(istrin 21)




;;; FUNCTION NAME: primeton
;;; DESCRIPTION: ML equivalent of filter to find all prime numbers from 2 to n
;;; NOTES: #11

(defun myfilter(P L)
  (cond ((null L) NIL)
    ((funcall P (first L)) (cons (first L) (myfilter P (rest L))))
    (t (myfilter P (rest L)))))
      
(defun sift(L)
  (if (null L) NIL
  (cons (first L) (sift (myfilter (lambda (y) (> (mod y (first L)) 0)) (rest L))))))
  
(defun generate(x y)
  (if (> x y) NIL
  (cons x (generate (+ x 1) y))))
  
(defun primeton(n)
  (sift (generate 2 n)))
  
(primeton 20)




;;; FUNCTION NAME: powerset
;;; DESCRIPTION: returns the powerset of set {1...n}
;;; NOTES: #12, create append function

(defun pshelperTwo(a L)
  (cond ((null L) NIL)
  (t (cons (first L) (cons (cons a (first L)) (pshelperTwo a (rest L)))))))

(defun pshelper(L)
  (cond ((null L) '(nil))
  (t (pshelperTwo (first L) (pshelper (rest L))))))
  
(defun psrev(L)
  (if (null L) NIL
  (append (psrev (rest L)) (list (first L)))))
  
(defun createlist(n)
  (if (eql n 0) NIL
  (cons n (createlist (- n 1)))))

(defun powerset(n)
  (pshelper (psrev (createlist n))))
  
(powerset 2)




;;; FUNCTION NAME: mergesort
;;; DESCRIPTION: mergesort function
;;; NOTES: #13, (let) is optional

(defun split(x)
  (floor (/ x 2)))
  
(defun halflen(L)
  (split (length L)))
  
(defun combine(L R)
  (cond
    ((<= (length L) 0) R)
    ((<= (length R) 0) L)
    ((<= (first L) (first R)) (cons (first L) (combine (rest L) R)))
    ((> (first L) (first R)) (cons (first R) (combine L (rest R))))))
    
(defun left(L x)
  (cond ((<= x 0) NIL)
  (t (cons (first L) (left (rest L) (- x 1))))))
  
(defun right(L x)
  (cond ((<= x 0) L)
  (t (right (rest L) (- x 1)))))
  
(defun mergesort(L)
  (cond ((<= (length L) 1) L)
  (t (combine (mergesort (left L (halflen L))) (mergesort (right L (halflen L)))))))

(mergesort '(5 3 2 11 7))




;;; FUNCTION NAME: occr
;;; DESCRIPTION: displays the occurrence of an element of a list, or NIL if empty
;;; NOTES: #14

(defun remv(x L)
  (if (null L) NIL
    (if (eql (first L) x) (remv x (rest L))
    (cons (first L) (remv x (rest L))))))

(defun counter(x L)
  (if (null L) 0
     (if (eql (first L) x) (+ 1 (counter x (rest L)))
     (counter x (rest L)))))
     
(defun occr(L)
  (if (null L) NIL
  (cons (list (first L) (count (first L) L)) (occr (remv (first L) L)))))


(occr '(1 2 1 2 3 2))




;;; FUNCTION NAME: permu
;;; DESCRIPTION: generates permutation of the identity list from 1 to n
;;; NOTES: #15, mapcar function is optional, append is allowed, &optional

(defun rev(L)
  (if (null L) NIL
  (append (rev (rest L)) (list (first L)))))

(defun createlist(n)
  (cond ((eql n 0) NIL)
  (t (cons n (createlist (- n 1))))))
  
(defun permuhelper (L &optional (rem L))
  (cond ((null rem) NIL)
        ((null (rest L)) (list L))
        (t (append (mapcar (lambda (y) (cons (first L) y)) (permuhelper (rest L)))
           (permuhelper (append (rest L) (list (first L))) (rest rem))))))
           
(defun permu(n)
  (permuhelper (rev (createlist n))))
  

(permu 3)

