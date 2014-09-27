#lang scheme
;CHAPTER 1 - TOYS
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;CHAPTER 2 - DO IT AGAIN AND AGAIN
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
;CHAPTER 3 - CONS THE MAGNIFICENT
(define rember ;remove member
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))
(define firsts ;make a list of the firsts item in a list
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))))))
(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (second (car l)) (seconds (cdr l)))))))
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else 
       (cond
         ((eq? (car lat) old) (cons old (cons new (cdr lat))))
         (else (cons (car lat) (insertR new old (cdr lat)))))))))
(define insertL 
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      (else 
       (cond
         ((eq? (car lat) old) (cons new lat))
         (else (cons (car lat) (insertL new old (cdr lat)))))))))
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) old) (cons new (cdr lat)))
         (else (cons (car lat) (subst new old (cdr lat)))))))))
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
         (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))
(define multirember ;remove all of a, such that it is at the first level
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) a) (multirember a (cdr lat))) ;continue recursing, even after finding first a
         (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
         (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
         (else (cons (car lat) (multisubst new old (cdr lat)))))))))
;CHAPTER 4 - NUMBERS
(define add1
  (lambda (n)
    (+ n 1)))
(define sub1
  (lambda (n)
    (- n 1)))
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
(define o-
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))
(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (* n (sub1 m)))))))
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))
(define < 
  (lambda (n m)
    (cond 
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))
(define = 
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))
(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (^ n (sub1 m)))))))
(define quotient 
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (quotient (o- n m) m))))))
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
(define one?
  (lambda (n)
    (= n 1)))
(define pick
  (lambda (n lat)
    (cond 
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else (cons (car lat) (no-nums (cdr lat)))))))))
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? (car lat) a) (add1 (occur a (cdr lat))))
         (else
          (occur a (cdr lat))))))))
;CHAPTER 5 - IT'S FULL OF STARS
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l))))))) 
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) 
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))
;CHAPTER 6 - SHADOWS
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else 
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
;deprecated (define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((eq? (operator nexp) (quote +)) (o+ (1st-sub-exp nexp) (2nd-sub-exp nexp)))
;      ((eq? (operator nexp) (quote x)) (* (1st-sub-exp nexp) (2nd-sub-exp nexp)))
;      (else (^ (1st-sub-exp nexp) (2nd-sub-exp nexp))))))
(define 1st-sub-exp ;to simplify our 'value' function
  (lambda (aexp)
    (car (cdr aexp))))
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr(cdr aexp)))))
(define operator
  (lambda (aexp)
    (car aexp)))
(define sero? ;different representation of numbers, where () is 0 and (()()) is 2
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons (quote ()) n)))
(define zub1
  (lambda (n)
    (cdr n)))
;CHAPTER 7 - FRIENDS AND RELATIONS
;a set is a list in which each atom appears at most once
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))
(define makeset2 ;using multirember
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat) (multirember (car lat) (makeset (cdr lat))))))))
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))
(define intersect ;definition in the book
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      (else (cond
              ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
              (else (intersect (cdr set1) set2)))))))
(define intersect2 ;to make sure duplicates do not occur
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      (else (cond
              ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) (rember (car set1) set2))))
              (else (intersect (cdr set1) set2)))))))
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2)) 
      (else
       (cons (car set1) (union (cdr set1) set2))))))
(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2) (difference (cdr set1) set2)) 
      (else
       (cons (car set1) (difference (cdr set1) set2))))))
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))
(define a-pair?
  (lambda (x)
    (cond
      ((null? x) #f)
      ((atom? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define third
  (lambda (p)
    (car (cdr (cdr p)))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))
;rel - relation - list of pairs
;fun - functions; at most one output for each input
(define fun?
  (lambda (rel)
    (set? (firsts rel))))
(define revrel ;reverse the relation (inverse)
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))
;fullfun - one-to-one function
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
;craziness - rember-f returns a function that takes two values, a and l
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
(define rember-eq? (rember-f eq?)) ;deprecated by just saying ((rember-f eq?) a l)
(define insertL-f
  (lambda (test?)
    (lambda (old new l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old) (cons new l))
        (else (cons (car l) ((insertL-f test?) old new (cdr l))))))))
(define insertR-f
  (lambda (test?)
    (lambda (old new l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) old new (cdr l))))))))
(define seqL 
  (lambda (new old l)
    (cons new (cons old l))))
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))
;(define insertL (insert-g seqL))
;(define insertR (insert-g seqR))
;(define insertL (insert-g (lambda (new old l) (cons new (cons old l))))) <--this is clearer than having to remember what 'seq' stands for
(define seqS
  (lambda (new old l)
    (cons new l)))
;(define subst (insert-g seqS))
(define atom-to-function
  (lambda (x)
    (cond 
      ((eq? x (quote +)) o+)
      ((eq? x (quote x)) *)
      (else ^))))
;deprecated by chapter 10 
;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      (else
;       ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((eq? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else 
         (cons (car lat) ((multirember-f test?) a (cdr lat))))))))
;(define multirember-eq? (multirember test?))
(define eq?-tuna
  (eq?-c (quote tuna)))
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))
(define a-friend
  (lambda (x y)
    (null? y)))
(define multirember&co
  (lambda (a lat col) ;col stands for 'collector' or 'continuation'
    (cond
      ((null? lat) (col (quote ()) (quote())))
      ((eq? (car lat) a) (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
      (else
       (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))
;(define new-friend
;  (lambda (newlat seen)
;    (col newlat (cons (car lat) seen))))
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat cons (quote tuna) seen)))
(define latest-friend
  (lambda (newlat seen) (a-friend (cons (quote and) newlat) seen)))
(define last-friend
  (lambda (x y)
    (length x)))
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL) (cons oldL (multiinsertLR new oldL oldR (cdr lat))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R)))) ;L and R are the sumber of insertions that have occurred
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons (car lat) newlat L R ))))))))
(define even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) 
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* (car l) p) s))))
         (else (evens-only*&co (cdr l) (lambda (newl p s) col newl p (o+ car l) s)))))
      (else (evens-only*&co (car l) (lambda (al ap as) (evens-only*&co (cdr l) (lambda (dl dp ds) (col (cons al dl) (* ap dp) (o+ as ds))))))))))
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
;partial function -- it may never stop!
(define keep-looking ;unnatural recursion because it does not recur on a part of lat
  (lambda (a sorn lat) ;sorn - symbol or number
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else
       (eq? sorn a)))))
(define eternity ;the most partial function. It never reaches its goal.
  (lambda (x)
    (eternity x)))
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
(define align ;not a partial function
  (lambda (pora)
    (cond 
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora))) ;shift creates an argument for align that is not part of original argument -- violates 7th commandment
      (else (build (first pora) (align (second pora)))))))
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else 
       (o+ (length* (first pora)) (length* (second pora)))))))
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (o+ (* (weight* (first pora)) 2) (weight* second pora))))))
(define shuffle ;a partial function
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))
(define C ;Lothar Collatz conjecture
  (lambda (n)
    (cond
      ((one? n) 1)
      (else 
       (cond
         ((even? n) (C (quotient n 2)))
         (else (C (add1 (* 3 n)))))))))
(define A ;Ackermann function
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))
;cannot define a function will-stop?
(lambda (l) ;name: length0
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l)))))) ;determines the length of the empty list
(lambda (l) ;name: length<=1
  (cond 
    ((null? l) 0)
    (else
     (add1 ((lambda (l) 
              (cond
                ((null? l) 0)
                (else (add1 (eternity (cdr l)))))) (cdr l))))))
((lambda (mk-length) ;this function creates length0
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))

((lambda (mk-length) ;this function creates length1
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity) (cdr l))))))))

((lambda (mk-length) ;length<=2
   (mk-length (mk-length (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (mk-length) ;length<=2
   (mk-length (mk-length (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
((lambda (mk-length) ;length<=3
   (mk-length (mk-length (mk-length (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;recursion is like an infinite tower of applications of mk-length to an arbitrary function
((lambda (mk-length) ;this function creates length
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x) 
      ((mk-length mk-length) x)))))
(lambda (le)
  ((lambda (mk-length) (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))
(define Y ;the applicative Y function
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
;CHAPTER 10-What is the value of all this?
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry)) entry-f))
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))
(define extend-table cons)
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table) (lambda (name)
                                                (lookup-in-table name (cdr table) table-f)))))))
(define expression-to-action ;reminiscent of atom-to-function from chapter 8
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))
(define list-to-action
  (lambda e
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))
(define value
  (lambda (e)
    (meaning e (quote()))))
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))
(define *quote
  (lambda (e table)
    (text-of e)))
(define text-of second)
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
(define initial-table
  (lambda (name)
    (car (quote()))))
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))
(define table-of first)
(define formals-of second)
(define body-of third)
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))
(define question-of first)
(define answer-of second)
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else 
       (cons (meaning (car args) table) (evlis (cdr args) table))))))
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))
(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))
(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons)) (cons (first vals) (second vals)))
      ((eq? name (quote car)) (car (first vals)))
      ((eq? name (quote cdr)) (cdr (first vals)))
      ((eq? name (quote null?)) (null? (first vals)))
      ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
      ((eq? name (quote atom?)) (:atom? (first vals)))
      ((eq? name (quote zero?)) (zero? (first vals)))
      ((eq? name (quote add1)) (add1 (first vals)))
      ((eq? name (quote sub1)) (sub1 (first vals)))
      ((eq? name (quote number?)) (number? (first vals))))))
(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive)) #t)
      ((eq? (car x) (quote non-primitive)) #t)
      (else #f))))
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals) (table-of closure)))))
(define new-entry build)
