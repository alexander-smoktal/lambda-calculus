#lang racket

;; Booleans
(define (True a b) a)
(define (False a b) b)
(define (If c a b) (c a b))

(define (And p q) (p q p))
(define (Or p q) (p p q))
(define (Not p) (p False True))

;; Natural numbers
(define (Succ n f x) (f (n f x)))
(define (Pred n f x) ((n (lambda (g) (lambda (h) (h (g f)))) (lambda (u) x)) (lambda (u) u)))

(define (Null f x) x)
(define (One f x) (Succ Null f x))
(define (Two f x) (Succ One f x))
(define (Three f x) (Succ Two f x))
(define (Four f x) (Succ Three f x))
(define (Five f x) (Succ Four f x))

(define (Plus m n) (lambda (f x) (m f (n f x))))
(define (Minus m n) (lambda (f x) ((n (lambda (t) (lambda (r s) (Pred t r s))) m) f x)))
(define (MinusC m n) (lambda (f x) ((n (lambda (t) (curry Pred t)) m) f x)))
(define (Mul m n) (lambda (f x) (m (lambda (y) (n f y)) x)))
(define (IsZero n) (n (lambda (x) False) True))
(define (Leq m n) (IsZero (Minus m n)))
(define (Eq m n) (And (Leq m n) (Leq n m)))

(define (Ten f x) ((Mul Five Two) f x))
(define (Hundred f x) ((Mul Ten Ten) f x))

(define (PlusOne x) (+ 1 x))
(define (Concat x) (string-append x "|"))

(define (NatToNumber m) (display (m PlusOne 0)))
(define (NatToString m) (display (m Concat "")))
  
;; Pairs
(define (Pair a b) (lambda (f) (f a b)))
(define (First p) (p (lambda (a b) a)))
(define (Second p) (p (lambda (a b) b)))

;; Lists
(define (Nil) (Pair True True))
(define (IsNil l) (First l))
(define (Cons h t) (Pair False (Pair h t)))
(define (Head l) (First (Second l)))
(define (Tail l) (Second (Second l)))

(define (ProcessList l f) ((If (IsNil l) void (lambda ()(f (Head l))))))
(define (ConsZeroList n) (n (lambda (l) (Cons Null l)) (Nil)))
(define (ConsRangeList n) (Second (n 
                                  (lambda (p) (Pair (Minus (First p) One) (Cons (First p) (Second p))))
                                  (Pair n (Nil)))))

;; Y-combinator
(define (Y f) ((lambda (x) (x x)) (lambda (x) (f (lambda (y) ((x x) y))))))

;Natural numbers
(define (Fac n)
  ((Y 
    (lambda (r)
      (lambda (m)
        ((If (Eq m Null) (lambda () One)
          (lambda () (Mul m (r (Minus m One))))))))) n))

;Lists
(define (PrintList l)
  (display "[")
  ((Y 
    (lambda (r)
      (lambda (m)
        ((If (IsNil m)
          void
          (lambda () 
            (NatToNumber (Head m))
            (display ", ")
            (r (Tail m)))))))) l)
  (display "]"))

(define (Map l f)
  ((Y 
    (lambda (r)
      (lambda (m)
        ((If (IsNil m)
          Nil
          (lambda () (Cons (f (Head m)) (r (Tail m))))))))) l))

(define (Filter l f)
  ((Y 
    (lambda (r)
      (lambda (m)
        ((If (IsNil m) Nil
          (lambda ()
            ((If (f (Head m))
              (lambda () (Cons (Head m) (r (Tail m))))
              (lambda () (r (Tail m))))))))))) l))

(define (Reduce l f z)
  ((Y 
    (lambda (r)
      (lambda (m)
        ((If (IsNil m) 
          (lambda () z)
          (lambda () (f (Head m) (r (Tail m))))))))) l))

