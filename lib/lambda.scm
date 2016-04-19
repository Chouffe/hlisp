(load "lib/core.scm")

(def make-pair
  (lambda (x)
    (lambda (y)
      (lambda (c)
        ((c x) y)))))

(def select-first
  (lambda (x)
    (lambda (y)
      x)))

(def select-second
  (lambda (x)
    (lambda (y)
      y)))

; Boolean Logic

(def condition make-pair)

(def true select-first)

(def false select-second)

; An if then else is statement becomes
; (((condition thenBranch) elseBranch) boolean)

(((condition 42) 41) true)

(def not
  (lambda (x)
    (((condition false) true) x)))

(def m-and
  (lambda (x)
    (lambda (y)
      (((condition y) false) x))))

(def m-or
  (lambda (x)
    (lambda (y)
      (((condition true) y) x))))

; Helper function for converting to boolean values #t #f

(defn (->boolean bool)
  (((condition #t) #f) bool))

(->boolean false)

(->boolean true)


; Natural numbers

(def zero identity)

(def succ
  (lambda (n)
    (lambda (s)
      ((s false) n))))

(def one (succ zero))

(def two (succ one))

(def zero?
  (lambda (n)
    (n select-first)))

; pred of zero is zero

(def pred
  (lambda (n)
  (((condition zero) (n select-second)) (zero? n))))

; Helper function for converting to natural values 0..N

(defn (->nat n)
  (if (->boolean (zero? n))
    0
    (+ 1 (->nat (pred n)))))

; Recursion

(def add1
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (if (->boolean (zero? y))
          x
          (((f f) (succ x)) (pred y)))))))

(def add2
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (if (->boolean (zero? y))
          x
          ((f (succ x)) (pred y)))))))

; Wont work because of
; (((condition x) (((f f) (succ x)) (pred y))) (zero? y))

(def add (add1 add1))

(defn (mult1 f)
  (lambda (x)
    (lambda (y)
      (if (->boolean (zero? y))
        zero
        ((add x) (((f f) x) (pred y)))))))

(def recursive
  (lambda (f)
    ((lambda (s) (f (s s)))
     (lambda (s) (f (s s))))))

(def mult (mult1 mult1))

(def power1
  (lambda (f)
    (lambda (x)
      (lambda (pow)
        (if (->boolean (zero? pow))
          one
          ((mult x) (((f f) x) (pred pow))))))))

(def power (power1 power1))

(def sub1
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (if (->boolean (zero? y))
          x
          (( (f f) (pred x)) (pred y)))))))

(def sub (sub1 sub1))
