;; Core hlisp functions

(def identity (lambda (x) x))

(defn (partial f x)
  (lambda (y . ys)
    (apply f (cons x (cons y ys)))))

(defn (flip f)
  (lambda (x y)
    (f y x)))

(def inc (partial + 1))

(defn (dec x) (- x 1))

(defn (not x)
  (if x #f #t))

(defn (comp f g)
  (lambda (x) (f (g x))))

(defn (complement f) (comp not f))

;; Sequence utils

(defn (map f xs)
  (if (nil? xs)
      nil
      (cons (f (car xs)) (map f (cdr xs)))))

(defn (foldl f x0 xs)
  (if (nil? xs)
       x0
       (foldl f (f x0 (car xs)) (cdr xs))))

(defn (foldr f x0 xs)
  (if (nil? xs)
      x0
      (f (foldr f x0 (cdr xs)) (car xs))))

(def reduce foldl)

(defn (sum xs) (reduce + 0 xs))

(defn (product xs) (reduce * 1 xs))

(defn (filter pred xs)
  (cond
    ((nil? xs)       nil)
    ((pred (car xs)) (cons (car xs) (filter pred (cdr xs))))
    (#t              (filter pred (cdr xs)))))

(def remove (lambda (pred xs) (filter (complement pred) xs)))

(defn (range start end)
  (if (>= start end)
      '()
      (cons start (range (inc start) end))))

(defn (into to from)
  (reduce (flip cons) to from))

(defn (concat xs ys)
  (cond
    ((nil? xs) ys)
    ((nil? ys) xs)
    (#t        (cons (car xs) (concat (cdr xs) ys)))))

(defn (reverse xs)
  (if (nil? xs)
    '()
    (concat (reverse (cdr xs)) (cons (car xs) nil))))

(defn (list . xs) xs)

(defn (seq coll)
  (apply list coll))

(defn (vec coll)
  (into [] coll))

;; Number utils

(def zero? (partial = 0))

(def pos? (partial < 0))

(def neg? (partial > 0))

(defn (>= x y)
  (or (> x y) (= x y)))

(defn (<= x y)
  (or (< x y) (= x y)))
