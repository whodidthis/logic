(ns logic.numbers-fun
  (:refer-clojure :exclude [even? pos? odd?])
  (:require [clojure.core.match :refer [match]]
            [defun :refer [defun]]))

(defun natural-number?
  ([0] true)
  ([['s x]] (recur x))
  ([_] false))

(defun even?
  ([0] true)
  ([['s ['s x]]] (recur x))
  ([_] false))

(defun odd?
  ([['s 0]] true)
  ([['s ['s x]]] (recur x))
  ([_] false))

(defun less?
  ([0 ['s y]] true)
  ([['s x] ['s y]] (recur x y))
  ([_ _] false))

(defun less-or-equal?
  ([0 0] true)
  ([x y] (less? x y)))

(defn greater? [x y]
  (less y x))

(defn greater-or-equal? [x y]
  (less-or-equal y x))

(defun plus
  ([0 y] y)
  ([['s x1] y] (recur x1 ['s y])))

(defun minus
  ([x 0] x)
  ([['s x1] ['s y1]] (recur x1 y1)))

(defun times
  ([0 y a] a)
  ([['s x1] y a] (recur x1 y (plus a y)))
  ([x y] (recur x y 0)))

(defun division
  ([x y a] (if (less x y)
             a
             (recur (minus x y) y ['s a])))
  ([x y] (if (greater? y 0) (recur x y 0))))

(defun merger
  ([[] y r] (concat r y))
  ([x [] r] (concat r x))
  ([([x1 & xs] :seq) ([y1 & ys] :seq) r]
   (if (less-or-equal? x1 y1)
     (recur xs (cons y1 ys) (conj r x))
     (recur (cons x1 xs) ys (conj r y)))))

(defn merger [x y r]
  (match [x y r]
    [[] y r] (concat r y)
    [x [] r] (concat r x)
    ))

(defun merger
  ([[] y r] (vec (concat r y)))
  ([x [] r] (vec (concat x r)))
  ([x  y r]
   (let [x1 (first x)
         y1 (first y)]
     (if (less-or-equal? x1 y1)
       (recur (subvec x 1) y (conj r x1))
       (recur x (subvec y 1) (conj r y1))))))

(rest [1 2 3])
(subvec [1 2 3] 1)

(defun merge-sort
  ([[]] [])
  ([]))

(merger [['s 0]] [0] [])
(merger [['s 0]] [] [])

(less-or-equal? ['s 0] 0)
(less-or-equal? ['s 0] 0 )

(match [ []]
       [([] :seq)] "woop"
       :else false)

(match [[1 3 4]]
       [[x1 & [2 3]]] :yeh
       [[x1 & [3 4]]] :meh)

(match [[1 2 3]]
       [([x1 & xs] :seq)] xs)
(let [[x1 & xs] [1 2 3]] xs)
