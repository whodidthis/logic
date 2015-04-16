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
  ([0 ['s y]] true)
  ([['s x] ['s y]] (recur x y)))

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

(times ['s ['s 0]] ['s ['s 0]])

(defn pos? [x] (greater? x 0))

(defun division
  ([x y a] (if (less x y)
             a
             (recur (minus x y) y ['s a])))
  ([x y] (if (pos? y) (recur x y 0))))
