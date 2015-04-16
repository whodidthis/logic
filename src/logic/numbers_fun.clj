(ns logic.numbers-fun
  (:refer-clojure :exclude [even? odd? take drop merge])
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
  (less? y x))

(defn greater-or-equal? [x y]
  (less-or-equal? y x))

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
  ([x y a] (if (less? x y)
             a
             (recur (minus x y) y ['s a])))
  ([x y] (if (greater? y 0) (recur x y 0))))

(defun length
  ([([] :seq)] 0)
  ([([x1 & xs] :seq)] (recur xs ['s 0]))
  ([([] :seq) a] a)
  ([([x1 & xs] :seq) a] (recur xs ['s a])))

#_
(defn length
  ([x] (match [x]
         [([] :seq)] 0
         [([x1 & xs] :seq)] (length xs ['s 0])))
  ([x a] (match [x a]
           [([] :seq) a] a
           [([x1 & xs] :seq) a] (recur xs ['s a]))))

(defun take
  ([0 x] [])
  ([n x] (recur n x []))
  ([0 x a] a)
  ([['s n] ([] :seq) a] a)
  ([['s n] ([x1 & xs] :seq) a] (recur n xs (conj a x1))))

(take ['s ['s ['s ['s 0]]]] ['a 'b 'c])

(defun drop
  ([0 x] x)
  ([['s n] ([] :seq)] [])
  ([['s n] ([_ & xs] :seq)] (recur n xs)))

(drop ['s ['s ['s ['s ['s 0]]]]] ['a 'b 'c 'd])

(defun merge
  ([x y] (recur x y []))
  ([([] :seq) y r] (concat r y))
  ([x ([] :seq) r] (concat r x))
  ([([x1 & xs] :seq) :as x ([y1 & ys] :seq) :as y r]
    (if (less-or-equal? x1 y1)
      (recur xs y (conj r x1))
      (recur x ys (conj r y1)))))

(defun merge-sort
  ([([] :seq)] [])
  ([([x1] :seq)] [x1])
  ([([_ & _] :seq) :as x]
   (let [size (division (length x) ['s ['s 0]])]
     (merge (merge-sort (take size x)) (merge-sort (drop size x))))))

(length [])
(merge-sort [['s ['s 0]] ['s ['s ['s 0]]] 0 0 ['s 0] 0])
(merge-sort ['a 'b 'c 'd 'e])

(merger [0] [['s 0]])
(merger [['s 0]] [0])

(merger [['s 0]] [] [])

(less-or-equal? ['s 0] 0)
(less-or-equal? ['s 0] 0 )

(match [ [1 2]]
       [([] :seq)] "woop"
  [([x1] :seq)] "wuup"
       :else false)

(match [[1 2 3] 5]
  [([x1 & xs] :seq) :as x y] [x y])

(match [[1]]
  [([] :seq)] "juba"
  [([x1] :seq)] "jaba"
  [([x1 & xs] :seq)] "tsuba"
  :else 2)

(match [[1 3 4]]
       [[x1 & [2 3]]] :yeh
       [[x1 & [3 4]]] :meh)

(match [[1 2 3]]
       [([x1 & xs] :seq)] xs)
(let [[x1 & xs] [1 2 3]] xs)

