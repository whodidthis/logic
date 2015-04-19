(ns logic.numbers-fun
  (:refer-clojure :exclude [even? odd? take drop merge max integer?])
  (:require [clojure.core.match :refer [match]]
            [defun :refer [defun]]))

(defun natural-number?
  ([0] true)
  ([['s x]] (recur x))
  ([_] false))

(defn integer? [x]
  (match x
    0 true
    ['s x1] (recur x1)
    ['m x1] (recur x1)
    :else false))

(defun even?
  ([0] true)
  ([['s ['s x]]] (recur x))
  ([['m ['m x]]] (recur x))
  ([_] false))

(defun odd?
  ([['s 0]] true)
  ([['s ['s x]]] (recur x))
  ([['m 0]] true)
  ([['m ['m x]]] (recur x))
  ([_] false))

(defun less?
  ([['m x1] 0] true)
  ([['m x1] ['m y1]] (recur x1 y1))
  ([['m x1] ['s y1]] true)
  ([0 ['s y1]] true)
  ([['s x1] ['s y1]] (recur x1 y1))
  ([_ _] false))

(less? 0 0)
(less? ['m ['m 0]] ['m 0])
(less? ['s 0] ['m 0])
(greater? ['s ['s 0]] ['m ['m ['m 0]]])

(defun less-or-equal?
  ([0 0] true)
  ([x y] (less? x y)))

(defn greater? [x y]
  (less? y x))

(defn greater-or-equal? [x y]
  (less-or-equal? y x))

(defun plus
  ([x] x)
  ([0 y] y)
  ([['s x1] y] (recur x1 ['s y]))
  ([['m x1] y] (recur x1))
  ([x y & more] (reduce plus (plus x y) more)))

(plus 0 ['m 0])
(plus 0 ['s 0] ['s ['s 0]] ['m 0] ['m ['m 0]])

(defun neg
  ([0] 0)
  ([['s x1]] ['m (neg x1)])
  ([['m x1]] ['s (neg x1)]))

(defun minus
  ([x] x)
  ([x 0] x)
  ([0 ['s y1]] (neg ['s y1]))
  ([0 ['m y1]] (neg ['m y1]))
  ([['s x1] ['s y1]] (recur x1 y1))
  ([['m x1] ['s y1]] (recur ['m ['m x1]] y1))
  ([x y] (plus x (neg y)))
  ([x y & more] (reduce minus (minus x y) more)))

(minus 0 ['m 0])
(minus 0 ['s 0])
(minus ['s ['s ['s 0]]] ['s ['s 0]] ['m 0] ['m 0] ['m ['m 0]] ['m ['m 0]] ['s ['s ['s ['s 0]]]])
(minus ['s 0] ['m ['m ['m ['m 0]]]])
(minus ['m 0] ['m 0])
(minus 0 ['s 0])
(minus 0 0)
(minus 0 ['s ['s 0]])
(minus ['s 0] ['m 0])
(minus ['s 0] ['s 0])
(minus ['s ['s 0]] ['s ['s ['s ['s ['s ['s 0]]]]]])
(minus ['s 0] 0)

(defun times
  ([0 y a] a)
  ([['s x1] y a] (recur x1 y (plus a y)))
  ;  ([['m x1] ['m y1] a] (times (neg x1) ['m y1] (minus a ['m y1])))
  ;  ([['m x1] y a] (neg (times (neg x1) y (plus a y))))
  ([x y] (recur x y 0)))

(times ['s ['s 0]] ['s ['s 0]])
(times ['m ['m 0]] ['s ['s 0]])
(times ['m ['m 0]] ['m ['m 0]])

(defun division
  ([x y a] (if (less? x y)
             a
             (recur (minus x y) y ['s a])))
  ([x y] (if (greater? y 0) (recur x y 0))))

(defun max
  ([x] x)
  ([x y] (if (greater? x y) x y))
  ([x y & more] (reduce max (max x y) more)))

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

