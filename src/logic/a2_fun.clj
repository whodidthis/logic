(ns logic.a2-fun
  (:refer-clojure :exclude [== odd? even? take drop max merge])
  (:require [defun :refer [defun]]
            [clojure.core.match :refer [match]])
  (:use logic.numbers-fun))

(defn neg-integer? [x]
  (match x
         ['m 0] true
         ['m x1] (recur x1)
         :else false))

(defun abs
  ([0] 0)
  ([['s x1]] ['s x1])
  ([['m x1]] ['s (abs x1)]))

(def tree1 ['tree [['m 0] ['s ['s 0]]]
            ['tree [['m ['m ['m 0]]] ['s ['s 0]] ['m ['m 0]]]
             'void
             'void]
            ['tree [['s ['s 0]] ['s 0] ['m 0]]
             'void
             'void]])

(defun choose-greatest
  (['void] 'void)
  ([['tree x l r]]
   (let [greatest (apply max (map abs x))]
     ['tree greatest (choose-greatest l) (choose-greatest r)])))

(choose-greatest tree1)
