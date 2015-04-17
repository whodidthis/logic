(ns logic.a1-fun
  (:refer-clojure :exclude [== odd? even? take drop max merge])
  (:require [defun :refer [defun]])
  (:use logic.numbers-fun))

(def tree1 ['tree ['s ['s ['s 0]]]
            ['tree ['s ['s 0]]
             'void
             'void]
            ['tree ['s 0]
             'void
             'void]])
(def tree2 ['tree ['s ['s 0]]
            'void
            ['tree ['s ['s 0]]
             'void
             'void]])
(def tree3 ['tree ['s 0]
            ['tree ['s ['s 0]]
             'void
             ['tree ['s ['s ['s ['s 0]]]]
              'void
              'void]]
            'void])

(def tree4 ['tree ['s 0]
            ['tree ['s ['s 0]]
             ['tree ['s ['s ['s 0]]]
              'void
              'void]
             ['tree ['s ['s ['s ['s 0]]]]
              'void
              'void]]
            ['tree ['s ['s ['s ['s ['s 0]]]]]
             'void
             ['tree ['s ['s ['s ['s ['s ['s 0]]]]]]
              'void
              'void]]])

(defun pre-order
  ([tree] (pre-order tree []))
  (['void _] [])
  ([['tree v l r] a] (concat a [v] (pre-order l []) (pre-order r []))))

(defun in-order
  ([tree] (in-order tree []))
  (['void _] [])
  ([['tree v l r] a] (concat a (in-order l []) [v] (in-order r []))))

(defun post-order
  ([tree] (post-order tree []))
  (['void _] [])
  ([['tree v l r] a] (concat a (post-order r) [v] (post-order l))))

(defn even-sum [tree]
  (->> (pre-order tree)
       (filter even?)
       (apply plus)))

(defn greatest-even-sum [trees]
  (apply max (map even-sum trees)))
