(ns logic.numbers-test
  (:refer-clojure :exclude [== max])
  (:require [clojure.core.logic :refer [run fresh ==]])
  (:use clojure.test
        logic.numbers))

(deftest numbers
  (testing "even and odds"
    (are [f x] (seq (run 1 [q] (f x)))
         even 0
         odd ['s 0]
         even ['s ['s 0]]
         odd ['s ['s ['s ['s ['s 0]]]]])
    (are [f x] (not (seq (run 1 [q] (f x))))
         even ['s 0]
         odd 0))
  (testing "comparisons"
    (are [f x y] (seq (run 1 [q] (f x y)))
         less 0 ['s 0]
         less-or-equal ['s ['s 0]] ['s ['s 0]]
         less-or-equal ['s ['s ['s 0]]] ['s ['s ['s 0]]]
         greater ['s ['s ['s]]] 0
         greater-or-equal 0 0)
    (are [f x y] (not (seq (run 1 [q] (f x y))))
         less 0 0
         greater 0 0))
  (testing "operations"
    (are [f x y r] (= (first (run 1 [q] (f x y q))) r)
         plus 0 0 0
         plus ['s ['s 0]] ['s ['s 0]] ['s ['s ['s ['s 0]]]]
         minus ['s ['s 0]] ['s 0] ['s 0]
         times ['s ['s ['s 0]]] ['s ['s 0]] ['s ['s ['s ['s ['s ['s 0]]]]]]
         division ['s ['s ['s ['s 0]]]] ['s ['s 0]] ['s ['s 0]]))
  (testing "sorting"
    (are [l r] (= (first (run 1 [q] (merge-sort l q))) r)
         [0] [0]
         [0 ['s 0]] [0 ['s 0]]
         [['s 0] 0] [0 ['s 0]]
         [['s ['s 0]] 0 ['s ['s ['s 0]]] ['s 0]] [0 ['s 0] ['s ['s 0]] ['s ['s ['s 0]]]])))

