(ns logic.numbers-fun-test
  (:refer-clojure :exclude [== max even? odd? merge drop take])
  (:require [clojure.core.logic :refer [run fresh ==]])
  (:use clojure.test
        logic.numbers-fun))

(deftest numbers
  (testing "even and odds"
    (are [f x] (f x)
      even? 0
      odd? ['s 0]
      even? ['s ['s 0]]
      odd? ['s ['s ['s ['s ['s 0]]]]])
    (are [f x] (not (f x))
      even? ['s 0]
      odd? 0))
  (testing "comparisons"
    (is (less? 0 ['s 0]))
    (is (greater? ['s ['s 0]] ['s 0]))
    (are [f x y] (not (f x y))
      less? 0 0
      greater? 0 0))
  (testing "operations"
    (are [f x y r] (= (f x y) r)
      plus 0 0 0
      plus ['s ['s 0]] ['s ['s 0]] ['s ['s ['s ['s 0]]]]
      minus ['s ['s 0]] ['s 0] ['s 0]
      times ['s ['s ['s 0]]] ['s ['s 0]] ['s ['s ['s ['s ['s ['s 0]]]]]]
      division ['s ['s ['s ['s 0]]]] ['s ['s 0]] ['s ['s 0]]))
  (testing "list handling"
    (are [f x y r] (= (f x y) r)
      take ['s ['s 0]] ['a 'b 'c 'd] ['a 'b]
      drop ['s ['s 0]] ['a 'b 'c 'd] ['c 'd]))
  (testing "sorting"
    (are [l r] (= (merge-sort l) r)
      [0] [0]
      [0 ['s 0]] [0 ['s 0]]
      [['s 0] 0] [0 ['s 0]]
      [['s ['s 0]] 0 ['s ['s ['s 0]]] ['s 0]] [0 ['s 0] ['s ['s 0]] ['s ['s ['s 0]]]])))
