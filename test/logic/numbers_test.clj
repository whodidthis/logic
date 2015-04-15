(ns logic.numbers-test
  (:refer-clojure :exclude [== max])
  (:use clojure.test
        logic.numbers))

(deftest numbers
  (testing "even and odd"
    (is '() (even ['s 0]))))

