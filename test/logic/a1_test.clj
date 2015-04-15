(ns logic.a1-test
  (:refer-clojure :exclude [== max])
  (:use clojure.test
        clojure.core.logic
        logic.a1))

(deftest mysteries
  (testing "finds even sums"
    (is (run 1 [q]
          (fresh [z]
            (even-numbers ))))))