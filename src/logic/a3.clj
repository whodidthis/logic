(ns logic.a3
  (:refer-clojure :exclude [== max])
  (:use clojure.core.logic
        logic.numbers))

(def tree1 ['tree [['a ['c 'c]] 'c]
            ['tree [['b 'c]]
             'void
             'void]
            ['tree [['a ['b 'c]]]
             'void
             'void]])

(defne stm [x]
  (['c])
  ([['a x1]]
    (stm x1))
  ([['b x1]]
    (stm x1))
  ([['c x1]]
    (stm x1)))

(defne cees [x n]
  (['c ['s 0]])
  ([]))

(run 1 [q]
  (stm ['a ['b 'c]]))

(defne transftree [bt1 bt2]
  (['void 'void])
  ([['tree x l r] ['tree ]]))