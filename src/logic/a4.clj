(ns logic.a4
  (:refer-clojure :exclude [== max])
  (:use clojure.core.logic
        logic.numbers))

(def trees ['tree [['! ['four '- 'two]] ['three '- 'two]]
            ['tree ['three 'two 'four]
             'void
             'void]
            ['tree [['three '* 'two] ['three '+ 'two]]
             'void
             'void]])

(defne ar-stm [ar sem]
  (['one ['s 0]])
  (['two ['s ['s 0]]])
  (['three ['s ['s ['s 0]]]])
  (['four ['s ['s ['s ['s 0]]]]])
  (['five ['s ['s ['s ['s ['s 0]]]]]])
  ([[x1 '+ x2] sem]
    (fresh [sem1 sem2]
      (ar-stm x1 sem1)
      (ar-stm x2 sem2)
      (plus sem1 sem2 sem)))
  ([[x1 '- x2] sem1]
    (fresh [sem1 sem2]
      (ar-stm x1 sem1)
      (ar-stm x2 sem2)
      (minus sem1 sem2 sem)))
  ([[x1 '* x2] sem]
    (fresh [sem1 sem2]
      (ar-stm x1 sem1)
      (ar-stm x2 sem2)
      (times sem1 sem2 sem)))
  ([[x1 '/ x2] sem]
    (!= x2 0)
    (fresh [sem1 sem2]
      (ar-stm x1 sem1)
      (ar-stm x2 sem2)
      (division sem1 sem2 sem)))
  ([['! x] sem]
    (fresh [sem1]
      (ar-stm x sem1)
      (factorial sem1 sem))))

(run 1 [q]
  (fresh [z]
    (ar-stm [#_['five '* 'five] #_'+ ['! 'four]] z)
    (== q z)))
