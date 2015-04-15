(ns logic.a2
  (:refer-clojure :exclude [== max])
  (:use clojure.core.logic
        logic.numbers))

(defne neg-integer [x]
  ([['m 0]])
  ([['s x1]]
    (neg-integer x1)))

(run 1 [q]
  (fresh [z]
    (neg-integer ['m 0])))

(defne abs [x a]
  ([0 0])
  ([['s x1] ['s x1]])
  ([['m x1] ['s a1]]
    (abs x1 a1)))

(defne absolutes [x a]
  ([[] []])
  ([[x1 . xs] [a1 . as]]
    (abs x1 a1)
    (absolutes xs as)))

(run 1 [q]
  (fresh [z r]
    (absolutes [['m ['m ['m ['m 0]]]] 0 ['s ['s 0]] ['m 0]] z)
    (max z r)
    (== q r)))

(run 1 [q]
  (fresh [z m]
    (absolutes [] z)
    (max z m)
    (== q m)))

(def tree1 ['tree [['m 0] ['s ['s 0]]]
            ['tree [['m ['m ['m 0]]] ['s ['s 0]] ['m ['m 0]]]
             'void
             'void]
            ['tree [['s ['s 0]] ['s 0] ['m 0]]
             'void
             'void]])

(defne choose-greatest [bt1 bt2]
  (['void 'void])
  ([['tree x l r] ['tree m l' r']]
    (fresh [a]
      (absolutes x a)
      (max a m)
      (choose-greatest l l')
      (choose-greatest r r'))))

(run 1 [q]
  (fresh [z]
    (choose-greatest tree1 z)
    (== q z)))