(ns logic.a1
  (:refer-clojure :exclude [== max])
  (:use clojure.core.logic
        logic.numbers))

(run 1 [q]
  (fresh [z]
    (less-or-equal 0 ['s 0])))

(run 1 [q]
  (fresh [z]
    (merger [0] [0] z)
    (== q z)))

(run 1 [q]
  (fresh [z]
    (merge-sort [['s ['s 0]] 0 ['s 0]] z)
    (== q z)))

(defne even-numbers [x r]
  ([[] []])
  ([[x1 . xs] r]
    (even x1)
    (fresh [r1]
      (appendo r1 [x1] r)
      (even-numbers xs r1))))

(defne even-sums [x s]
  ([[] 0])
  ([[x1 . xs] s]
    (even x1)
    (fresh [z]
      (plus z x1 s)
      (even-sums xs z)))
  ([[x1 . xs] s]
    (odd x1) ;how to \+(even x1)
    (even-sums xs s)))

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

(defne pre-order [tree xs]
  (['void ()])
  ([['tree x l r] xs]
    (fresh [pre-l pre-r y]
      (pre-order l pre-l)
      (pre-order r pre-r)
      (conso x pre-l y)
      (appendo y pre-r xs))))

(run 1 [q]
  (fresh [z]
    (pre-order 'void z)
    (== z q)))
; => (())
(run 1 [q]
  (fresh [z]
    (pre-order ['tree ['s 0] 'void 'void] z)
    (== z q)))
; => ((['s 0]))
(run 1 [q]
  (fresh [z]
    (pre-order tree1 z)
    (== q z)))
; => (([s [s [s 0]]] [s [s 0]] [s 0])))

(defne tree-sums [trees sums]
  ([[] []])
  ([[t . ts] sums]
    (fresh [v s sums1]
      (pre-order t v)
      (even-sums v s)
      (appendo sums1 [s] sums)
      (tree-sums ts sums1))))

(run 1 [q]
  (fresh [z]
    (tree-sums [tree1 tree2 tree3] z)
    (== q z)))
; => (([s [s [s [s [s [s 0]]]]]] [s [s [s [s 0]]]] [s [s 0]])))

(defn greatest-even-sum [trees m]
  (fresh [sums]
    (tree-sums trees sums)
    (lmax sums m)))

(run 1 [q]
  (fresh [z]
    (greatest-even-sum [tree1 tree2 tree3] z)
    (== q z)))
; => ([s [s [s [s [s [s 0]]]]]]))

(defne appendo1 [x y z]
  ([[] y y])
  ([[x1 . xs] y z]
    (fresh [zs]
      (conso x1 zs z)
      (appendo1 xs y zs))))
(run 1 [q]
  (fresh [z]
    (appendo1 z [4 5] [1 2 3 4 5])
    (== q z)))

(run 1 [q]
  (fresh [z]
    (appendo1 [1 2 3] z [1 2 3 4 5])
    (== q z)))

(defne reverso [x y]
  ([[] []])
  ([[x1 . xs] y]
    (fresh [z]
      (appendo z [x1] y)
      (reverso xs z))))

(run 1 [q]
  (fresh [z]
    (reverso [1 2 3] z)
    (== q z)))

(run 1 [q]
  (fresh [z]
    (reverso z [4 2 3])
    (== q z)))

(run 1 [q]
  (fresh [z]
    (even-sums [['s 0] ['s ['s 0]] 0 ['s ['s 0]]] z)
    (== q z)))
