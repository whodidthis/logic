(ns logic.numbers
  (:refer-clojure :exclude [== max])
  (:use clojure.core.logic))

(defne natural-number [x]
  ([0])
  ([['s x1]]
    (natural-number x1)))

(defne even [x]
  ([0])
  ([['s ['s x1]]]
    (even x1)))

(defne odd [x]
  ([['s 0]])
  ([['s ['s x1]]]
    (odd x1)))

(defne less [x y]
  ([0 ['s y1]])
  ([['s x1] ['s y1]]
    (less x1 y1)))

(defne less-or-equal [x y]
  ([0 0])
  ([0 ['s y1]])
  ([['s x1] ['s y1]]
    (less-or-equal x1 y1)))

(defn greater [x y]
  (less y x))

(defn greater-or-equal [x y]
  (less-or-equal y x))

(defne plus [x y z]
  ([0 y y])
  ([['s x1] y ['s z1]]
    (plus x1 y z1)))

(defn minus [x y z]
  (plus z y x))

(defne times [x y z]
  ([0 y 0])
  ([['s x1] y z]
    (fresh [z1]
      (times x1 y z1)
      (plus z1 y z))))

(defne division-4 [x z a r]
  ([x z r r]
    (less x z))
  ([x z a r]
    (greater-or-equal x z)
    (fresh [x-z]
      (minus x z x-z)
      (division-4 x-z z ['s a] r))))

(defn division [x z r]
  (greater z 0)
  (division-4 x z 0 r))

(defne length [x n]
  ([[] 0])
  ([[x1] ['s 0]])
  ([[x1 . xs] ['s n1]]
    (length xs n1)))

(defne merger [x y r]
  ([[] y y])
  ([x [] x])
  ([[x1 . xs] [y1 . ys] [x1 . rs]]
    (less-or-equal x1 y1)
    (merger xs y rs))
  ([[x1 . xs] [y1 . ys] [y1 . rs]]
    (greater x1 y1)
    (merger x ys rs)))

(defne merge-sort [x r]
  ([[] []])
  ([[x1] [x1]])
  ([[x1 . xs] r]
    (fresh [l h xh1 xh2 y1 y2]
      (length x l)
      (division l ['s ['s 0]] h)
      (appendo xh1 xh2 x)
      (length xh1 h)
      (merge-sort xh1 y1)
      (merge-sort xh2 y2)
      (merger y1 y2 r))))

(defne max* [x y z]
  ([x y x]
    (less-or-equal y x))
  ([x y y]
    (less x y)))

(defne max [x m]
  ([[x1] x1])
  ([[x1 . xs] m]
    (fresh [m1]
      (max* m1 x1 m)
      (max xs m1))))

(defne factorial [x r]
  ([0 ['s 0]])
  ([['s p] f]
    (fresh [f1]
      (factorial p f1)
      (times ['s p] f1 f))))
