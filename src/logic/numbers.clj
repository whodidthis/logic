(ns logic.numbers
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

(defne natural-number [x]
  ([0])
  ([['s x1]]
   (natural-number x1)))

(run 1 [q]
  (natural-number 0))
; => (_0)
(run 1 [q]
  (natural-number ['s 0]))
; => (_0)

(defne plus [x y z]
  ([0 y y])
  ([['s x1] y ['s z1]]
   (plus x1 y z1)))

(run 1 [q]
  (fresh [z]
    (plus 0 ['s 0] z)
    (== q z)))
; => ([s 0])

(run 1 [q]
  (fresh [y]
    (plus ['s 0] y ['s ['s ['s ['s 0]]]])
    (== q y)))
; => ([s [s [s 0]]])
