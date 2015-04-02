(ns logic.core
  #_(:refer-clojure :exclude [== >= > <= < =])
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        clojure.core.logic.pldb
        #_clojure.core.logic.arithmetic))

(db-rel father x y)
(db-rel male x)
(db-rel mother x y)
(db-rel female x)

(def facts
  (db [father 'terach 'abraham]
      [father 'terach 'nachor]
      [father 'terach 'haran]
      [father 'abraham 'isaac]
      [father 'haran 'lot]
      [father 'haran 'milcah]
      [father 'haran 'yiscah]
      [mother 'sarah 'isaac]
      [male 'terach]
      [male 'abraham]
      [male 'nachor]
      [male 'haran]
      [male 'isaac]
      [male 'lot]
      [female 'sarah]
      [female 'milcah]
      [female 'yiscah]))

(with-db facts
  (run* [q] (male q)))

(defn parent [x y]
  (conde
    ((father x y))
    ((mother x y))))

(with-db facts
  (run* [q]
    (fresh [x y]
      (parent x y)
      (== q [x y]))))

(run* [q]
  (fresh [a b]
    (appendo a b [1 2 3])
    (== q a)))

; s is a list with a single element
(defn s [x y]
  (conso x [] y))

; natural_number(0). natural_number(s(X)) :- natural_number(X).
(defn natural-number [x]
  (conde
    [(== x 0)]
    [(fresh [p]
       (s p x)
       (natural-number p))]))

; natural_number(s(s(0))). should be yes
(run 1 [q]
  (natural-number '((0)))
  (== q true)) ;for a 'nicer' yes.
; => (true)

; natural_number(s(s(s(1)))). should be no
(run 1 [q]
  (natural-number '(((1))))
  (== q true))
; => ()

; plus(0, X, X). plus(s(X), Y, s(Z)) :- plus(X, Y, Z).
(defn plus [x y z]
  (conde
    [(fresh [a]
       (== [0 a a] [x y z]))]
    [(fresh [x1 z1]
       (s x1 x)
       (s z1 z)
       (plus x1 y z1))]))

; plus(s(s(0)), s(0), X). should be X = s(s(s(0)))
(run 1 [q]
  (fresh [z]
    (plus '((0)) '(0) z)
    (== q z)))
; => ((((0)))), it's a (((0))) inside resulting list!
