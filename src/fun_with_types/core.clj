(ns fun-with-types.core
  (:require [clojure.core.typed :refer
             [ann Seqable NonEmptyColl Coll AnyInteger check-ns cf fn>
              def-alias]]
            [clojure.pprint :refer [pp pprint]])
  (:import [clojure.lang ISeq IPersistentMap]))

(ann ^:no-check clojure.core/mod [AnyInteger AnyInteger -> AnyInteger])

(ann div-by [AnyInteger AnyInteger -> Boolean])
(defn div-by [x y]
  (== (mod y x) 0))

(ann div-by-3-or-5 [AnyInteger -> Boolean])
(defn div-by-3-or-5 [x]
  (or (div-by 3 x) (div-by 5 x)))

(ann euler1 [AnyInteger -> AnyInteger])
(defn euler1 [n]
  (reduce + (filter div-by-3-or-5 (range n))))

(ann my-inc [Number -> Number])
(defn my-inc [i] (+ i 1))

(ann my-identity (All [x] [x -> x]))
(defn my-identity [x] x)

(ann my-map (All [x y] [(Fn [x -> y]) (Coll x) -> (Coll y)]))
(defn my-map [f c]
  (if (seq c)
    (cons (f (first c))
          (my-map f (rest c)))
    '()))

(ann a (Value 3))
(def a 3)

(ann foldl (All [a c] [(Fn [a c -> a]) a (Coll c) -> a]))
(defn foldl [f acc coll]
  (if (seq coll)
    (recur f (f acc (first coll)) (rest coll))
    acc))

(def-alias Azimut (U (Value "N")
                     (Value "S")
                     (Value "E")
                     (Value "W")))
