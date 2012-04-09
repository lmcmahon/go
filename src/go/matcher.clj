(ns go.matcher
  (:use pattern-match))

(defmacro defm [name & forms]
  `(defn ~name [ & args#]
     (match args# ~@forms)))
