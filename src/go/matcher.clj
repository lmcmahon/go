(ns go.matcher
  (:use pattern-match))

(defmacro defm [name & forms]
  `(defn ~name [ & args#]
     (match args# ~@forms)))

(defmacro defd [name args doc & forms]
  `(defn ~name ~doc ~args
     ~@forms))