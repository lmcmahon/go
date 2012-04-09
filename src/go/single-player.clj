(ns go.sp
  (:use go.core go.graphics pattern-match))

(def board (make-array Integer/TYPE 19 19))
(def pastMoves '())
(def futureMoves '())
(def turn 1)

;;FIX THIS
(defn onClose [] (println "close"))
