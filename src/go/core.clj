(ns go.core
  (:use go.graphics pattern-match))

;(declare go.graphics/displayBoard)
(defmacro defp [name args & body]
  `(defn ~name ~args
     (prof ~(keyword name) ~@body)))


;loc is [r c], color is an int, killed is listOf [r c]
(defstruct moveS :loc :color :killed)

(defstruct game :board :size :pastMoves :futureMoves)

;NOTE: returns nil without executing body if the space was not empty
(defmacro with-piece-set [[board r c color] & body]
  `(if (= (aget ~board ~r ~c) 0)
     (do (aset ~board ~r ~c ~color)
	 (let [result# (do ~@body)]
	   (aset ~board ~r ~c 0)
	   result#))
     nil))

(defn inBoard [r c size]
  (and (>= r 0) (>= c 0) (< r size) (< c size)))

(defn enemy? [board r c color]
  (let [p (aget board r c)]
    (not (or (= 0 p) (= p color)))))

;will return out of bounds positions
(defn adjacent [r c]
  (list [(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]))

(defn liberties
  ([game [r c]] (liberties r c))
  ([{:keys [size board]} r c]
     (if (inBoard r c size)
       (liberties (list [r c]) (aget board r c) #{} #{} #{})
       [0 #{}]))
  ([{:as game :keys [size board]}
    [[r c] & rcs] color libs seen pieces]
     (if r ;no more spaces to check
       (if (or (not (inBoard r c size)) (get seen [r c]) (enemy? board r c color))
	 (recur game rcs color libs seen pieces)
	 (if (= (aget board r c) 0)
	   (recur game rcs color (conj libs [r c]) (conj seen [r c]) pieces)
	   (recur game (concat (adjacent r c) rcs)
		  color libs (conj seen [r c]) (conj pieces [r c]))))
       [(count libs) pieces])))

(defn getKills [game r c color]
  (let [en (filter (fn [[ro co]] (and (inBoard game ro co) (enemy? game ro co color)))
		   (adjacent r c))
	kills (filter (fn [[l p]] (= l 0))
		      (map liberties en))]
    (if (> (count kills) 0)
      (apply clojure.set/union (map (fn [[l p]] p) kills))
      (let [[selfLibs _] (liberties r c)]
	(if (= selfLibs 0)
	  #{[r c]}
	  #{})))))
  
(defn getMove [[r c] color]
  (with-piece-set [board r c color]
    (let [kills (getKills r c color)]
      (if (get (set kills) [r c])
	nil
	(struct moveS [r c] color kills)))))

(defn makeMove [m]
  (let [{[r c] :loc :keys [color killed]} m]
    (dorun (map (fn [[ro co]] (aset board ro co 0)) killed))
    (aset board r c color)))

(defn undoMove [m]
  (let [{[r c] :loc :keys [color killed]} m]
    (dorun (map (fn [[ro co]] (aset board ro co (if (= color 1) 2 1))) killed))
    (aset board r c 0)))

(defn newMove [m]
  (def pastMoves (cons m pastMoves))
  (def futureMoves '())
  (makeMove m))

(defn undoLastMove []
  (let [move (first pastMoves)]
    (undoMove move)
    (def pastMoves (rest pastMoves))
    (def futureMoves (cons move futureMoves))))

(defn redoNextMove []
  (let [move (first futureMoves)]
    (def pastMoves (cons move pastMoves))
    (def futureMoves (rest futureMoves))
    (makeMove move)))

(defn uRedoNextMove []
  (if (not (empty? futureMoves))
    (do (redoNextMove)
	(displayBoard board 19 19))))

(defn uUndoLastMove []
  (if (not (empty? pastMoves))
    (do (undoLastMove)
	(displayBoard board 19 19))))

(defn uF2Last []
  (while (not (empty? futureMoves))
    (redoNextMove))
  (displayBoard board 19 19))

(defn uB2First []
  (while (not (empty? pastMoves))
    (undoLastMove))
  (displayBoard board 19 19))


(defmacro with-temp-move [m & body]
  `(let [move# ~m]
     (makeMove move#)
     (let [result# (do ~@body)]
       (undoMove move#)
       result#)))


(defn clicked [r c]
  (let [move (getMove [r c] turn)]
    (if move
      (do (def turn (inc (mod turn 2)))
	  (newMove move)
	  (displayBoard board 19 19))
      (println "invalid move"))))
