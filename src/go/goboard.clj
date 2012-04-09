(ns go.goboard
  (:use go.protocols go.matcher pattern-match))

(def empty 0)
(def black 1)
(def white 2)

(defn inBoard [[r c]]
  (and (>= r 0) (>= c 0) (< r 19) (< c 19)))

(defn enemy [c1 c2]
  (= (+ c1 c2) (+ black white)))

;will return out of bounds positions
(defn adjacent [[r c]]
  (list [(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]))

(defn neighbors [loc] (filter inBoard (adjacent loc)))

(defn friends [board loc color]
  (filter #(= color (.color? board %))
	  (neighbors loc)))

(defn enemies [board loc color]
  (filter #(enemy color (.color? board %)) (neighbors loc)))

(defn imediate-libs [board loc]
  (filter (fn [loc] (= (.color? board loc) empty))
	  (neighbors loc)))

;(group board loc) note- can be called to find an 'empty' group
;(group board color need-to-check group)
(defn group ([board loc] (group board (.color? board loc) (list loc) #{}))
  ([board color [loc & check] grp]
     (if (not loc)
       grp
       (let [frds (->> (friends board loc color) (filter #(not (grp %))))]
	 (recur board color (concat frds check) (into (conj grp loc) frds))))))

(defn same-group [g1 g2]
  (if (g1 (first (seq g2)))
    true
    false))

(defn squash-groups [& groups]
  (reduce (fn [gs g2]
	    (if (->> gs (filter #(same-group g2 %)) seq)
	      gs
	      (cons g2 gs)))
	  '() groups))

(defprotocol Igoboard
  (setc [this loc color] "Sets the point [r c] to color")
;  (get-move [this r c color] "Gets the moveS for this move, or nil if it is illegal")
  )

(defrecord go-board [^ints board current-turn past-moves future-moves]
  Iboard
  (clicked [this r c] nil)
  (undo-move [this] nil)
  (redo-move [this] nil)
  (fast-forward [this] nil)
  (rewind [this] nil)
  (image-color-map [this] nil)
  (color? [this [r c]] (aget board (+ r (* c 19))))
  
  Igoboard
  (setc [this [r c] color] (aset board (+ r (* c 19)) color)))

(defn new-go-board []
  (go-board. (int-array (* 19 19)) (atom empty) (atom nil) (atom nil)))

(defn printb [board]
  (dotimes [r 19]
    (dotimes [c 19]
      (print (.color? board [ r c]) " "))
    (println "")))