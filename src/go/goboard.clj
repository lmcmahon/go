(ns go.goboard
  (:use go.protocols go.matcher pattern-match))

;;; A loc is a vector of a row column pair [r c]
;;; A group is a hashmap of locs that form a group of stones that share liberties 

(declare squash-groups)
(declare group)
(def empty 0)
(def black 1)
(def white 2)

(defn inBoard [[r c]]
  (and (>= r 0) (>= c 0) (< r 19) (< c 19)))

(defn enemy? [c1 c2]
  (= (+ c1 c2) (+ black white)))

;will return out of bounds positions
(defn adjacent [[r c]]
  (list [(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]))

(defn neighbors [loc] (filter inBoard (adjacent loc)))

(defn friends [board loc color]
  (filter #(= color (.color? board %))
	  (neighbors loc)))

(defn enemies [board loc color]
  (filter #(enemy? color (.color? board %)) (neighbors loc)))

(defn imediate-libs [board loc]
  (filter (fn [loc] (= (.color? board loc) empty))
	  (neighbors loc)))

(defn group-libs [board group]
  (distinct (mapcat #(imediate-libs board %) group)))

(defd need-clearing [board locs]
  "locs should be a list of stones that might be part of a group that needs to be cleared.
 Returns a list of stones that need to be cleared."
  (->> locs (map (group board)) (apply squash-groups) (filter #(not (seq (group-libs board %)))) (mapcat seq)))

;(group board loc) note- can be called to find an 'empty' group
;(group board color need-to-check group)
(defn group "Takes a bourd and a loc, and returns the group that the loc is
a part of.  If called with only the board, returns a function that takes a loc
and returns the group its a part of (currying)."
  ([board] #(group board %))
  ([board loc] (group board (.color? board loc) (list loc) #{}))
  ([board color [loc & check] grp]
     (if (not loc)
       grp
       (let [frds (->> (friends board loc color) (filter #(not (grp %))))]
	 (recur board color (concat frds check) (into (conj grp loc) frds))))))

(defd same-group [g1 g2]
  "true if the two groups are the same"
  (if (g1 (first (seq g2)))
    true
    false))

(defd squash-groups [& groups]
  "takes multiple groups and returns a list of distinct groups"
  (reduce (fn [gs g2]
	    (if (->> gs (filter #(same-group g2 %)) seq)
	      gs
	      (cons g2 gs)))
	  '() groups))

(defmacro with-set [board loc color & body]
  `(let [old# (.color? ~board ~loc)]
     (.setc ~board ~loc ~color)
     (let [ret# (do ~@body)]
       (.setc ~board ~loc old#)
       ret#)))

(defn loc-killed [board loc color]
  (need-clearing board (enemies board loc color)))

(defn getMove [board loc color]
  (if (not (= (.color? board loc) empty))
    nil
    (with-set board loc color
      (cond-let [killed (seq (loc-killed board loc color))] (struct moveS loc color killed)
		[_ (->> loc (group board) (group-libs board) seq)] (struct moveS loc color nil)))))

(defn other-color [c]
  (if (= c black) white black))

(defprotocol Igoboard
  (setc [this loc color] "Sets the point [r c] to color")
  (get-move [this loc color] "Gets the moveS for this move, or nil if it is illegal")
  (make-move [this move] "Makes a move (does not change the move stack/turn)")
  (revert-move [this move] "Reverts a move, without changeing the stack/turn")
  (switch-turn [this] "Switches turns"))

;;; evaluates body with move bound to the first value of stack
;;; after destructively poping the stack.  Returns nil and does
;;; nothing if the stack was empty
(defmacro with-pull-off [[move stack] & body]
  `(when (seq (deref ~stack))
     (let [~move (first (deref ~stack))]
       (swap! ~stack rest)
       ~@body)))

;;; pushes a value 'val' onto an atom 'a'
(defn push! [a val]
  (swap! a #(cons %2 %) val))

;;; transfers a move from the atom 'from' to the atom 'to' and calls 'func' on it,
;;; then switches the current turn (to be used inside go-board)
;;; returns nil and does nothing if the from stack is empty
(defmacro transfer-move [from to func]
  `(with-pull-off [move# ~from]
     (~func ~'this move#)
     (push! ~to move#)
     (.switch-turn ~'this)))

(defrecord go-board [^ints board current-turn past-moves future-moves]
  Iboard
  (clicked [this r c]
	   (when-let [move (.get-move this [r c] @current-turn)]
	     (.make-move this move)
	     (swap! future-moves (fn [_] nil))
	     (push! past-moves move)
	     (.switch-turn this)))
  (undo-move [this] (transfer-move past-moves future-moves .revert-move))
  (redo-move [this] (transfer-move future-moves past-moves .make-move))
  (fast-forward [this] (while (seq @future-moves)
			 (.redo-move this)))
  (rewind [this] (while (seq @past-moves)
		   (.undo-move this)))
  (image-color-map [this] {empty "images/blank_space.png"
			   black "images/black_stone.png"
			   white "images/white_stone.png"})
  (color? [this [r c]] (aget board (+ r (* c 19))))
  
  Igoboard
  (setc [this [r c] color]
	(aset board (+ r (* c 19)) color))
  (get-move [this loc color]
	    (getMove this loc color))
  (make-move [this {:keys (loc color killed)}]
	     (dorun (map #(.setc this % empty) killed))
	     (.setc this loc color))
  (switch-turn [this]
	       (swap! current-turn other-color))
  (revert-move [this {:keys (loc color killed)}]
	       (dorun (map #(.setc this % (other-color color)) killed))
	       (.setc this loc empty)))

;;; constructor
(defn new-go-board []
  (go-board. (int-array (* 19 19)) (atom black) (atom nil) (atom nil)))

(defn printb [board]
  (dotimes [r 19]
    (dotimes [c 19]
      (print (.color? board [ r c]) " "))
    (println "")))