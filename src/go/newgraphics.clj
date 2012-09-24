(ns go.newgraphics
  (:use go.protocols
	[go.goboard :only (new-go-board)]
	seesaw.core))

(import 'java.io.File)
(import 'javax.swing.ImageIcon)
(import 'java.awt.event.MouseListener)

(declare click)
(declare update-all)

(defmacro on-mouse-click [component event & body]
  `(. ~component addMouseListener
      (proxy [MouseListener] []
	(mousePressed [e#])
	(mouseClicked [~event] ~@body)
	(mouseEntered [e#])
	(mouseExited [e#])
	(mouseReleased [e#]))))

(defmacro label-icon [pathstr & [on-click]]
  (if on-click
    `(let [l# (label :icon (icon (File. ~pathstr)))]
       (on-mouse-click l# ~'e ~on-click)
       l#)
    `(label :icon (icon (File. ~pathstr)))))

(def board (new-go-board))

(def labels (vec (for [r (range 19) c (range 19)]
		   (label-icon "images/blank_space.png" (click r c)))))

(def gp (grid-panel :items (flatten labels) :columns 19 :hgap 0 :vgap 0))

(def button-panel (horizontal-panel :items [(label-icon "images/double_left_arrow.png" (do (.rewind board) (update-all)))
					    (label-icon "images/left_arrow.png" (do (.undo-move board) (update-all)))
					    (label-icon "images/right_arrow.png" (do (.redo-move board) (update-all)))
					    (label-icon "images/double_right_arrow.png" (do (.fast-forward board) (update-all)))]))

(def f (frame :title "Game Board"
	      :content (vertical-panel :items [gp button-panel])))

(def color-map (let [m (.image-color-map board)
		     ks (keys m)]
		 (zipmap ks (map #(icon (File. (m %))) ks))))

(defn update [[r c]]
  (config! (nth labels (+ c (* r 19))) :icon (color-map (.color? board [r c]))))

(defn update-all []
  (time (doall (map update (for [r (range 19) c (range 19)] [r c])))))

(defn display []
  (update-all)
  (-> f pack! show!))

(defn click [r c]
  (.clicked board r c)
  (display))