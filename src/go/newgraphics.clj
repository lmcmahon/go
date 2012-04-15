(ns go.newgraphics
  (:use go.protocols
	[go.goboard :only (new-go-board)]
	seesaw.core))

(import 'java.io.File)
(import 'javax.swing.ImageIcon)
(import 'java.awt.event.MouseListener)

(defmacro on-mouse-click [component event & body]
  `(. ~component addMouseListener
      (proxy [MouseListener] []
	(mousePressed [e#])
	(mouseClicked [~event] ~@body)
	(mouseEntered [e#])
	(mouseExited [e#])
	(mouseReleased [e#]))))

(def i (icon (File. "images/black_stone.png")))

(def labels (vec (for [r (range 19) c (range 19)]
		   (let [l (label :icon (icon (File. "images/blank_space.png")))]
		     (on-mouse-click l e (click r c))
		     l))))

(def f (frame :title "Title"))

(def gp (grid-panel :items (flatten labels) :columns 19 :hgap 0 :vgap 0))

(def board (new-go-board))

(def color-map (let [m (.image-color-map board)
		     ks (keys m)]
		 (zipmap ks (map #(icon (File. (m %))) ks))))

(defn update [[r c]]
  (config! (nth labels (+ c (* r 19))) :icon (color-map (.color? board [r c]))))

(defn update-all []
  (doall (map update (for [r (range 19) c (range 19)] [r c]))))

(defn display []
  (update-all)
  (-> f (config! :content gp) pack! show!))

(defn click [r c]
  (.clicked board r c)
  (display))