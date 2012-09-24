(ns go.graphics
  (:use pattern-match go.core clojure.contrib.profile))

(import '(javax.swing JFrame JPanel JButton JLabel JTextField GroupLayout ImageIcon JScrollPane ScrollPaneLayout))
(import '(java.awt.event ActionListener MouseListener WindowAdapter))
(import 'javax.imageio.ImageIO)
(import 'java.io.File)

(defmacro defm [name & forms]
  `(defn ~name [ & args#]
     (match args# ~@forms)))


(defmacro on-action [component event & body]
  `(. ~component addActionListener
      (proxy [ActionListener] []
	(actionPerformed [~event] ~@body))))

(defmacro on-mouse-click [component event & body]
  `(. ~component addMouseListener
      (proxy [MouseListener] []
	(mousePressed [e#])
	(mouseClicked [~event] ~@body)
	(mouseEntered [e#])
	(mouseExited [e#])
	(mouseReleased [e#]))))


;globals
(def emptyImage (ImageIcon. (ImageIO/read (File. "/home/liam/Documents/code/clojure/go/images/blank_space.png"))))
(def blackImage (ImageIcon. (ImageIO/read (File. "/home/liam/Documents/code/clojure/go/images/black_stone.png"))))
(def whiteImage (ImageIcon. (ImageIO/read (File. "/home/liam/Documents/code/clojure/go/images/white_stone.png"))))
(def #^"[[LObject" uiBoard (make-array Object 19 19))


(defn cycleImage [label]
  (if (= (.getIcon label) emptyImage)
    (.setIcon label blackImage)
    (if (= (.getIcon label) blackImage)
      (.setIcon label whiteImage)
      (.setIcon label emptyImage))))

;(defm setImageColor
;  [label 0] (.setIcon label emptyImage)
;  [label 1] (.setIcon label blackImage)
					;  [label _] (.setIcon label whiteImage))
(defn setImageColor [^JLabel label ^int color]
  (if (= color 0)
    (.setIcon label emptyImage)
    (if (= color 1)
      (.setIcon label blackImage)
      (.setIcon label whiteImage))))

(defn intoParallelGroup)
(defn intoSequentialGroup
  ([layout vec] (intoSequentialGroup layout vec (.createSequentialGroup layout)))
  ([layout vec group] (do (doseq [x vec]
			    (if (coll? x)
			      (.addGroup group (intoParallelGroup layout x (.createParallelGroup layout)))
			      (.addComponent group x)))
			  group)))

(defn intoParallelGroup
  ([layout vec] (intoParallelGroup layout vec (.createParallelGroup layout)))
  ([layout vec group] (do (doseq [x vec]
			    (if (coll? x)
			      (.addGroup group (intoSequentialGroup layout x (.createSequentialGroup layout)))
			      (.addComponent group x)))
			  group)))
  
  
(defn addStuffToLayout [layout]
  (let [hVec (map seq (seq uiBoard))
	vVec (apply map list hVec)
	B2First (JLabel. (ImageIcon. "/home/liam/Documents/code/clojure/go/images/double_left_arrow.png"))
	F2Last (JLabel. (ImageIcon. "/home/liam/Documents/code/clojure/go/images/double_right_arrow.png"))
	BNext (JLabel. (ImageIcon. "/home/liam/Documents/code/clojure/go/images/right_arrow.png"))
	BPrev (JLabel. (ImageIcon. "/home/liam/Documents/code/clojure/go/images/left_arrow.png"))
	BVec [B2First BPrev BNext F2Last]]
    (on-mouse-click BNext e (uRedoNextMove))
    (on-mouse-click BPrev e (uUndoLastMove))
    (on-mouse-click F2Last e (uF2Last))
    (on-mouse-click B2First e (uB2First))
    (doto layout
      (.setHorizontalGroup (intoParallelGroup layout [hVec BVec]))
      (.setVerticalGroup (intoSequentialGroup layout (concat vVec [BVec]))))))

(defn displayBoard [#^"[[I" intArray rows cols]
  (dorun (for [r (range rows) c (range cols)]
	   (setImageColor (aget uiBoard c r) (aget intArray r c)))))

(defn initUI []
  (dorun (for [x (range 19) y (range 19)]
	   (let [temp (JLabel. emptyImage)]
	     (aset uiBoard x y temp)
	     (on-mouse-click temp e (clicked y x)))))
  (let [frame (JFrame. "Go")
	panel (JPanel.)
	layout (GroupLayout. panel)]
    (let [scp (JScrollPane. panel)]
      (.setLayout scp (ScrollPaneLayout.))
      (.setContentPane frame scp))
    (.setLayout panel layout)
    (addStuffToLayout layout)
    (doto frame
      (.pack)
      (.setVisible true)
      (.addWindowListener (proxy [WindowAdapter] []
			    (windowClosing [e] (onClose)))))))
