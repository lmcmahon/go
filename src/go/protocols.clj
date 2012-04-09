(ns go.protocols)

(defstruct moveS :loc :color :killed)

(defprotocol Iboard
  (clicked [this r c] "The ui will call this when the user clicks")
  (undo-move [this] "This should roll back a move, with the option of redoing it")
  (redo-move [this] "Redoes a rolled back move, if there is one")
  (fast-forward [this] "Redoes all the moves on the stack")
  (rewind [this] "Undoes all the moves on the stack")
  (color? [this loc] "Returns the color of the given space")
  (image-color-map [this] "Returns a map from color to the image to be used by the UI"))
