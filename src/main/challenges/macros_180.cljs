(ns challenges.macros-180)

(defmacro mouse-position [w]
  `[(Math/floor (/ ~js/mouseX ~w)) (Math/floor (/ ~js/mouseY ~w))])

(defmacro within-cols [i]
  `(and (pos? ~i) (< ~i cols)))

(defmacro within-rows [j]
  `(and (pos? ~j) (< ~j rows)))
