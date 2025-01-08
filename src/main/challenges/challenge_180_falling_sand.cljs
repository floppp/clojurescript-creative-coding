;; 180 - Falling Sand
(ns challenges.challenge-180-falling-sand
  (:require [p5]))

;; Definición de variables
;; (def state {:sizes {:w js/window.innerWidth :h js/window.innerHeight}})
(def state {:sizes {:w 1000 :h 800}})
(def width (-> state :sizes :w))
(def height (-> state :sizes :h))
(def w 5) ;; tamaño de cada celda;
(def cols (/ width w))
(def rows (/ height w))
(def rcols (range cols))
(def rrows (range rows))
(def grid (volatile! #js []))
(def matrix 6)
(def hue-value (volatile! 50))

(defn make-2darray
  "Creación de matriz con función opcional para rellenarla.
  Se le pasan dimensiones (`nrows` y `ncols`) y opcionalmente
  un callback para calcular el valor de sus elementos a partir
  del índice de cada uno de ellos `(fn [col_idx row_idx))`.
  Si no pasamos función, el fallback es rellenar con ceros."
  [ncols nrows & {:keys [f] :or {f (constantly 0)}}]
  #_(vec (repeat nrows (vec (repeat ncols 0))))
  (mapv (fn [i] (mapv #(f i %) (range nrows))) (range ncols)))

(defn make-flat-2darray
  [ncols nrows]
  (vec (repeat (* nrows ncols) 0)))

(defn setup
  []
  (let [canvas (js/createCanvas width height)]
    (.parent canvas "p5jsparent")
    (js/loadPixels))
  (js/colorMode js/HSB 360 255 255)
  (let [arr (make-2darray cols rows)]
    (println (count arr) (count (first arr))))
  (vreset! grid (clj->js (make-2darray cols rows))))

(defn mouse-pressed []
  (let [[mouse-col mouse-row] [(Math/floor (/ js/mouseX w)) (Math/floor (/ js/mouseY w))]
        extent (Math/floor (/ matrix 2))]
    ;; Pintamos de forma aletoria nuevos granos de arena alrededor del punto donde clicamos.
    (doseq [i (range (- extent) (inc extent))
            j (range (- extent) (inc extent))]
      (when (zero? (rand-int 2))
        (let [col (+ mouse-col i)
              row (+ mouse-row j)]
          (when (and (> col -1) (< col cols) (> row 1) (< row rows))
            (aset @grid col row @hue-value)))))
    (vreset! hue-value (+ 5 @hue-value))
    (when (> @hue-value 359)
      (vreset! hue-value 1))))

(comment
  (range 0 100 5)

  )

(defn draw []
  (js/background 0)
  (js/noStroke)
  (doseq [i (range cols)
          j (range rows)]
    ;; (js/stroke 255)
    ;; Podríamos ahorranos esto si el valor del grid base fuese negro en el
    ;; espacio de color que estamos usando. Pero no tiene mayor importancia
    ;; y lo simplifica mucho.
    (when (> (aget @grid i j) 0)
      (js/fill (aget @grid i j) 255 255)
      (js/square (* i w) (* j w) w)))

  ;; Para calcular el movimiento, volvemos a repasar el grid, y
  ;; si teníamos en una celda un 1, en este nuevo grid el 1 se
  ;; habrá movido hacia abajo.
  (let [next-grid (clj->js (make-2darray cols rows))]
    (doseq [i rcols j rrows]
      (let [state (aget @grid i j)]
        (when (> state 0)
          (let [below (aget @grid i (inc j))
                dir (if (zero? (rand-int 2)) 1 -1)
                next-i (+ i dir)
                prev-i (- i dir)
                next-j (inc j)]
            (cond
              (zero? below) (aset next-grid i next-j state)

              ;; Comprobamos posición bajo derecha.
              (and (pos? next-i) (< next-i cols) (zero? (aget @grid next-i next-j)))
              (aset next-grid next-i next-j state)

              ;; Comprobamos posición bajo izquierda.
              (and  (pos? prev-i) (< prev-i cols) (zero? (aget @grid prev-i next-j)))
              (aset next-grid prev-i next-j state)

              :else (aset next-grid i j state))))))
    (vreset! grid next-grid)))
