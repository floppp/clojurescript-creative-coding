(ns challenges.challenge-001-starlight
  (:require [p5]))

;; El rendimiento obtenido con objetos de js y mutándolos
;; con `set!` es muchísimo mejor que usando diccionarios
;; de clojurescript y usando `assoc`. Aunque queda el
;; código bastante peor, lo dejo tal que así.
(def state {:sizes {:w js/window.innerWidth :h js/window.innerHeight}})
(def width 800) ;;(-> state :sizes :w)
(def height 500) ;;(-> state :sizes :h)
(def ^:dynamic *speed* 10.0)

(defn new-star
  "Creamos una nueva estrella con coordenadas aleatorias."
  [& {:keys [w] :or {w (rand-int width)}}]
  #js {:x (- width (rand-int (* 2 width)))
       :y (- height (rand-int (* 2 height)))
       :z w
       :pz w})

(defn create-star []
  (new-star width))

(defn update-star
  [speed ^js star]
  (let [new-z (- (.-z star) speed)]
    (if (< new-z 1)
      (new-star)
      (do (set! (.-pz star) (.-z star))
          (set! (.-z star) new-z)
          star))))

(defn show-star
  [^js star] ;; para evitar `cannot infer` le decimos que es un objeto de js
  (js/fill 255)
  (js/noStroke)
  ;; Estas divisiones para poner en posición respecto del fondo
  ;; las coordenadas `x` e `y` de la estrella.
  (let [sx (js/map (/ (.-x star) (.-z star)) 0 1 0 width)
        sy (js/map (/ (.-y star) (.-z star)) 0 1 0 height)
        px (js/map (/ (.-x star) (.-pz star)) 0 1 0 width)
        py (js/map (/ (.-y star) (.-pz star)) 0 1 0 height)]
    (js/stroke 255)
    (js/strokeWeight (js/map (.-z star) 0 width 4 1))
    (js/line px py sx sy)))

(def stars (volatile!
            (mapv (fn [_] (create-star))
                  (range 5000))))

(defn setup []
  (let [canvas (js/createCanvas width height)]
    (.parent canvas "p5jsparent")))

(defn draw []
  (js/background 0)
  ;; movemos todo al centro
  (js/translate (/ width 2) (/ height 2))
  (binding [*speed* (js/map js/mouseX 0 width 0 50)]
    (vswap! stars
            (fn [arr]
              (mapv
               (fn [star] ;; así para evitarme una nueva pasada
                 ;; Efectos en ambas llamadas, tanto a update como a show (dibujar solamente).
                 (let [new-star (update-star *speed* star)]
                   (show-star new-star)
                   new-star))
               arr)))))
