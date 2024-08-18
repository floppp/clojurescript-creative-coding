;; Primer proyecto
(ns ten-minute-physics.cannon-ball-2d
  (:require [goog.object :as g]
            [core.vector :as v]
            [core.canvas :as c]))


(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(def sim-min-width 20)
(def height (- (.-innerHeight js/window) 100))
(def width (- (.-innerWidth js/window) 20))
(def c-scale (/ (Math/min width height) sim-min-width))
(set! (.-height canvas) height)
(set! (.-width canvas) width)
(def sim-width (/ (.-width canvas) c-scale))

(def gravity #js {:x 0.0 :y -10.0})
(def time-step (/ 1.0 60.0))

(def ball #js {:radius 0.2
               :pos #js {:x 0.2 :y 0.2}
               :vel #js {:x 10.0 :y 15.0}})

(defn cx [pos]
  (* (.-x pos) c-scale))

(defn cy [pos]
  (- (.-height canvas) (* (.-y pos) c-scale)))

(defn setup [])

(defn simulate
  "Simulamos la nueva posición modificando la velocidad a partir de la
  aceleración (gravedad) y la posición a partir de la velocidad."
  []
  (set! (.-x (.-vel ball)) (+ (.-x (.-vel ball))
                              (* time-step (.-x gravity))))
  (set! (.-y (.-vel ball)) (+ (.-y (.-vel ball))
                              (* time-step (.-y gravity))))
  (set! (.-x (.-pos ball)) (+ (.-x (.-pos ball))
                              (* time-step (.-x (.-vel ball)))))
  (set! (.-y (.-pos ball)) (+ (.-y (.-pos ball))
                              (* time-step (.-y (.-vel ball)))))
  ;; Comprobamos parecedes y suelo
  (when (< (.-x (.-pos ball)) 0.0)
    (set! (.-x (.-pos ball)) 0.0)
    (set! (.-x (.-vel ball)) (- (.-x (.-vel ball)))))
  (when (> (.-x (.-pos ball)) sim-width)
    (set! (.-x (.-pos ball)) sim-width)
    (set! (.-x (.-vel ball)) (- (.-x (.-vel ball)))))
  (when (< (.-y (.-pos ball)) 0.0)
    (set! (.-y (.-pos ball)) 0.0)
    (set! (.-y (.-vel ball)) (- (.-y (.-vel ball))))))

(defn draw-ball []
  (. ctx (beginPath))
  (. ctx (clearRect 0 0 (.-width canvas) (.-height canvas)))
  (set! (.-fillStyle ctx) (name "blue"))
  (. ctx (arc (cx (.-pos ball))
              (cy (.-pos ball))
              (* c-scale (.-radius ball))
              0.0
              (* 2.0 Math/PI)))
  (. ctx (closePath))
  (. ctx (fill)))

(defn draw []
  (simulate)
  (draw-ball)
  #_(js/requestAnimationFrame draw))
