;; Demasiado mix entre record, atom y #js. Debería haberlo hecho todo con #js.
(ns radu.how-math-can-make-your-code-better
  (:require [goog.object :as g]
            [core.vector :as v]
            [core.canvas :as c]))

(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(def height (- (.-innerHeight js/window) 100))
(def width (- (.-innerWidth  js/window) 100))
(def size 700)
(def full-circ (* 2.0 Math/PI))
(set! (.-height canvas) size)
(set! (.-width canvas) size)
(set! (.. canvas -style -backgroundColor) "black") ;; igual ;; (set! (.-backgroundColor (.-style canvas)) "black")

(defrecord Track [center radius])
;; necesito objeto de #js para setear
(defrecord Ball [track radius speed offset center])

(defn get-track-position
  [track offset]
  #js {:x (+ (.. track -center -x) (* (Math/cos (* 5 offset)) ^number (.-radius track)))
       :y (- (.. track -center -y) (* (Math/sin offset) ^number (.-radius track)))})

(defn draw-track
  [ctx track offset]
  (let [{:keys [center radius]} track]
    (. ctx (beginPath))
    (doseq [a (range 0 full-circ 0.1)]
      (let [pos (get-track-position track a)]
        (. ctx (lineTo (.-x pos) (.-y pos)))))
    ;; (. ctx (arc (.-x center) (.-y center) radius 0 full-circ))
    (. ctx (closePath))
    (set! (.-strokeStyle ctx) "white")
    (. ctx (stroke))))

(defn draw-ball
  [ctx ball]
  (let [{:keys [center radius]} ball]
    (. ctx (beginPath))
    (. ctx (arc (.-x center) (.-y center) radius 0 full-circ))
    (. ctx (closePath))
    (set! (.-strokeStyle ctx) "white")
    (. ctx (stroke))))

(defn move-ball
  [ctx {:keys [track radius speed offset center]}]
  (let [new-offset (+ offset speed)]
    (->Ball
     track
     radius
     speed
     new-offset
     (get-track-position track new-offset))))

(defn animate
  [ctx track ball]
  ;; (. ctx (clearRect 0 0 size size))
  ;; (draw-track ctx track (:offset @ball))
  (draw-ball ctx @ball)
  (let [new-ball (move-ball ctx @ball)]
    (reset! ball new-ball))
  ;; no hace falta porque en app.cljs estamos contectando el draw de p5 con esto, por lo que él se encarga
  ;; de llamar cada n frames.
  #_(js/requestAnimationFrame animate))

(def track (->Track #js {:x (/ size 2) :y (/ size 2)} 100))
(def ball-speed 0.01)
(def ball (atom (->Ball track 10 ball-speed 0 (get-track-position track 0))))

(defn setup []
  (draw-track ctx track (:offset @ball))
  #_(let [track (->Track #js {:x (/ size 2) :y (/ size 2)} 100)
          ball (->Ball track 10 0.1 0 (get-track-position track 0))]
      (draw-track ctx track (:offset ball))
      (draw-ball ctx ball)))

(defn draw []
  (animate ctx track ball))
