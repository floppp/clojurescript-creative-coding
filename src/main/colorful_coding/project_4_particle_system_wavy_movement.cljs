;; Para que funcione con volatile! he tenido que usar un p5.vector par alpha,
;; que permite modificación in-place sin pegarme con Clojure.
;;
;; La opción mejor habría sido usar #js, pero quería probar protocolos.
;;
;; Para borrar no hago lo que él porque bastante complicado de hacer aquí,
;; simplemente gastos algunos valores fijos que he visto que van bien
;; y recorto el vector a ellos cuando los supera.
(ns colorful-coding.project-4-particle-system-wavy-movement
  (:require [p5]))

(def state (volatile! {:width 800 :height 800 :p nil :ps nil}))
(def w (-> @state :width))
(def h (-> @state :height))

(defprotocol P5Object
  (show [this] "Cómo se presenta un elemento en p5js.")
  (updatep [this] "Elementos que e tienen que actualizar."))

(defrecord Particle [pos vel acc rgb alpha]
  P5Object
  (show [t]
    (js/noStroke)
    (js/fill (-> t :rgb .-x) (-> t :rgb .-y) (-> t :rgb .-z)  (-> t :alpha .-x))
    (js/ellipse (-> t :pos .-x)
                (-> t :pos .-y)
                10))
  (updatep [t]
    (.add (:vel t) (:acc t))
    (.add (:pos t) (:vel t))
    (let [dis (js/dist (/ w 2) (/ h 2) (-> pos .-x) (-> pos .-y))
          rgb  (js/createVector
                (js/map (-> pos .-x) 0 w 0 255)
                (js/map (-> pos .-y) 0 h 0 255)
                (js/map dis 0 (/ w 2) 0 255))
          m (js/map (js/sin (* js/frameCount 6)) -1 1 0.4 0.6)]
      (.mult ^js (:acc t) m)
      (.sub (:rgb t) (:rgb t))
      (.add (:rgb t) rgb)
      (when (> dis 100)
        (.add (:alpha t) (js/createVector -2 0))))))

(defn make-particle []
  (let [pos (js/createVector (/ w 2) (/ h 2))]
    (->Particle pos
                (js/createVector 0 0)
                (.normalize (.random2D p5/Vector))
                (js/createVector
                 (js/map (-> pos .-x) 0 w 0 255)
                 (js/map (-> pos .-y) 0 h 0 255)
                 (js/map (js/dist (/ w 2) (/ h 2) (-> pos .-x) (-> pos .-y)) 0 (/ w 2) 0 255))
                (js/createVector 255 0))))

(defn remove-index [v idx]
  (vec (remove #(= (first %) idx) (map-indexed vector v))))

(defn setup []
  (js/createCanvas w h)
  (js/angleMode js/DEGREES)
  (vswap! state (fn [st] (assoc st :p (make-particle)))))

(defn draw []
  (js/background 30)
  (if (< (count (:ps @state)) 1500)
    (vswap! state
            (fn [st]
              (assoc st :ps (vec (into (:ps st) (mapv (fn [_] (make-particle)) (range 5)))))))
    (vswap! state
            (fn [st]
              (assoc st :ps (subvec (:ps st) 500 1500)))))
  (doseq [idx (range (count (:ps @state)))]
    (let [p (nth (:ps @state) idx)]
      (when (> (-> p :alpha .-x) 0)
        (show p)
        (updatep p)))))

(comment
  (make-particle))
