(ns ten-minute-physics.fluid-simulation
  (:require [goog.object :as g]
            [core.vector :as v]
            [core.canvas :as c]))

;; Representamos el fluido como un campo de velocidades sobre el grid
;; en que dividimos el canvas.
(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(def sim-height 1.1)
(set! (.-width canvas) (- (.-innerWidth js/window) 20))
(set! (.-height canvas) (- (.-innerHeight js/window) 100))

(def c-scale (/ (.-height canvas) sim-height))
(def sim-width (/ (.-width canvas) c-scale))
(def u-field 0)
(def v-field 1)
(def s-field 2)
(def cnt (atom 0))

(defn c-x [x] (* x c-scale))
(defn c-y [y] (- (.-height canvas) (* y c-scale)))

(defn make-fluid [density num-x num-y h]
  (let [num-x (+ 2 num-x)
        num-y (+ 2 num-y)
        num-cells (* num-x num-y)
        m (js/Float32Array. num-cells)]
    (.fill m 1.0)
    #js {:density density
         :num-x num-x
         :num-y num-y
         :num-cells num-cells
         :h h
         :u (js/Float32Array. num-cells)
         :v (js/Float32Array. num-cells)
         :new-u (js/Float32Array. num-cells)
         :new-v (js/Float32Array. num-cells)
         ;; campo presiones
         :p (js/Float32Array. num-cells)
         ;; campo `smoke`, el valor del fluido en sí
         :s (js/Float32Array. num-cells)
         :m m
         :new-m (js/Float32Array. num-cells)}))

(defn integrate-fluid! [fluid dt gravity]
  (let [n (:num-y fluid)]
    (doseq [i (range 1 (:num-x fluid))
            j (range 1 (dec n))]
      (let [fst (aget (:s fluid) (+ j (* i n)))
            scd (aget (:s fluid) (+ (dec j) (* i n)))]
        (when (and (not= fst 0) (not= scd 0))
          (aset (:v fluid) (+ j (* i n))
                (+ (* gravity dt)
                   (aget (:v fluid) (+ j (* i n))))))))))

(defn solve-fluid-incompresibility [fluid n-iters dt]
  (let [n (:num-y fluid)
        cp (* (:density fluid) (/ (:h fluid) dt))
        s (:s fluid)]
    (doseq [iter (range 0 n-iters)
            i (range 1 (dec (:num-x fluid)))
            j (range 1 (dec (:num-y fluid)))]
      (when-not (= (aget (:s fluid) (+ j (* i n))) 0)
        (let [current-p (+ j (* i n))
              prev-p (+ (dec j) (* i n))
              next-p (+ (inc j) (* i n))
              prev-row (+ j (* (dec i) n))
              next-row (+ j (* (inc i) n))
              s (aget s current-p)
              sx0 (aget s prev-p)
              sx1 (aget s next-p)
              sy0 (aget s prev-row)
              sy1 (aget s next-row)
              s (+ sx0 sx1 sy0 sy1)]
          (when-not (zero? s)
            (let [div ()])))))))


(defn setup []

  )

(defn draw [])
