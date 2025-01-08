(ns challenges.challenge-078-simple-particle-system
  (:require [p5]))

(def particles #js [])
(def n-particles 10)

(defprotocol Update
  (update-particle [this]))

(defprotocol Show
  (show [this]))

(defrecord Particle [x y vx vy alpha]
  Show
  (show [t]
    ;; (js/stroke 255)
    (js/noStroke)
    (js/fill 255 150 0 (-> t :alpha))
    (js/ellipse (-> t :x) (-> t :y) 18 18))
  Update
  (update-particle [t]
    (set! (.-x t) (+ (.-x t) (.-vx t)))
    ;; (set! (.-y t) (+ (.-y t) (* (/ (.-y t) 25) (.-vy t))))
    (set! (.-y t) (+ (.-y t) (.-vy t)))
    (set! (.-alpha t) (- (.-alpha t) 2))))

(defn make-particle []
  (->Particle 300
              380
              (js/random (js/random -1.5 -0.5) (js/random 0.5 1.5))
              (js/random -3 -1)
              255))

(defn setup []
  (let [canvas (js/createCanvas 600 400)]
    (.parent canvas "p5jsparent")))

(defn draw []
  (js/background 0)

  (doseq [_ (range 4)]
    (.push particles (make-particle)))
  ;; Filtrando solamente ya funciona, no necesito borrar, pero mejor borrar
  #_(doseq [p (filter #(> (.-alpha %) 1) particles)]
      (show p)
      (update-particle p))
  ;; Para borrar itero al revés para evitar glitch:
  ;;     hacer splice -> saltar una partícula -> vuelve en siguiente iteración
  (let [c (dec (count particles))]
    (doseq [idx (range c)]
      (let [reversed-idx (- c idx)
            p (aget particles reversed-idx)]
        (show p)
        (update-particle p)
        (when (< (.-alpha p) 0)
          (.splice particles reversed-idx 1))))))
