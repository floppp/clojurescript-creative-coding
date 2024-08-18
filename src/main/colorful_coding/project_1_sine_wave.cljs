(ns colorful-coding.project-1-sine-wave
  (:require [p5]))

(def state {:sizes {:w 1200 :h 1200}})
(def width (-> state :sizes :w))
(def height (-> state :sizes :h))

(defn setup []
  []
  (js/createCanvas width height js/WEBGL)
  (js/angleMode js/DEGREES))

(defn draw []
  []
  (js/background 30)
  (js/rotateX 60)
  (js/noFill)
  (js/stroke 255)
  ;; Se puede hacer con un único `doseq` pero queda bastante más claro así,
  ;; porque de la otra forma necesitamos hacer `when` para ver cuándo
  ;; llamamos a las funciones de inicio/fin.
  (doseq [i (range 150)]
    (let [r (js/map (js/sin (/ js/frameCount 1)) -1 1 100 200)
          g (js/map i 0 20 0 255)
          b (js/map (js/cos js/frameCount) -1 1 200 100)]
      (js/stroke r g b))
    (js/beginShape)
    (js/rotate (/ js/frameCount 200))
    (doseq [j (range 0 360 60)]
      ;; TODO: Quiero hacer que cada x tiempo se moficique el sentido del giro.
      ;; (js/rotate (/ js/frameCount 2000))
      (let [rad (* i 3)
            x (* (js/cos j) rad)
            y (* (js/sin j) rad)
            z (* (js/sin (+ (* js/frameCount 2) (* 5 i))) 50)]
        (js/vertex x y z)))
    (js/endShape js/CLOSE)))



