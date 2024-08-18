(ns colorful-coding.project-2-spirograph
  (:require [p5]))

#_(def state #js {:r1 (+ 100 (rand-int 50))
                  :r2 (+ 100 (rand-int 50))
                  :a1 0
                  :a2 0
                  :prevX nil
                  :prevY nil
                  :a1Inc nil
                  :a2Inc nil
                  :speed 50
                  })

(def r1 (volatile! (+ 100 (rand-int 50))) )
(def r2 (volatile! (+ 100 (rand-int 50))))
(def a1 (volatile! 0))
(def a2 (volatile! 0))
(def prevX (volatile! nil))
(def prevY (volatile! nil))
(def a1Inc (volatile! nil))
(def a2Inc (volatile! nil))
(def speed (volatile! 50))


(defn setup
  []
  (vreset! a1Inc (js/random 0.1 5))
  (vreset! a2Inc (js/random 0.1 5))
  (js/createCanvas 800 800)
  (js/angleMode js/DEGREES)
  (js/background 30))



(defn draw []
  (js/translate (/ js/width 2) (/ js/height 2))

  (doseq [_ (range @speed)]
    (let [x1 (* @r1 (js/cos @a1))
          y1 (* @r1 (js/sin @a1))
          x2 (+ x1 (* @r2 (js/cos @a2)))
          y2 (+ y1 (* @r2 (js/sin @a2)))
          r (js/map (js/sin js/frameCount) -1 1 100 200)
          g (js/map (js/sin js/frameCount) -1 1 100 200)
          b (js/map (js/sin js/frameCount) -1 1 200 100)]
      (js/stroke r g b)

      ;; Si no compruebo el valor nil/no nil, me aparece ralla desde 0
      ;; hasta el primer punto calculado.
      (when-not (nil? @prevX)
        (js/line @prevX
                 @prevY
                 x2 y2))

      (vreset! prevX x2)
      (vreset! prevY y2)

      (vreset! a1 (+ @a1 @a1Inc))
      (vreset! a2 (+ @a2 @a2Inc)))
    ))


;; Estados muy chulos
;; #js {:speed 50 :r1 121, :r2 136, :a1 0, :a2 0, :prevX nil, :prevY nil, :a1Inc 0.7508956561156331, :a2Inc 3.8642557890718057}
;; #js {:a2 0, :a1Inc 4.275794517513079, :speed 50, :a1 0, :r2 136, :a2Inc 1.0797030798400762, :r1 109, :prevY nil, :prevX nil}
;; #js {:a2 0, :a1Inc 1.2013414944503171, :speed 50, :a1 0, :r2 103, :a2Inc 3.7605190603179026, :r1 141, :prevY nil, :prevX nil}
