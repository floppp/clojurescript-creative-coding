(ns app
  (:require [goog.object :as g]
            [challenges.challenge-1-starlight]
            [challenges.challenge-007-solar-system-2d]
            [challenges.challenge-008-solar-system-3d]
            [challenges.challenge-078-simple-particle-system]
            [challenges.challenge-102-2d-water-ripple]
            [challenges.challenge-103-fire-effect]
            [challenges.challenge-132-fluid-simulation]
            [challenges.challenge-180-falling-sand]
            [colorful-coding.project-1-sine-wave]
            [colorful-coding.project-2-spirograph]
            [colorful-coding.project-4-particle-system-wavy-movement]
            [colorful-coding.project-9-flow-field]
            [lramirez.flow-field-generator]
            [idmnyu.random-pixels]
            [ten-minute-physics.cannon-ball-2d]
            [radu.how-math-can-make-your-code-better :as ch]))


;; -------------------------------------------
;;  Artefactos necesarios para funcionamiento
;; -------------------------------------------
(defn windowResized []
  (let [w js/window.innerWidth
        h js/window.innerHeight]
    (js/resizeCanvas w h)))

(defn init
  ([] (init false))
  ([should-resize?]
   ;; Definimos qué funciones se llamarán por p5.js
   (doto js/window
     (g/set "setup" ch/setup)
     (g/set "draw" ch/draw)
     ;; (g/set "mousePressed" ch/mouse-pressed)
     ;; (g/set "mouseDragged" ch/mouse-dragged)
     )
   (when should-resize?
     (g/set js/window "windowResized" windowResized))))

(defn start [] ;; defn ^:dev/after-load start []
  (js/console.log "<<< start >>>")
  (ch/setup)
  (init)
  (js/reDraw))

(defn stop [] ;; defn ^:dev/before-load stop []
  (js/console.log ">>> stop <<<"))
