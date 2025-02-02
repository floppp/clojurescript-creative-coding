(ns app
  (:require [goog.object :as g]
            #_[challenges.challenge-001-starlight]
            #_[challenges.challenge-007-solar-system-2d]
            #_[challenges.challenge-008-solar-system-3d]
            #_[challenges.challenge-078-simple-particle-system]
            #_[challenges.challenge-102-2d-water-ripple]
            #_[challenges.challenge-103-fire-effect]
            #_[challenges.challenge-132-fluid-simulation]
            #_[challenges.challenge-180-falling-sand :as ch]
            #_[colorful-coding.project-1-sine-wave :as ch]
            #_[colorful-coding.project-2-spirograph :as ch]
            #_[colorful-coding.project-4-particle-system-wavy-movement :as ch]
            #_[colorful-coding.project-9-flow-field :as ch]
            #_[lramirez.flow-field-generator :as ch] ;; NO ACABADO
            #_[lramirez.circular-motion :as ch] ;; NO ACABADO
            #_[idmnyu.random-pixels :as ch]
            #_[ten-minute-physics.cannon-ball-2d :as ch]
            #_[ten-minute-physics.fluid-simulation :as ch]
            #_[radu.how-math-can-make-your-code-better :as ch]
            #_[qoback.particles-connection :as ch]
            #_[qoback.particles-connection-canvas :as ch]
            #_[qoback.particles-connection-canvas-space-splitting :as ch]
            #_[generative-design.colors :as ch]
            [generative-design.random-traces :as ch]
            ))


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
     ;; (g/set "preload" ch/preload)
     (g/set "setup" ch/setup)
     (g/set "draw" ch/draw)
     ;; (g/set "mousePressed" ch/mouse-pressed)
     ;; (g/set "mouseDragged" ch/mouse-dragged)
     ;; (g/set "mouseReleased" ch/mouse-released)
     )
   (when should-resize?
     (g/set js/window "windowResized" windowResized))))

(defn start [] ;; defn ^:dev/after-load start []
  (js/console.log "<<< start >>>")
  (ch/setup)
  (init)
  #_(js/reDraw))

(defn stop [] ;; defn ^:dev/before-load stop []
  (js/console.log ">>> stop <<<"))
