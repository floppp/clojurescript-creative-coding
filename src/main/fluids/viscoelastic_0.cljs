;; First Step of
;; https://github.com/kotsoft/particle_based_viscoelastic_fluid?tab=readme-ov-file
(ns fluids.viscoelastic-0
  #_(:require [p5]))


(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(def height (- (.-innerHeight js/window) 100))
(def width (- (.-innerWidth js/window) 20))
(def running (atom false))
(def grav-x 0)
(def grav-y 0.1)
(set! (.-height canvas) height)
(set! (.-width canvas) width)
(def dt 1)
(def particles #js [])
(def n-particles 2000)
(def boundary-mul 0.5)
(def boundary-minx 5)
(def boundary-maxx (- width 5))
(def boundary-miny 5)
(def boundary-maxy (- height 5))

(defn make-particle [x y  vx vy]
  #js{:x x :y y :prev-x x :prev-y y :vx vx :vy vy})

(defn init-particles!
  [n-particles]
  (doseq [_ (range n-particles)]
    (let [p (make-particle (* (js/Math.random) width)
                           (* (js/Math.random) height)
                           (dec (* (js/Math.random) 2))
                           (dec (* (js/Math.random) 2)))]
      (.push particles p))))

;; no action in step 0
(defn apply-viscosity! [])
(defn adjust-spring! [])
(defn apply-spring-displacement! [])
(defn double-density-relaxation! [])

(defn resolve-collisions!
  []
  (doseq [^js p particles]
    (cond
      (< (.-x p) boundary-minx) (set! (.-x p) (+ (.-x p) (* boundary-mul (- boundary-minx (.-x p)))))
      (> (.-x p) boundary-maxx) (set! (.-x p) (+ (.-x p) (* boundary-mul (- boundary-maxx (.-x p)))))
      :else "")
    (cond
      (< (.-y p) boundary-miny) (set! (.-y p) (+ (.-y p) (* boundary-mul (- boundary-miny (.-y p)))))
      (> (.-y p) boundary-maxy) (set! (.-y p) (+ (.-y p) (* boundary-mul (- boundary-maxy (.-y p)))))
      :else "")))

(defn adjust-velocity!
  []
  (doseq [^js p particles]
    (set! (.-vx p) (/ (- (.-x p) (.-prev-x p)) dt))
    (set! (.-vy p) (/ (- (.-y p) (.-prev-y p)) dt))))

(defn update!
  []
  (doseq [^js p particles]
    (set! (.-vx p) (+ (.-vx p) (* grav-x dt)))
    (set! (.-vy p) (+ (.-vy p) (* grav-y dt))))
  (apply-viscosity!)
  (doseq [^js p particles]
    (set! (.-prev-x p) (.-x p))
    (set! (.-prev-y p) (.-y p))
    (set! (.-x p) (+ (.-x p) (* (.-vx p) dt)))
    (set! (.-y p) (+ (.-y p) (* (.-vy p) dt))))
  (adjust-spring!)
  (apply-spring-displacement!)
  (double-density-relaxation!)
  (resolve-collisions!)
  (adjust-velocity!))

(defn setup
  []
  (init-particles! n-particles))

(defn draw
  []
  ()
  (. ctx (clearRect 0 0 (.-width canvas) (.-height canvas)))
  ;; (. ctx (save))
   ;; (. ctx (translate (- 2.5) (- 2.5)))
  (set! (.-fillSstyle ctx) "#0066FF")
  (doseq [p particles]
    (. ctx (fillRect (.-x p) (.-y p) 5 5)))
  ;; (. ctx (restore))
  ;; Vamos a intentar evitar p5.
  (js/requestAnimationFrame draw))
