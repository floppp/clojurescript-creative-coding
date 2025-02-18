;; First Step of
;; https://github.com/kotsoft/particle_based_viscoelastic_fluid?tab=readme-ov-file
(ns fluids.viscoelastic-1
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
(def n-particles 700)
(def boundary-mul 0.5)
(def boundary-minx 5)
(def boundary-maxx (- width 5))
(def boundary-miny 5)
(def boundary-maxy (- height 5))
(def kernel-radius 40)
(def kernel-radius-square (* kernel-radius kernel-radius))
(def rest-density 2)
(def stiffness 1)
(def near-stiffness 0.5)
(def neighbor-indexes #js [])
(def neighbor-unitx #js [])
(def neighbor-unity #js [])
(def neighbor-closeness #js [])

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

(defn compute-density
  "Calculamos densidad y densidad cercada (`near-density`)."
  [^js p0 ^number i]
  (loop [j 0
         density 0
         near-density 0
         num-neighbors 0]
    ;; (.log js/console j n-particles (= j n-particles) particles)
    (if (= j n-particles)
      {:density density :near-density near-density :num-neighbors num-neighbors}
      (if (= i j)
        (recur (inc j) density near-density num-neighbors)
        (let [p1 (aget particles j)
              diffx (- (.-x p1) (.-x p0))
              diffy (- (.-y p1) (.-y p0))]
          ;; (.log js/console j)
          (cond
            (or (> diffx kernel-radius) (< diffx (- kernel-radius)))
            (recur (inc j) density near-density num-neighbors)
            (or (> diffy kernel-radius) (< diffy (- kernel-radius)))
            (recur (inc j) density near-density num-neighbors)
            :else
            (let [rsq (+ (* diffx diffx) (* diffy diffy))]
              (if (< rsq kernel-radius-square)
                (let [r (js/Math.sqrt rsq)
                      q (/ r kernel-radius)
                      closeness (- 1 q)
                      closeness-sq (* closeness closeness)]
                  (aset neighbor-indexes num-neighbors j)
                  (aset neighbor-unitx num-neighbors (/ diffx r))
                  (aset neighbor-unity num-neighbors (/ diffy r))
                  (aset neighbor-closeness num-neighbors closeness)
                  (recur (inc j)
                         (+ density (* closeness closeness))
                         (+ near-density (* closeness closeness-sq))
                         (inc num-neighbors)))
                (recur (inc j) density near-density num-neighbors)))))))))

(defn wall-density
  "Añadimos `wall-density`."
  [p0 density near-density]
  (let [closest-x (js/Math.min (.-x p0) (- width (.-x p0)))
        closest-y (js/Math.min (.-y p0) (- height (.-y p0)))
        d (atom density)
        nd (atom near-density)]
    (when (< closest-x kernel-radius)
      (let [q (/ closest-x kernel-radius)
            closeness (- 1 q)
            closeness-sq (* closeness closeness)]
        (swap! d (fn [old] (+ old closeness-sq)))
        (swap! nd (fn [old] (+ old (* closeness closeness-sq))))))
    (when (< closest-y kernel-radius)
      (let [q (/ closest-y kernel-radius)
            closeness (- 1 q)
            closeness-sq (* closeness closeness)]
        (swap! d (fn [old] (+ old closeness-sq)))
        (swap! nd (fn [old] (+ old (* closeness closeness-sq))))))
    {:density @d :near-density @nd}))

(defn compute-pressure
  [num-neighbors pressure near-pressure]
  (loop [dispx 0 dispy 0 j 0]
    (if (= j num-neighbors)
      {:dispx dispx :dispy dispy}
      (let [p1 (aget particles (aget neighbor-indexes j))
            closeness (aget neighbor-closeness j)
            D (/ (* dt dt (+ (* pressure closeness) (* near-pressure closeness closeness))) 2)
            DX (* D (aget neighbor-unitx j))
            DY (* D (aget neighbor-unity j))]
        (set! (.-x p1) (+ (.-x p1) DX))
        (set! (.-y p1) (+ (.-y p1) DY))
        (recur (- dispx DX) (- dispy DY) (inc j))))))

(defn double-density-relaxation!
  []
  (doseq [i (range n-particles)]
    ;; (.log js/console i particles)
    (let [p0 (aget particles i)
          {:keys [density near-density num-neighbors]} (compute-density p0 i)
          {:keys [density near-density]} (wall-density p0 density near-density)
          pressure (* stiffness (- density rest-density))
          near-pressure (* near-stiffness near-density)
          {:keys [dispx dispy]} (compute-pressure num-neighbors pressure near-pressure)
          ]
      (set! (.-x p0) (+ (.-x p0) dispx))
      (set! (.-y p0) (+ (.-y p0) dispy)))))

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
  (update!)
  (. ctx (clearRect 0 0 (.-width canvas) (.-height canvas)))
  ;; (. ctx (save))
  ;; (. ctx (translate (- 2.5) (- 2.5)))
  (set! (.-fillSstyle ctx) "#0066FF")
  (doseq [p particles]
    (. ctx (fillRect (.-x p) (.-y p) 5 5)))
  ;; (. ctx (restore))
  ;; Vamos a intentar evitar p5.
  (js/requestAnimationFrame draw))
 
