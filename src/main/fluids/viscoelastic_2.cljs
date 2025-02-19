;; First Step of
;; https://github.com/kotsoft/particle_based_viscoelastic_fluid?tab=readme-ov-file
(ns fluids.viscoelastic-2
  #_(:require [p5]))

(defn make-material [name rest-density stiffness near-stiffness kernel-radius]
  {:name name
   :rest-density rest-density
   :stiffness stiffness
   :near-stiffness near-stiffness
   :kernel-radius kernel-radius
   :max-pressure 1})

(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(def height (- (.-innerHeight js/window) 100))
(def width (- (.-innerWidth js/window) 20))
(def grav-x 0)
(def grav-y 0.2)
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
(def kernel-radius-inv (/ 1 kernel-radius))
(def rest-density 2)
(def stiffness 0.5)
(def near-stiffness 0.5)
(def neighbor-indexes #js [])
(def neighbor-unitx #js [])
(def neighbor-unity #js [])
(def neighbor-closeness #js [])
(def visited-buckets #js [])
(def num-hash-buckets 2000)
(def particle-list-heads #js [])
(def particle-list-next-idx [])
(def bucket-size-inv (/ 1 kernel-radius))

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

(defn get-hash-bucket-idx
  [bucket-x bucket-y]
  (let [h (bit-xor (* bucket-x 92837111) (* bucket-y 689287499))]
    ;; (.log js/console bucket-x bucket-y h (mod (js/Math.abs h) num-hash-buckets))
    (mod (js/Math.abs h) num-hash-buckets)))

(defn double-density-relaxation!
  []
  (set! (.-length visited-buckets) 0)
  (doseq [i (range n-particles)]
    (let [p0 (aget particles i)
          bucket-x (js/Math.floor (* (.-x p0) kernel-radius-inv))
          bucket-y (js/Math.floor (* (.-y p0) kernel-radius-inv))
          density (atom 0)
          near-density (atom 0)
          num-neihgbors (atom 0)
          num-visited-buckets (atom 0)]
      (doseq [bucket-dx (range -1 2)
              bucket-dy (range -1 2)]
        (let [bucket-idx (get-hash-bucket-idx (js/Math.floor (+ bucket-x bucket-dx))
                                              (js/Math.floor (+ bucket-y bucket-dy)))
              found (some #(= % bucket-idx) visited-buckets)]
          (when (not found)
            (aset visited-buckets @num-visited-buckets bucket-idx)
            (swap! num-visited-buckets (fn [old] (inc old)))
            (loop [neighbor-idx (aget particle-list-heads bucket-idx)]
              (if (= -1 neighbor-idx)
                ()
                (if (= i neighbor-idx)
                  (recur (aget particle-list-next-idx neighbor-idx))
                  (let [p1 (aget particles neighbor-idx)
                        diffx (- (.-x p1) (.-x p0))]
                    (if (or (> diffx kernel-radius) (< diffx (- kernel-radius)))
                      (recur (aget particle-list-next-idx neighbor-idx))
                      (let [diffy (- (.-y p1) (.-y p0))]
                        (or (> diffy kernel-radius) (< diffy (- kernel-radius)))
                        (let [rSq (+ (* diffx diffx) (* diffy diffy))]
                          (when (< rSq kernel-radius-square)
                            (let [r (js/Math.sqrt rSq)
                                  q (* r kernel-radius-inv)
                                  closeness (- 1 q)
                                  closeness-sq (* closeness closeness)]
                              (swap! density (fn [old] (+ old closeness-sq)))
                              (swap! near-density (fn [old] (+ old (* closeness closeness-sq))))
                              (aset neighbor-indexes @num-neihgbors neighbor-idx)
                              (aset neighbor-unitx @num-neihgbors (/ diffx r))
                              (aset neighbor-unity @num-neihgbors (/ diffy r))
                              (aset neighbor-closeness @num-neihgbors closeness)
                              (swap! num-neihgbors (fn [old] (inc old)))))
                          (recur (aget particle-list-next-idx neighbor-idx))))))))))))
      (let [pressure (atom (* stiffness (- @density rest-density)))
            near-pressure (atom (* near-stiffness @near-density))
            dispx (atom 0)
            dispy (atom 0)]
        (when (> @pressure 1)
          (reset! pressure 1))
        (when (> @near-pressure 1)
          (reset! near-pressure 1))
        (doseq [j (range @num-neihgbors)]
          (let [p1 (aget particles (aget neighbor-indexes j))
                closeness (aget neighbor-closeness j)
                D (/ (* dt dt (+ (* @pressure closeness) (* @near-pressure closeness closeness)))
                     2)
                DX (* D (aget neighbor-unitx j))
                DY (* D (aget neighbor-unity j))]
            (set! (.-x p1) (+ (.-x p1) DX))
            (set! (.-y p1) (+ (.-y p1) DY))
            (swap! dispx (fn [o] (- o DX)))
            (swap! dispy (fn [o] (- o DY)))))
        (set! (.-x p0) (+ (.-x p0) @dispx))
        (set! (.-y p0) (+ (.-y p0) @dispy))))))

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

(defn populate-hash-grid!
  []
  ;; Limpiamos en cada iteración
  (doseq [i (range num-hash-buckets)]
    (aset particle-list-heads i -1))
  ;; Poblamos `hash-grid`
  (doseq [i (range n-particles)]
    (let [p (aget particles i)
          bucket-x (js/Math.floor (* (.-x p) bucket-size-inv))
          bucket-y (js/Math.floor (* (.-y p) bucket-size-inv))
          bucket-idx (get-hash-bucket-idx bucket-x bucket-y)]
      (aset particle-list-next-idx i (aget particle-list-heads bucket-idx))
      (aset particle-list-heads bucket-idx i)))
  )

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
  (populate-hash-grid!)
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

