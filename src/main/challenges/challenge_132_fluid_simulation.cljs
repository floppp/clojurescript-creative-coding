(ns challenges.challenge-132-fluid-simulation
  (:require [p5]
            [core.vector :as v]))

(def size 256)

(def iterations 10)

;; Hay que iterar y modificar tanto que creo que lo mejor es
;; tirar de js directamente sin dudarlo.
(defn  make-fluid [N dt diffusion viscosity]
  #js {:size N :dt dt :diff diffusion :visc viscosity
       :s (clj->js (v/make-vector (* N N) 0.0))
       :density (clj->js (v/make-vector (* N N) 0.0))
       :vx (clj->js (v/make-vector (* N N) 0.0))
       :vy (clj->js (v/make-vector (* N N) 0.0))
       :vx0 (clj->js (v/make-vector (* N N) 0.0))
       :vxy (clj->js (v/make-vector (* N N) 0.0))})

(def state (make-fluid size 0.1 0.0 0.0))

(defn IX
  "Obtenemos coordenada 1D de la representación que corresponde
  con la 2D del modelo."
  [x y]
  (+ x (* size y)))

;; TODO: Esta implementación posiblemente haya de cambiarla porque irá excesivamente lenta.
(defn add-density-to-fluid
  [x y amount]
  (let [index (IX x y)
        density (.-density state)
        old-value (aget density index)]
    (aset density index (+ amount old-value))))

(defn add-velocity-to-fluid
  [x y amount-x amount-y]
  (let [index (IX x y)
        vx (.-vx state)
        vy (.-vy state)
        old-vx (aget vx index)
        old-vy (aget vy index)]
    (aset vx index (+ amount-x old-vx))
    (aset vy index (+ amount-y old-vy))))

(defn set-bound [xs v])
(defn linear-solver [b xs x0s a c]
  (let [c-recip (/ 1.0 c)]
    (for [_ (range iterations)
          j (range 1 (dec size))
          i (range 1 (dec size))]
      (let [new-value (* a
                         (+ (aget xs (IX (inc i) j))
                            (aget xs (IX (dec i) j))
                            (aget xs (IX i (inc j)))
                            (aget xs (IX i (dec j))))
                         c-recip)]
        (aset xs (IX i j) (+ (aget xs (IX i j)) new-value))
        (when (= i (dec size))
          (set-bound x0s b))))))

(defn project [vx vy p div]
  (for [j (range 1 (dec size))
        i (range 1 (dec size))]
    (let [new-value (/ (* -0.5
                          (+ (- (aget vx (IX (inc i) j))
                                (aget vx (IX (dec i) j)))
                             (- (aget vx (IX i (inc j)))
                                (aget vx (IX i (dec j))))))
                       size)]
      (aset div (IX i j) (+ (aget vx (IX i j))) new-value)
      (aset p (IX i j) 0)))
  (set-bound 0 div)
  (set-bound 0 p)
  (linear-solver 0 p div 1 6)

  (for [j (range 1 (dec size))
        i (range 1 (dec size))]
    (do
      (aset vx (IX i j) (- (aget vx (IX i j))
                           (* 0.5
                              size
                              (- (aget p (IX (inc i) j))
                                 (aget p (IX (dec i) j))))))
      (aset vy (IX i j) (- (aget vy (IX i j))
                           (* 0.5
                              size
                              (- (aget p (IX (inc i) j))
                                 (aget p (IX (dec i) j)))))))
    )
  (set-bound 1 vx)
  (set-bound 2 vy)
  )

(defn advection [b d d0 vx]
  )

(defn diffuse [b xs x0s diff dt]
  (let [a (* dt diff (- size 2) (- size 2))]
    (linear-solver b xs x0s a (inc (* 6 a)))))

(defn setup []
  (js/createCanvas size size))

(defn draw   [])
