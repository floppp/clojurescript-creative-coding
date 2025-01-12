(ns qoback.particles-connection
  (:require [p5]))

(def mass #js [])
(def density 100)
(def ps #js [])
(def limit 200)
(def dt 0.1)

(defn make-particle [idx]
  #js{:id idx
      :diameter 6
      :radius 3
      :pos #js {:x (js/random 50 750)
                :y (js/random 50 550)}
      :vel #js {:x (js/random -10 10)
                :y (js/random -10 10)}
      :acc #js {:x 0 :y 0}})

(defn setup []
  (doseq [idx (range 20)]
    (aset ps idx (make-particle idx)))
  (let [canvas (js/createCanvas 800 600)]
    (.parent canvas "p5jsparent")
    (js/angleMode js/DEGREES)
    (js/background 30)))

(defn update-ps []
  (doseq [^js p ps]
    (let [new-x (+ (.. p -pos -x) (* dt (.. p -vel -x)))
          new-y (+ (.. p -pos -y) (* dt (.. p -vel -y)))]
      (set! (.. p -pos -x) new-x)
      (set! (.. p -pos -y) new-y)
      (when (< (.-x (.-pos p)) (.-radius p))
        (set! (.-x (.-pos p))  (.-radius p))
        (set! (.-x (.-vel p)) (- (.-x (.-vel p)))))
      (when (> (.-x (.-pos p)) (- 800 (.-radius p)))
        (set! (.-x (.-pos p)) (- 800 (.-radius p)))
        (set! (.-x (.-vel p)) (- (.-x (.-vel p)))))
      (when (< (.-y (.-pos p)) (.-radius p))
        (set! (.-y (.-pos p)) (.-radius p))
        (set! (.-y (.-vel p)) (- (.-y (.-vel p)))))
      (when (> (.-y (.-pos p)) (- 600 (.-radius p)))
        (set! (.-y (.-pos p)) (- 600 (.-radius p)))
        (set! (.-y (.-vel p)) (- (.-y (.-vel p))))))))

(defn update-vs []
  (when (aget mass 0)
    (doseq [^js p ps]
      (let [new-x-vel (+ (.. p -vel -x) (* 0.01 dt (.. p -acc -x)))
            new-y-vel (+ (.. p -vel -y) (* 0.01 dt (.. p -acc -y)))]
        (set! (.-x (.-vel p)) new-x-vel)
        (set! (.-y (.-vel p)) new-y-vel)))))

(defn draw-lines []
  ;; naive primero.
  (doseq [^js p ps
          ^js q ps]
    (let [d (Math/sqrt (+ (Math/pow (- (.. p -pos -x) (.. q -pos -x)) 2)
                          (Math/pow (- (.. p -pos -y) (.. q -pos -y)) 2)))]
      (when (and (not= 0 d) (< d limit))
        (js/stroke 255
                   255
                   255
                   (* (/ (- limit d) limit) 255))
        (js/line (.. p -pos -x)
                 (.. p -pos -y)
                 (.. q -pos -x)
                 (.. q -pos -y))))))

(defn compoute-ps-accelerations []
  (when (aget mass 0)
    (js/stroke 255 255 255)
    (doseq [^js p ps]
      (let [px (.. p -pos -x)
            py (.. p -pos -y)
            mx (aget mass 0)
            my (aget mass 1)
            deltax (- mx px)
            deltay (- my py)
            r (+ (* deltax deltax) (* deltay deltay))
            mass-m (* density (aget mass 3))
            #_(js/line px
                       py
                       mx
                       my)
            x-acc (* mass-m  (/ (- mx px) r))
            y-acc (* mass-m  (/ (- my py) r))]
        (set! (.-acc p) #js {:x x-acc :y y-acc})
        #_(.log js/console mass-m r (.-acc p))))))

(defn draw-mass []
  (js/fill 50 55 255)
  (js/circle (aget mass 0) (aget mass 1) (aget mass 3)))

(defn update-mass []
  (let [omass (aget mass 2)]
    (when-not (nil? omass)
      (let [delta (/ (- js/frameCount omass) 10)]
        (when (< delta 50)
          (aset mass 3 delta))))))

(defn set-mass
  [& args]
  (let [old-diam (aget mass 2)]
    (aset mass 0 js/mouseX)
    (aset mass 1 js/mouseY)
    (aset mass 2 (if (nil? old-diam) js/frameCount (aget mass 2)))
    (when args
      (aset mass 3 js/frameCount))))

(defn mouse-pressed []
  (set-mass "init"))

(defn mouse-dragged []
  (set-mass))

(defn clean-mass []
  (aset mass 0 nil)
  (aset mass 1 nil)
  (aset mass 2 nil)
  (aset mass 3 nil))

(defn mouse-released [] (clean-mass))

(defn draw []
  (js/background 30)
  (draw-lines)
  (js/fill 50 55 255)
  (doseq [p ps]
    (js/circle (.. p -pos -x) (.. p -pos -y) (.-diameter p)))
  (update-mass)
  ;; tiene que estar antes de actualizar
  (compoute-ps-accelerations)
  (draw-mass)
  (update-vs)
  (update-ps))
