(ns qoback.particles-connection-canvas
  (:require [p5]
            [core.canvas :as c]
            [core.canvas-macros :refer-macros [circle]])
  #_(:require [core.canvas-macros :as c]))

(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(set! (.-height canvas) 600)
(set! (.-width canvas) 900)
(set! (.. canvas -style -backgroundColor) "black")

(def mass #js [])
(def density 100)
(def ps #js [])
(def limit 100)
(def dt 0.1)
(def frame-count (atom nil))

(defn make-particle
  ([idx] (make-particle idx false))
  ([idx empty-vel?]
   #js {:id idx
        :diameter 8
        :radius 4
        :pos #js {:x (js/random 50 850)
                  :y (js/random 50 550)}
        :vel (if empty-vel?
               #js {:x 0 :y 0}
               #js {:x (js/random -10 10)
                    :y (js/random -10 10)})
        :acc #js {:x 0 :y 0}}))

(defn draw-mass []
  (when-let [x (aget mass 0)]
    (let [fc @frame-count]
      (c/draw-circle ctx x (aget mass 1) fc "blue"))
    #_(.log js/console mass))
  #_(js/fill 50 55 255)
  #_(js/circle (aget mass 0) (aget mass 1) (aget mass 3)))

(defn set-mass
  [ev & args]
  (when (and (or args (aget mass 0)) (.-x ev))
    (let [x-val (- (.-x ev) (.-offsetLeft canvas))
          y-val (- (.-y ev) (.-offsetTop canvas))]
      (aset mass 0 x-val)
      (aset mass 1 y-val)
      (aset mass 2 20))))

(defn clean-mass []
  (aset mass 0 nil)
  (aset mass 1 nil)
  (aset mass 2 nil)
  (reset! frame-count nil))

(defn setup []
  (doseq [idx (range 50)]
    (aset ps idx (make-particle idx)))
  (.removeEventListener canvas "mouseup" clean-mass)
  (.removeEventListener canvas "touchend" clean-mass)
  (.removeEventListener canvas "mousedown" set-mass) ;; click no sirve porque se dispara después que `touchend`.
  (.removeEventListener canvas "mousemove" set-mass)
  (.addEventListener canvas "mouseup" clean-mass)
  (.addEventListener canvas "touchend" clean-mass)
  (.addEventListener
   canvas
   "mousedown"
   (fn [ev]
     (reset! frame-count 0)
     (set-mass ev "init")))
  (.addEventListener canvas "mousemove" set-mass))

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

(defn distance [p q]
  (Math/sqrt (+ (Math/pow (- (.. p -pos -x) (.. q -pos -x)) 2)
                (Math/pow (- (.. p -pos -y) (.. q -pos -y)) 2))))

(defn draw-lines []
  ;; naive primero.
  (doseq [^js p ps
          ^js q ps]
    (let [d (distance p q)]
      (when (and (not= 0 d) (< d limit))
        (c/draw-line
         ctx
         (.. p -pos -x)
         (.. p -pos -y)
         (.. q -pos -x)
         (.. q -pos -y)
         {:c (str "rgba(255, 255, 255, "  (/ (- limit d) limit) ")")})))))

(defn compoute-ps-accelerations []
  (when (aget mass 0)
    (doseq [^js p ps]
      (let [px (.. p -pos -x)
            py (.. p -pos -y)
            mx (aget mass 0)
            my (aget mass 1)
            deltax (- mx px)
            deltay (- my py)
            r (+ (* deltax deltax) (* deltay deltay))
            mass-m (* density (aget mass 2))
            x-acc (* mass-m  (/ (- mx px) r))
            y-acc (* mass-m  (/ (- my py) r))]
        (set! (.-acc p) #js {:x x-acc :y y-acc})
        #_(c/draw-line ctx px
                     py
                     mx
                     my)))))

(defn mouse-released [] (clean-mass))

(defn draw []
  (. ctx (clearRect 0 0 900 600))
  (draw-lines)
  (doseq [^js p ps]
    (c/draw-circle ctx (.. p -pos -x) (.. p -pos -y) (.-radius p) "blue"))
  ;; tiene que estar antes de actualizar
  (compoute-ps-accelerations)
  (draw-mass)
  (update-vs)
  (update-ps)
  #_(js/requestAnimationFrame draw)
  (let [fc @frame-count]
    (when (and (not (nil? fc)) (< fc 40))
      (swap! frame-count #(inc %)))))
