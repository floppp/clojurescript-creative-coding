(ns qoback.particles-connection-canvas
  (:require [p5]
            [core.canvas :as c]))

(def width 1000)
(def height 600)
(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(set! (.-height canvas) height)
(set! (.-width canvas) width)
(set! (.. canvas -style -backgroundColor) "black")
(def squares #js [])
(def mass #js [])
(def density 10)
(def ps #js [])
(def limit 100)
(def dt 0.1)
(def frame-count (atom nil))
(def n-particles 2000)

(defn make-particle
  ([idx] (make-particle idx true))
  ([idx moving-particle?]
   #js {:id idx
        :diameter 8
        :radius 4
        :square nil
        :pos #js {:x (js/random (/ limit 10) (- width (/ limit 10)))
                  :y (js/random (/ limit 10) (- height (/ limit 10)))}
        :vel (if moving-particle?
               #js {:x (js/random -10 10)
                    :y (js/random -10 10)}
               #js {:x 0 :y 0})
        :acc #js {:x 0 :y 0}}))

(defn draw-mass []
  (when-let [x (aget mass 0)]
    (let [fc (/ @frame-count 5)]
      (c/draw-circle ctx x (aget mass 1) fc "blue"))))

(defn set-mass
  [ev & args]
  (when (and (or args (aget mass 0)) (.-x ev))
    (let [x-val (- (.-x ev) (.-offsetLeft canvas))
          y-val (- (.-y ev) (.-offsetTop canvas))]
      (aset mass 0 x-val)
      (aset mass 1 y-val))))

(defn clean-mass []
  (aset mass 0 nil)
  (aset mass 1 nil)
  (reset! frame-count nil))

(defn get-neighbors-idx
  [^js p]
  (let [sq (.-square p)
        n-rows (/ height limit)
        n-cols (/ width limit)
        row (.-row sq)
        col (.-col sq)
        prev-row (if (zero? row) row (dec row))
        next-row (if (= row (dec n-rows)) row (inc row))
        prev-col (if (zero? col) col (dec col))
        next-col (if (= col (dec n-cols)) col (inc col))]
    (for [r (range prev-row (inc next-row))
          c (range prev-col (inc next-col))]
      (let [idx (+ c (* n-cols r))
            neighbor-p (aget squares idx)]
        neighbor-p))))

(defn split-space [& [draw?]]
  (set! (.-length squares) 0)
  (let [w (/ limit 1)]
    (doseq [idx (range (count ps))]
      (let [^js p (aget ps idx)
            n-cols (/ width w)
            row (Math/floor (/ (.. p -pos -y) w))
            col (Math/floor (/ (.. p -pos -x) w))
            square (+ (* row n-cols) col)
            current (aget squares square)]
        (set! (.-square p) #js {:row row :col col})
        (if (nil? current)
          (aset squares square #js [idx])
          (.push (aget squares square) idx))))
    (when draw?
      (doseq [yinit (range 0 height w)
              xinit (range 0 width w)]
        (let [x-idx (/ xinit w)
              y-idx (/ yinit w)
              square-idx (+ x-idx (* y-idx (/ width w)))]
          (.beginPath ctx)
          (.rect ctx xinit yinit w w)
          (if (nil? (aget squares square-idx))
            (set! (.-strokeStyle ctx) "rgba(255, 255, 255, 0.2")
            (set! (.-strokeStyle ctx) "red"))
          (.stroke ctx))))))

(defn setup []
  (doseq [idx (range n-particles)]
    (aset ps idx (make-particle idx true)))
  #_(doseq [^js p ps]
      (let [ns (remove nil? (get-neighbors-idx p))]
        (.log js/console ns)))
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
      (when (> (.-x (.-pos p)) (- width (.-radius p)))
        (set! (.-x (.-pos p)) (- width (.-radius p)))
        (set! (.-x (.-vel p)) (- (.-x (.-vel p)))))
      (when (< (.-y (.-pos p)) (.-radius p))
        (set! (.-y (.-pos p)) (.-radius p))
        (set! (.-y (.-vel p)) (- (.-y (.-vel p)))))
      (when (> (.-y (.-pos p)) (- height (.-radius p)))
        (set! (.-y (.-pos p)) (- height (.-radius p)))
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

(defn draw-lines-naive []
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

(defn draw-lines
  "Avoding redundant drawns."
  []
  (doseq [i (range n-particles)
          j (range (inc i) n-particles)]
    (let [^js p (nth ps i)
          ^js q (nth ps j)
          d (distance p q)]
      ;; (get-neighbors p)
      (when (< d limit)
        (let [opacity (/ (- limit d) limit)
              p-pos (.. p -pos)
              q-pos (.. q -pos)]
          (c/draw-line
           ctx
           (.-x p-pos) (.-y p-pos)
           (.-x q-pos) (.-y q-pos)
           {:c (str "rgba(255, 255, 255, " opacity ")")}))))))

(defn draw-neighbors-lines
  "Avoding redundant drawns."
  []
  (doseq [^js p ps]
    (let [ns (remove nil? (get-neighbors-idx p))]
      (doseq [^js n ns]
        (let [q (aget ps (first n))
              d (distance p q)]
          (when (< d limit)
            (let [opacity (/ (- limit d) limit)
                  p-pos (.. p -pos)
                  q-pos (.. q -pos)]
              (c/draw-line
               ctx
               (.-x p-pos) (.-y p-pos)
               (.-x q-pos) (.-y q-pos)
               {:c (str "rgba(255, 255, 255, " opacity ")")})))))))

  #_(doseq [i (range n-particles)
          j (range (inc i) n-particles)]
    (let [^js p (nth ps i)
          ^js q (nth ps j)
          d (distance p q)]
      ;; (get-neighbors p)
      (when (< d limit)
        (let [opacity (/ (- limit d) limit)
              p-pos (.. p -pos)
              q-pos (.. q -pos)]
          (c/draw-line
           ctx
           (.-x p-pos) (.-y p-pos)
           (.-x q-pos) (.-y q-pos)
           {:c (str "rgba(255, 255, 255, " opacity ")")}))))))

(defn draw-lines-batch-drawing
  "Batch drawing. CANNOT BE USED BECAUSE .strokeStyle is
  overriding each time, but nice technique if color/alpha
  remains constant."
  []
  (.beginPath ctx)
  (doseq [i (range (count ps))
          j (range (inc i) (count ps))]
    (let [^js p (nth ps i)
          ^js q (nth ps j)
          d (distance p q)]
      (when (< d limit)
        (let [opacity (/ (- limit d) limit)
              p-pos (.. p -pos)
              q-pos (.. q -pos)]
          ;; Add the line to the batch
          (.moveTo ctx (.-x p-pos) (.-y p-pos))
          (.lineTo ctx (.-x q-pos) (.-y q-pos))
          ;; Optionally set stroke style here
          (set! (.-strokeStyle ctx)
                (str "rgba(255, 255, 255, " opacity ")"))))))
  ;; Render the batch
  (.stroke ctx))

(defn compute-ps-accelerations
  [& [draw-acc?]]
  (when (aget mass 0)
    (doseq [^js p ps]
      (let [px (.. p -pos -x)
            py (.. p -pos -y)
            mx (aget mass 0)
            my (aget mass 1)
            deltax (- mx px)
            deltay (- my py)
            r (+ (* deltax deltax) (* deltay deltay))
            mass-m (* density @frame-count)
            acc-x (* mass-m  (/ (- mx px) r))
            acc-y (* mass-m  (/ (- my py) r))]
        (set! (.-acc p)
              #js {:x acc-x :y acc-y})
        (when draw-acc?
          (let [acc-m (/ (Math/sqrt (Math/pow acc-x 2) (Math/pow acc-y 2)) 20)]
            (c/draw-line ctx px
                         py
                         mx
                         my
                         {:c (str "rgba(255, 255, 255, " acc-m ")")
                          ;; :w (if (> acc-m 10) 20 1)
                          })))))))
(defn mouse-released [] (clean-mass))

(defn draw []
  (. ctx (clearRect 0 0 width height))
  (split-space)
  ;; (draw-lines)
  (draw-neighbors-lines)
  (doseq [^js p ps]
    (c/draw-circle ctx (.. p -pos -x) (.. p -pos -y) (.-radius p) "blue"))
  ;; tiene que estar antes de actualizar
  (compute-ps-accelerations)
  (draw-mass)
  (update-vs)
  (update-ps)
  #_(js/requestAnimationFrame draw)
  (let [fc @frame-count]
    (when (and (not (nil? fc)) (< fc 200))
      (swap! frame-count #(inc %)))))
