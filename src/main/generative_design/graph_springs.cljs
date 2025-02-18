;; http://www.generative-gestaltung.de/2/sketches/?02_M/M_6_1_02
(ns generative-design.graph-springs
  (:require [p5]))

(def node-diameter 16)
(def r (/ node-diameter 2))
(def node-count 30)
(def nodes #js [])
(def springs #js [])
(def selected-node (atom nil))


(defn make-spring
  [^Node from ^Node to & {:keys [length stiffness damping] :or {length 20 stiffness 1 damping 0.9}}]
  #js {:from from :to to :length length :stiffness stiffness :damping damping})

(defn update-spring
  [^js spring]
  (let [diff (.sub p5/Vector (.. spring -to -p) (.. spring -from -p))]
    (.normalize diff)
    (.mult diff (.-length spring))
    (let [target (.add p5/Vector (.. spring -from -p) diff)
          force (.sub p5/Vector target (.. spring -to -p))]
      (.mult force 0.5)
      (.mult force (.-stiffness spring))
      (.mult force (- 1 (.-damping spring)))
      (.add (.. spring -to -v) force)
      (.add (.. spring -from -v) (.mult p5/Vector force (- 1)))
      )))

(defn make-node [x y & {:keys [min-x min-y max-x max-y radius strength]
                        :or {min-x js/Number.MIN_VALUE
                             min-y js/Number.MIN_VALUE
                             max-x js/Number.MAX_VALUE
                             max-y js/Number.MAX_VALUE
                             radius 100
                             strength (- 5)
                             }}]
  #js {:p (js/createVector x y)
       :min-x min-x
       :min-y min-y
       :max-x max-x
       :max-y max-y
       :radius radius
       :ramp strength
       :strength strength
       :damping 0.5
       :v (js/createVector 0 0)
       :p-v (js/createVector 0 0)
       :max-v 10})


(defn init-nodes-and-springs!
  []
  (doseq [_ (range node-count)]
    (let [n (make-node (+ (/ js/width 2) (js/random -200 200))
                       (+ (/ js/height 2) (js/random -200 200))
                       {:min-x r :min-y r :max-x (- js/width r) :max-y (- js/height r)})]
      (.push nodes n)))
  (doseq [j (range (dec node-count))]
    (let [r-count (js/floor (js/random 1 3))]
      (doseq [_ (range r-count)]
        (let [r (js/floor (js/random (inc j) node-count))
              new-spring (make-spring (aget nodes j) (aget nodes r))]
          (.push springs new-spring))))))

(defn mouse-pressed
  []
  (loop [idx 0
         max-dist 20]
    (when (< idx node-count)
      (let [node (aget nodes idx)
            d (js/dist js/mouseX js/mouseY (.. node -p -x) (.. node -p -y))]
        (if (< d max-dist)
          (do
            (reset! selected-node node)
            (recur (inc idx) d))
          (recur (inc idx) max-dist))))))

(defn mouse-released
  []
  (reset! selected-node nil))

(defn attract-node
  [^js this ^js other]
  (let [this-node-vector (js/createVector (.. this -p -x) (.. this -p -y))
        other-node-vector (js/createVector (.. other -p -x) (.. other -p -y))
        d (.dist this-node-vector other-node-vector)]
    (when (and (> d 0) (< d (.-radius this)))
      (let [s (js/pow (/ d (.-radius this)) (/ 1 (.-ramp this)))
            f (/ (* s 9 (.-strength this) (+ (/ 1 (inc s)) (/ (- s 3) 4))) d)
            df (.sub this-node-vector other-node-vector)]
        ;; (.log js/console df)
        (.mult df f)
        (set! (.. other -v -x) (+ (.. other -v -x) (.-x df)))
        (set! (.. other -v -y) (+ (.. other -v -y) (.-y df)))))))

(defn nodes-attract
  [node]
  (loop [idx 0]
    (let [other (aget nodes idx)]
      (when-not (nil? other)
        (if (= node other)
          (recur (inc idx))
          (do (attract-node node other)
              (recur (inc idx))))))))

(defn update-node [^js node]
  (.limit (.-v node) (.-max-v node))
  (set! (.. node -p -x) (+ (.. node -p -x) (.. node -v -x)))
  (set! (.. node -p -y) (+ (.. node -p -y) (.. node -v -y)))

  (when (< (.. node -p -x) (.-min-x node))
    (set! (.. node -p -x)
          (- (.-min-x node) (- (.. node -p -x) (.-min-x node))))
    (set! (.. node -v -x) (- (.. node -v -x))))

  (when (> (.. node -p -x) (.-max-x node))
    (set! (.. node -p -x)
          (- (.-max-x node) (- (.. node -p -x) (.-max-x node))))
    (set! (.. node -v -x) (- (.. node -v -x))))

  (when (< (.. node -p -y) (.-min-y node))
    (set! (.. node -p -y)
          (- (.-min-y node) (- (.. node -p -y) (.-min-y node))))
    (set! (.. node -v -y) (- (.. node -v -y))))

  (when (> (.. node -p -y) (.-max-y node))
    (set! (.. node -p -y)
          (- (.-max-y node) (- (.. node -p -y) (.-max-y node))))
    (set! (.. node -v -y) (- (.. node -v -y))))

  (.mult (.-v node) (- 1 (.-damping node))))

(defn setup []
  (let [canvas (js/createCanvas js/windowWidth js/windowHeight)]
    (.parent canvas "p5jsparent")
    (js/noStroke)
    (init-nodes-and-springs!)))

(defn draw
  []
  (js/background 255)
  ;; >>> nodos se repelen
  (doseq [^js n nodes]
    (nodes-attract n))

  ;; >>> fuerzas de muelles
  (doseq [^js s springs]
    (update-spring s))

  ;; >>> aplicamos valores a los nodos
  (doseq [^js n nodes]
    (update-node n))

  ;; >>> movemos
  (when-not (nil? @selected-node)
    (set! (.. @selected-node -p -x) js/mouseX)
    (set! (.. @selected-node -p -y) js/mouseY))

  ;; >>> pintamos
  (js/stroke 0 130 164)
  (js/strokeWeight 2)
  (doseq [^js s springs]
    (let [fp (.. s -from -p)
          tp (.. s -to -p)]
      (js/line (.-x fp) (.-y fp) (.-x tp) (.-y tp))))

  (js/noStroke)
  (doseq [^js n nodes]
    (js/fill 255)
    (js/ellipse (.. n -p -x) (.. n -p -y) node-diameter node-diameter)
    (js/fill 0)
    (js/ellipse (.. n -p -x) (.. n -p -y) (- node-diameter 4) (- node-diameter 4))))
