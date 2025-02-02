(ns generative-design.random-traces
  (:require [goog.object :as g]
            [p5]))

(defn make-node [x y min-x max-x min-y max-y]
  #js {:x x
       :y y
       :min-x min-x
       :max-x max-x
       :min-y min-y
       :max-y max-y
       :radius 200
       :ramp 1
       :strength -1
       :damping 0.5
       :velocity (js/createVector)
       :p-velocity (js/createVector)
       :max-velocity 10})

(def node-count 200)

(def nodes #js [])

(defn create-nodes []
  (doseq [_ (range node-count)]
    (.push nodes (make-node (+ (/ js/width 2) (js/random -1 1))
                            (+ (/ js/height 2) (js/random -1 1))
                            5
                            (- js/width 5)
                            5
                            (- js/height 5)))))

(defn update-node [^js node]
  (.limit (.-velocity node) (.-max-velocity node))
  (set! (.-x node) (+ (.-x node) (.. node -velocity -x)))
  (set! (.-y node) (+ (.-y node) (.. node -velocity -y)))

  (when (< (.-x node) (.-min-x node))
    (set! (.-x node) (- (.-min-x node)
                        (- (.-x node) (.-min-x node))))
    (set! (.. node -velocity -x) (- (.. node -velocity -x))))

  (when (> (.-x node) (.-max-x node))
    (set! (.-x node) (- (.-max-x node)
                        (- (.-x node) (.-max-x node))))
    (set! (.. node -velocity -x) (- (.. node -velocity -x))))

  (when (< (.-y node) (.-min-y node))
    (set! (.-y node) (- (.-min-y node)
                        (- (.-y node) (.-min-y node))))
    (set! (.. node -velocity -y) (- (.. node -velocity -y))))

  (when (> (.-y node) (.-max-y node))
    (set! (.-y node) (- (.-max-y node)
                        (- (.-y node) (.-max-y node))))
    (set! (.. node -velocity -y) (- (.. node -velocity -y))))

  (.mult (.-velocity node) (- 1 (.-damping node))))

(defn attract-node
  [this ^js that]
  (let [this-v (js/createVector (.-x this) (.-y this))
        that-v (js/createVector (.-x that) (.-y that))
        d (.dist this-v that-v)]
    (when (and (> d 0) (< d ^js(.-radius this)))
      (let [s (js/pow (/ d ^js(.-radius this)) (/ 1 ^js (.-ramp this)))
            f (/ (* 9 s ^js(.-strength this)
                    (+ (/ 1 (inc s)) (/ (- s 3) 4)))
                 d)
            df (.sub this-v that-v)]
        (.mult df f)
        (set! ^js(.. that -velocity -x) (+ ^js(.. that -velocity -x) (.-x df)))
        (set! ^js(.. that -velocity -y) (+ ^js(.. that -velocity -y) (.-y df)))))))

(defn attract-nodes [node nodes]
  (doseq [i (range node-count)]
    (let [other (aget nodes i)]
      (when (not= other node)
        (attract-node node other )))))

(defn setup []
  (let [canvas (js/createCanvas 800 600)]
    (.parent canvas "p5jsparent")
    (js/noStroke)
    (create-nodes)))

(defn draw []
  (js/fill 255 10)
  (js/rect 0 0 js/width js/height)
  (js/fill 0)

  (doseq [idx (range node-count)]
    (let [node (aget nodes idx)]
      (attract-nodes node nodes)
      (update-node node)
      (js/ellipse (.-x node) (.-y  node) 10 10))))
