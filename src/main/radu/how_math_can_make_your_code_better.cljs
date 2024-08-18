;; Demasiado mix entre `record`, `atom` y `#js`. Debería haberlo hecho todo con `#js`.
;; polyrythm
(ns radu.how-math-can-make-your-code-better
  (:require [goog.object :as g]
            [core.vector :as v]
            [core.canvas :as c]))

(def canvas (.getElementById js/document "whiteboard"))
(def ctx (.getContext canvas "2d"))
(def height (- (.-innerHeight js/window) 100))
(def width (- (.-innerWidth  js/window) 100))
(def size 1000)
(def full-circ (* 2.0 Math/PI))
(def audio-ctx (new js/window.AudioContext))
(def track-min-radius 100)
(def track-step 15)
(def tracks (atom []))
(def balls (atom []))
(def N 20)
(def ball-radius 10)
(def ball-min-speed 0.01)
(def ball-speed-step -0.0001)
(def sound-frequencies [1760 1567.98 1396.91 1318.51 1174.66 1046.5 987.77 880 783.99 698.46
                        659.25 587.33 523.25 493.88 440 392 349.23 329.63 293.66 261.63])

(set! (.-height canvas) size)
(set! (.-width canvas) size)
(set! (.. canvas -style -backgroundColor) "black") ;; igual ;; (set! (.-backgroundColor (.-style canvas)) "black")

(defn play-sound
  [& {:keys [frequency duration] :or {frequency 400 duration 2}}]
  (let [osc (. audio-ctx createOscillator)
        envelope (. audio-ctx createGain)]
    (do
      (. osc (connect envelope))
      ;; Esto de `envelope` es para que el sonido sea más suave, que desaparezca de forma lineal.
      (. envelope (connect (.-destination audio-ctx)))
      (. (. envelope -gain) (setValueAtTime 0 (. audio-ctx -currentTime)))
      (. (.-gain envelope) (linearRampToValueAtTime 0.05 (+ (.-currentTime audio-ctx) 0.05))) ;; 0.05 es fracción de segundo
      (. (.-gain envelope) (linearRampToValueAtTime 0 (+ (.-currentTime audio-ctx) duration)))
      (. (.-frequency osc) (setValueAtTime frequency (.-currentTime audio-ctx)))
      (. osc (start))
      (. osc (stop (+ duration (.-currentTime audio-ctx)))))))

(defrecord Track [center radius period])
;; necesito objeto de #js para setear
(defrecord Ball [track radius speed offset center round sound-frequency])

(defn get-track-position
  [track offset]
  #js {:x (+ (.. track -center -x) (* (Math/cos offset) ^number (.-radius track)))
       :y (- (.. track -center -y) (* (Math/sin offset) ^number (.-radius track)))
       :round (Math/floor (/ offset (:period track)))})

(defn draw-track
  [ctx track]
  (let [{:keys [center radius]} track]
    (. ctx (beginPath))
    (doseq [a (range 0 full-circ 0.1)]
      (let [pos (get-track-position track a)]
        (. ctx (lineTo (.-x pos) (.-y pos)))))
    ;; Más sencillo pero menos flexible.
    ;; (. ctx (arc (.-x center) (.-y center) radius 0 full-circ))
    (. ctx (closePath))
    (set! (.-strokeStyle ctx) "white")
    (. ctx (stroke))))

(defn draw-ball
  [ctx ball]
  (let [{:keys [center radius]} ball]
    (. ctx (beginPath))
    (. ctx (arc (.-x center) (.-y center) radius 0 full-circ))
    (. ctx (closePath))
    (set! (.-strokeStyle ctx) "white")
    (. ctx (stroke))))

(defn move-ball
  [ctx {:keys [track radius speed offset center round sound-frequency]}]
  (let [new-offset (+ offset speed)
        res (get-track-position track new-offset)
        new-round (if (not= (.-round res) round)
                    (.-round res)
                    round)]
    (when (not= round new-round)
      (play-sound {:frequency sound-frequency}))
    (map->Ball
     {:track track
      :radius radius
      :speed speed
      :offset new-offset
      :center res
      :round new-round
      :sound-frequency sound-frequency})))

(defn animate
  [ctx tracks balls]
  (. ctx (clearRect 0 0 size size))
  (doseq [track @tracks]
    (draw-track ctx track))
  (let [new-balls (map #(move-ball ctx %) @balls)]
    (reset! balls new-balls))
  (doseq [ball @balls]
    (draw-ball ctx ball)))

(defn setup []
  (doseq [i (range N)]
    (let [track-radius (+ track-min-radius (* i track-step))
          ball-speed (+ ball-min-speed (* i ball-speed-step))
          f (nth sound-frequencies i)
          t (->Track #js {:x (/ size 2) :y (/ size 2)} track-radius Math/PI)
          b (map->Ball {:track t
                        :radius ball-radius
                        :speed ball-speed
                        :offset 0
                        :center (get-track-position t 0)
                        :round 0
                        :sound-frequency f})]
      (swap! tracks conj t)
      (swap! balls conj b))))

(defn draw []
  (animate ctx tracks balls))
