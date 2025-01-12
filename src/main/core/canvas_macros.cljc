(ns core.canvas-macros)

(defmacro circle
  [ctx x y r color]
  `(do
     (. ~ctx beginPath)
     (set! (.-fillStyle ~ctx) (name ~color))
     (. ~ctx (arc ~x ~y ~r 0.0 (* 2.0 js/Math.PI)))
     (. ~ctx closePath)
     (. ~ctx fill)))


(defmacro line
  [ctx x0 y0 x1 y1 & {:keys [w c] :or {w 1 c "rgba(255, 255, 255, 1"}}]
  (.beginPath ctx)
  (.moveTo ctx x0 y0)
  (.lineTo ctx x1 y1)
  (set! (.-lineWidth ctx) w)
  (set! (.-strokeStyle ctx) c)
  (.stroke ctx))
