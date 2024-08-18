(ns challenges.challenge-103-fire-effect
  (:require [p5]))


(def w 300)
(def h 200)
;; (def cols 600)
;; (def rows 400)

(def buffer1 (volatile! nil))
(def buffer2 (volatile! nil))
(def cooling (volatile! nil))
(def y-start (volatile! 0.0))

(defn mouse-pressed []
  (.fill @buffer1 255)
  (.noStroke @buffer1)
  (.ellipse @buffer1 js/mouseX js/mouseY 100 100)
  )

(defn setup []
  (js/pixelDensity 1)
  (js/createCanvas (* 2 w) h)

  (vreset! buffer1 (js/createGraphics w h))
  (vreset! buffer2 (js/createGraphics w h))
  (vreset! cooling (js/createImage w h)))

(defn cool []
  (.loadPixels @cooling)
  (let [increment 0.02]
    (doseq [x (range w)
            y (range h)]
      (let [xoff (* (inc x) increment)
            yoff (+ @y-start (* (inc y) increment))
            n (js/noise xoff yoff)
            bright (* (js/pow n 3) 255)
            index (* 4 (+ x (* y w)))]
        (aset (.-pixels @cooling) (+ index 0) bright)
        (aset (.-pixels @cooling) (+ index 1) bright)
        (aset (.-pixels @cooling) (+ index 2) bright)
        (aset (.-pixels @cooling) (+ index 3) 255)))
    (.updatePixels @cooling)
    (vswap! y-start (fn [v] (+ v increment))))
  )

(defn fire [rows]
  (.loadPixels @buffer1)
  (doseq [x (range w)
          j (range rows)]
    (let [y (- h (inc j))
          index (* 4 (+ x (* y w)))]
      (aset (.-pixels @buffer1) (+ index 0) 255)
      (aset (.-pixels @buffer1) (+ index 1) 255)
      (aset (.-pixels @buffer1) (+ index 2) 255)
      (aset (.-pixels @buffer1) (+ index 3) 255)
      ))
  (.updatePixels @buffer1))


(defn draw []
  (fire 2)

  (when js/mouseIsPressed
    (.fill @buffer1 255)
    (.noStroke @buffer1)
    (.ellipse @buffer1 js/mouseX js/mouseY 100 100))

  (cool)
  (js/background 0)
  (.loadPixels @buffer1)
  (.loadPixels @buffer2)

  (doseq [x (range 1 (dec w))
          y (range 1 (dec h))]
    (let [yw (* y w)
          index0 (* 4 (+ x yw))
          index1 (* 4 (+ (inc x) yw))
          index2 (* 4 (+ (dec x) yw))
          index3 (* 4 (+ x (* (inc y) w)))
          index4 (* 4 (+ x (* (dec y) w)))
          c1 (aget (.-pixels @buffer1) index1)
          c2 (aget (.-pixels @buffer1) index2)
          c3 (aget (.-pixels @buffer1) index3)
          c4 (aget (.-pixels @buffer1) index4)
          c5 (aget (.-pixels @cooling) index0)
          new-c (- (* 0.25 (+ c1 c2 c3 c4)) c5)]
      (aset (.-pixels @buffer2) (+ index4 0) new-c)
      (aset (.-pixels @buffer2) (+ index4 1) new-c)
      (aset (.-pixels @buffer2) (+ index4 2) new-c)
      (aset (.-pixels @buffer2) (+ index4 3) 255)
      ))
  (.updatePixels @buffer2)

  (let [tmp @buffer1]
    (vreset! buffer1 @buffer2)
    (vreset! buffer2 tmp))


  (js/image @buffer2 0 0)
  (js/image @cooling w 0)




  )

(comment
  (for [i (range 10)
        j (range 10)]
    (let [f (* i j)]
      [f i j]))
  )
