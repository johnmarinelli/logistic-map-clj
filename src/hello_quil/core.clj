(ns hello-quil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; xn+1 = r*xn * (1 - xn)
(defn logistic-map [^double r ^double itr]
  (let [x0 (rand)]
    (take (inc itr) (iterate #(* r % (- 1.0 %)) x0))))

(def lmap (logistic-map 4.0 10000))
; since input (x) to a logistic map is just xn-1, 
; our xs will just be lmap[0:len(lmap)-2]
; and our ys will be lmap[1:len(lmap)-1]
(def lmap-xs (drop-last lmap))
(def lmap-ys (rest lmap))
(def coords (partition 2 (interleave lmap-xs lmap-ys)))
(def world-coords (map #(* 10 %) coords))

(println world-coords)

(defn draw-plot [coords]
  (println (take 5 coords))
  (println (take 5 (partition 2 coords)))
  (doseq [coord-pair (partition 2 coords)]
    (println coord-pair)
    (apply q/line coord-pair)))

(defn setup []
  ;set fps to 30
  (q/frame-rate 30)
  (q/color-mode :hsb)
  ;initial state
  {:color 0
   :coords world-coords})

(defn update-state [state]
  {:color (mod (+ (:color state) 0.7) 255)
   :coords (:coords state)})

(defn draw-state [state]
  ; clear sketch
  (q/background 180)
  ; set color
  (q/fill (:color state) 255 255)
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)])
  (println (:coords state)))

(comment(defn setup []
                                        ; Set frame rate to 30 frames per second.
   (q/frame-rate 30)
                                        ; Set color mode to HSB (HSV) instead of default RGB.
   (q/color-mode :hsb)
                                        ; setup function returns initial state. It contains
                                        ; circle color and position.
   {:color 0
    :angle 0}))

(comment(defn update-state [state]
                                        ; Update sketch state by changing circle color and position.
   {:color (mod (+ (:color state) 0.7) 255)
    :angle (+ (:angle state) 0.1)}))

(comment(defn draw-state [state]
                                        ; Clear the sketch by filling it with light-grey color.
   (q/background 128)
                                        ; Set circle color.
   (println state)
   (q/fill (:color state) 255 255)
                                        ; Calculate x and y coordinates of the circle.
   (let [angle (:angle state)
         x (* 150 (q/cos angle))
         y (* 150 (q/sin angle))]
                                        ; Move origin point to the center of the sketch.
     (q/with-translation [(/ (q/width) 2)
                          (/ (q/height) 2)]
                                        ; Draw the circle.
       (q/ellipse x y 100 100)))))

(q/defsketch hello-quil
  :title "You spin my circle right round"
  :size [500 500]
                                        ; setup function called only once, during sketch initialization.
  :setup setup
                                        ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
  :middleware [m/fun-mode])

(defn -main [] ())
