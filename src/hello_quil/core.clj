(ns hello-quil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; xn+1 = r*xn * (1 - xn)
(defn logistic-map [^double r ^double itr]
  (let [x0 (rand)]
    (take (inc itr) (iterate #(* r % (- 1.0 %)) x0))))

;(def lmap (logistic-map 4.0 10000))
; since input (x) to a logistic map is just xn-1, 
; our xs will just be lmap[0:len(lmap)-2]
; and our ys will be lmap[1:len(lmap)-1]
;(def lmap-xs (drop-last lmap))
;(def lmap-ys (rest lmap))
;(def coords (partition 2 (interleave lmap-xs lmap-ys)))
;(def world-coords (map #(apply (partial * 100) %) coords))
;
(defn bifurcation-diagram []
  (let [rs (range 1 4 0.01)
        result-map {}
        itrs 5]
    (into {}
          (for [r rs]
            (assoc result-map r (set (logistic-map r itrs)))))))

(def bd (bifurcation-diagram))

(defn draw-plot []
  (let [r-inc-amt (/ (q/frame-count) 100.0)
        max-r 4.0
        r (+ 1.0 r-inc-amt)
        itrs 10
        lmap (if (< r max-r) (logistic-map r itrs) [])]
   (doseq [x lmap]
     (let [scaledx (* x 100)
           invertedx (- (q/height) scaledx)
           scaledr (* r 100)]
       (q/point scaledr invertedx)))))

(defn setup []
  ;set fps to 30
  (q/frame-rate 30)
  (q/color-mode :hsb)
  ;initial state
  {:color 0
   :bifurcation-diagram bd})

(defn update-state [state]
  {:color (mod (+ (:color state) 0.7) 255)
   :bifurcation-diagram (:bifurcation-diagram state)})

(defn draw-state [state]
  ; clear sketch
  (q/background 180)
  ; set color
  (q/fill (:color state) 255 255)
  (draw-plot))

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
