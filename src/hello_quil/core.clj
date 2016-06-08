(ns hello-quil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; xn+1 = r*xn * (1 - xn)
(defn logistic-map [^double r ^double itr]
  (let [x0 (rand)]
    (take (inc itr) (iterate #(* r % (- 1.0 %)) x0))))

(defn bifurcation-diagram [& {:keys [begin end inc-amt itrs-per-lmap] 
                              :or 
                              {begin 1.0 
                               end 4.0 
                               inc-amt 0.01 
                               itrs-per-lmap 1000}}]
  (let [rs (range begin end inc-amt)]
    (apply hash-map
           (->> rs
                (map (fn [r] (set (logistic-map r itrs-per-lmap))))
                (interleave rs)))))

(def bd (bifurcation-diagram))

; draws bifurcation diagram one step at a time, clearing previous steps
(defn draw-plot-1 []
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

; draws bifurcation diagram one step at a time, keeping previous steps
(defn draw-plot-2 []
  (let [frame-count (q/frame-count)
        r-inc-amt (/ frame-count 100.0) ; => r moves in 0.01s
        max-r 4.0
        r (+ 1.0 r-inc-amt)
        rg (range 1.0 r 0.01)
        lmaps (select-keys bd rg)]
    (doseq [lmap lmaps]
      (let [r (first lmap)
            xs (second lmap)]
       (doseq [x xs] 
         (let [scaledx (* x 100)
               invertedx (- (q/height) scaledx)
               scaledr (* r 100)]
           (q/point scaledr scaledx)))))))

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
  (draw-plot-2))

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
