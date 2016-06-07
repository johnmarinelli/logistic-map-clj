(ns logistic-map.core
  (:require [common.db]
            [clojure.java.io :as io]            
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.datasets :refer :all]
            [incanter.io :refer :all]))

; defs for pipelining
(defn tell [in] (println (.toString in)) in)

(defn logistic-map [^double r]
  (let [x (rand)] 
    (iterate #(* r %  (- 1.0 %)) x)))

(defn lmap [] (logistic-map 4.0))
(defn lmap-xs [lmap] (drop-last (lmap)))
(defn lmap-ys [lmap] (rest (lmap)))

; so we can do this
(-> lmap lmap-xs (partial take 5) tell)


