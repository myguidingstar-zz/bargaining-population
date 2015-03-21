(ns bargaining-population.utils
  (:require [bargaining-population.automaton :refer
             [initial-automaton]]))

(defn initialize-population [init]
  (-> (fn [[k v]] (repeat v (initial-automaton k)))
      (mapcat init)
      shuffle))

(defn population-size [init]
  (apply + (vals init)))

(defn list->points
  "Generates a list of points that can be fed to chart.
  Arguments:

  - x-step: constant distance between two adjacent points' x values.
  - height: the height of the chart.
  - ys: list of main values, each of which is between 0 and 1."
  [x-step height ys]
  (-> #(vector (* x-step (inc %1)) (- height (* height %2)))
      (map-indexed ys)))

(defn true-keys
  "Finds keys in a hash-map where associated value is `true`."
  [m]
  (reduce-kv (fn [l k v] (if v (conj l k) l)) [] m))

(defn rates->points
  "Generates a list of points that can be fed to population-type-rate-chart.

  Arguments:

  - size: the height/width of the chart.

  - rates: list of [x, y] values, each of which is between 0 and 1."
  [size rates]
  (-> (fn [[x y]] (vector (* size x) (- size (* size y))))
      (map rates)))
