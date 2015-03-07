(ns bargaining-population.automaton)

(defn pure-strategy [strategy]
  {:name (keyword (str "all-" (name strategy)))
   :state-id 1
   :state-ids #{1}
   :state-strategies {1 strategy}
   :state-transitions [[1 :high 1]
                       [1 :medium 1]
                       [1 :low 1]]})

(def accumulator
  {:name :accumulator
   :state-id 1
   :state-ids #{1 2 3}
   :state-strategies {1 :high
                      2 :medium
                      3 :low}
   :state-transitions [[1 :high 3]
                       [1 :medium 2]
                       [1 :low 1]
                       [2 :high 3]
                       [2 :medium 2]
                       [2 :low 1]
                       [3 :high 3]
                       [3 :medium 2]
                       [3 :low 1]]})

(defn fsm->strategy [fsm]
  (get (-> fsm :state-strategies)
       (-> fsm :state-id)))

(defn transit-state [{:keys [state-id state-transitions] :as fsm} opponent-state]
  (first
   (for [[from os to] state-transitions
         :when (and (= from state-id)
                    (= os opponent-state))]
     to)))

(defn transit-fsm [fsm opponent-state]
  (assoc fsm :state-id (transit-state fsm opponent-state)))


(defn randomize-by-frequencies
  [frequencies]
  (if (every? zero? frequencies)
    (rand-int (count frequencies))
    (let [thresholds (reductions + 0 frequencies)
          random (rand (last thresholds))]
      (some #(and (<= (nth thresholds %) random)
                  (< random (nth thresholds (inc %)))
                  %)
            (range (count frequencies))))))

(defn random-true-by-probability [p]
  (zero? (randomize-by-frequencies [p (- 1 p)])))
