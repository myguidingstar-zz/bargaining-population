(ns bargaining-population.automaton)

(defn pure-strategy [strategy]
  {:state-id 1
   :next-id 2
   :state-ids #{1}
   :state-strategies {1 strategy}
   :state-transitions [[1 :high 1]
                       [1 :medium 1]
                       [1 :low 1]]})

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
