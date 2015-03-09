(ns bargaining-population.automaton)

(def initial-automaton
  {:high         {:strategy :high
                  :accommodator? false}
   :medium       {:strategy :medium
                  :accommodator? false}
   :low          {:strategy :low
                  :accommodator? false}
   :accommodator {:strategy :medium
                  :accommodator? true}})

(defn automaton-name
  [{:keys [strategy accommodator?]}]
  (if accommodator?
    :accommodator
    strategy))

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

(defn randomize-by-frequency-map
  [frequency-map]
  (let [frequencies (vals frequency-map)
        ids (keys frequency-map)
        random-id
        (if (every? zero? frequencies)
          (rand-int (count frequencies))
          (let [thresholds (reductions + 0 frequencies)
                random (rand (last thresholds))]
            (some #(and (<= (nth thresholds %) random)
                        (< random (nth thresholds (inc %)))
                        %)
                  (range (count frequencies)))))]
    (nth ids random-id)))

(defn random-true-by-probability [p]
  (zero? (randomize-by-frequencies [p (- 1 p)])))

(defn aggregate-payoff-by-type* [population payoff-record]
  (let [n (count population)]
    (reduce (fn [acc i]
              (let [type (:name (nth population i))]
                (-> acc
                    (update-in [type :total] #(+ % (nth payoff-record i)))
                    (update-in [type :count] inc))))
            {:all-high {:total 0
                        :count 0}
             :all-medium {:total 0
                          :count 0}
             :all-low {:total 0
                       :count 0}
             :accommodator {:total 0
                            :count 0}}
            (range n))))

(defn aggregate-payoff-by-type [population payoff-record]
  (->> (aggregate-payoff-by-type* population payoff-record)
       (reduce (fn [acc [k v]]
                 (->> (if (zero? (:count v))
                        0
                        (/ (:total v) (:count v)))
                      (vector k)
                      (conj acc)))
               {})))
