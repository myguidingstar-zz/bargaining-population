(ns bargaining-population.mutation)

(defn nth-transition->target [fsm n target]
  (-> fsm
      (assoc-in [:state-transitions n 2] target)))

(defn random-transition->target [fsm target]

  (-> fsm
      (nth-transition->target (rand-int (count (:state-transitions fsm))) target)))

(defn append-new-state [fsm new-state-id]
  (-> fsm
      (update-in [:state-transitions]
                 #(into %
                        (map (fn [strat] [new-state-id strat new-state-id])

                             [:high :medium :low])))))

(defn add-state [{:keys [next-id state-ids] :as fsm}]
  (-> fsm
      (update-in [:state-ids] #(conj % next-id))
      (update-in [:next-id] inc)
      (update-in [:state-strategies]
                 #(conj % [next-id (rand-nth [:high :medium :low])]))
      (random-transition->target next-id)
      (append-new-state next-id)))

(defn remove-transitions-from [fsm id-to-remove]
  (-> fsm
      (update-in [:state-transitions]
                 #(vec (remove (fn [[from-id _ _]] (= from-id id-to-remove)) %)))))

(defn remove-transitions-to
  "Transitions will go back to their state of origin"
  [fsm id-to-remove]
  (-> fsm
      (update-in [:state-transitions]
                 (fn [transitions]
                   (-> (fn [transition]
                         (let [[from-id _ to-id] transition]
                           (if (= to-id id-to-remove)
                             [from-id _ from-id]
                             transition)))
                       (mapv transitions))))))

(defn remove-state* [fsm id-to-remove]
  (let [state-ids (disj (:state-ids fsm) id-to-remove)]
    (-> fsm
        (assoc :state-id (rand-nth (vec state-ids)))
        (assoc :state-ids state-ids)
        (update-in [:state-strategies] #(dissoc % id-to-remove))
        (remove-transitions-from id-to-remove)
        (remove-transitions-to id-to-remove))))

(defn remove-state [fsm]
  (if (< 1 (count (:state-ids fsm)))
    (let [id-to-remove (rand-nth (vec (:state-ids fsm)))]
      (remove-state* fsm id-to-remove))
    fsm))

(defn change-transition-target
  [{:keys [state-transitions state-ids] :as fsm}]
  (let [n (count state-transitions)]
    (if (= n 0)
      fsm
      (let [change-position (rand-int n)]
        (-> fsm
            (update-in [:state-transitions change-position]
                       (fn [fsm]
                         (let [[_ __ to-id] fsm
                               other-state-ids (disj state-ids to-id)]
                           (if (seq other-state-ids)
                             [_ __ (rand-nth (vec other-state-ids))]
                             fsm)))))))))

(defn mutate
  "Randomly mutate an automaton using one of three mutation functions."
  [fsm]
  (let [mutator (rand-nth [add-state remove-state change-transition-target])]
    (mutator fsm)))
