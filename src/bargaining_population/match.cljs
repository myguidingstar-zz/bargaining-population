(ns bargaining-population.match
  (:require [bargaining-population.automaton :refer [fsm->strategy transit-fsm]]))

(def claim-values
  {:high 8
   :medium 5
   :low 2})

(defn claims->payoffs
  "Returns a pair of payoffs according to given claims."
  [pair-of-claims]
  (let [[p1 p2] (map claim-values pair-of-claims)]
    (if (<= (+ p1 p2) 10)
      [p1 p2]
      [0 0])))

(defn match->payoffs
  "Returns a pair of payoffs for an automaton pair considering what
  they should claim when they met."
  [fsm-pair]
  (claims->payoffs (map fsm->strategy fsm-pair)))

(defn transit-automata
  "Transits a pair of auttomata to their new states as they react to
  each other during a match."
  [[fsm1 fsm2]]
  (let [s1 (fsm->strategy fsm1)
        s2 (fsm->strategy fsm2)
        fsm1' (transit-fsm fsm1 s2)
        fsm2' (transit-fsm fsm2 s1)]
    [fsm1' fsm2']))

(defn inspect-matches
  "Runs automata through an infinte number of matches and collects
  full results as *sequence of payoff pairs* and *automaton pairs* (as
  they've changed states) in each match. Use it to inspect
  matches."
  [fsm-pair]
  (drop 1 (-> (fn [[payoff-seq fsm-pair]]
                [(match->payoffs fsm-pair)
                 (transit-automata fsm-pair)])
              (iterate [[] fsm-pair]))))

(defn match-results
  "Runs an automaton pair through a finte number of matches. Returns
  the final *automaton pair* and *payoff sequence pair* collected in
  each match."
  [fsm-pair rounds-per-match]
  (loop [fsm-pair fsm-pair
         payoff-seq-1 []
         payoff-seq-2 []
         round rounds-per-match]
    (if (zero? round)
      [fsm-pair [payoff-seq-1 payoff-seq-2]]
      (let [[payoff-1 payoff-2] (match->payoffs fsm-pair)]
        (recur (transit-automata fsm-pair)
               (conj payoff-seq-1 payoff-1)
               (conj payoff-seq-2 payoff-2)
               (dec round))))))

(defn present-values
  "Calculates present value for a (maybe lazy) sequence of payoffs."
  [discount-rate payoff-seq]
  (->> payoff-seq
       (reductions (fn [[last-discount _] payoff]
                     [(* discount-rate last-discount) (* last-discount payoff)])
                   [1 ()])
       (drop 1)
       (map second)))

(defn present-value-sum
  "Sum of present values for a payoff sequence."
  [discount-rate payoff-seq]
  (apply + (present-values discount-rate payoff-seq)))

(defn mean [population]
  (/ (apply + population) (count population)))

(defn pow [x y]
  (.pow js/Math x y))

(defn sqrt [x]
  (.sqrt js/Math x))

(defn standard-deviation [population]
  (let [m (mean population)
        n (count population)]
    (sqrt (/ (->> population
                  (map #(pow (- % m) 2))
                  (apply +))
             n))))
