(ns bargaining-population.cycle
  (:require-macros [lonocloud.synthread :as ->])
  (:require [bargaining-population.automaton
             :refer [initial-automaton random-true-by-probability
                     randomize-by-frequency-map aggregate-payoff-by-type]]
            [bargaining-population.match
             :refer [match-results mean present-value-sum]]))

(defn match-phase
  ""
  [population rounds-per-match payoff-aggregator]
  (let [n (/ (count population) 2)
        results (map #(match-results % rounds-per-match)
                     (partition 2 population))
        new-population (mapcat :automaton-pair results)
        payoff-seqs (mapcat :payoff-seq-pair results)]
    [new-population (map payoff-aggregator payoff-seqs)]))

(defn reproduction-phase
  "By convention, first five automata are killed and replaced with new generated ones.
  `payoff-record` is a list of payoffs each of which can be either the avarge or present value of payoff sequence of a single automaton in the last match.
  the order of `payoff-record` is that of `population`."
  [[population payoff-record] reproduction-size]
  (let [new-born (repeatedly reproduction-size #(->> payoff-record
                                                     (map inc)
                                                     (aggregate-payoff-by-type population)
                                                     randomize-by-frequency-map
                                                     initial-automaton))]
    (concat new-born (drop reproduction-size population))))

(defn run-cycle
  [{:keys [rounds-per-match payoff-aggregator
           discount-rate reproduction-size mutation-probability]}]
  (let [payoff-aggregator (if (and (= :present-value payoff-aggregator)
                                   (number? discount-rate))
                            #(present-value-sum discount-rate %)
                            #(mean %))]
    (fn [[population _]]
      (let [after-matches
            (match-phase (shuffle population) rounds-per-match payoff-aggregator)
            payoffs (second after-matches)]
        [(-> after-matches
             (reproduction-phase reproduction-size))
         payoffs]))))

(enable-console-print!)
