(ns bargaining-population.app
  (:require [rum :include-macros true]
            [bargaining-population.automaton :refer [initial-automaton]]
            [bargaining-population.cycle :refer [run-cycle]]
            [bargaining-population.match :refer [mean]]))

(def status (atom :stopped))

(def init (atom {:high 2
                 :medium 2
                 :low 2
                 :accommodator 2}))

(def config (atom {:rounds-per-match 3
                   :payoff-aggregator :mean
                   :discount-rate 0.5
                   :reproduction-size 2}))

(def population-cycles (atom []))

(def payoff-cycles (atom []))

(def payoff-mean-cycles (atom []))

(def max-payoff-mean (atom 0))

(def selected-cyle (atom 0))

(rum/defc init-population < rum/reactive []
  [:.ui.labeled.input
   (-> (fn [type]
         [[:.ui.label (name type)]
          [:input {:type "number" :min 0 :step 1
                   :value (str (get (rum/react init) type))
                   :on-change #(do (swap! init assoc type
                                          (-> % .-target .-value js/parseInt)) nil)}]])
       (mapcat [:high :medium :low :accommodator]))])

(defn initialize [init]
  (mapcat (fn [[k v]] (repeat v (initial-automaton k)))
          init))

(defn population-size [init]
  (apply + (vals init)))

(defn update-cycles! [population payoffs]
  (let [payoff-mean (mean payoffs)]
    (swap! population-cycles #(conj % population))
    (swap! payoff-cycles #(conj % payoffs))
    (swap! payoff-mean-cycles #(conj % payoff-mean))
    (swap! max-payoff-mean #(max % payoff-mean))))

(defn init! []
  (let [[population payoffs]
        ((run-cycle @config) [(initialize @init) nil])]
    (update-cycles! population payoffs)))

(defn next-cycle! []
  (let [[population payoffs]
        ((run-cycle @config) [(last @population-cycles) (last @payoff-cycles)])]
    (update-cycles! population payoffs)))

(defn forever! []
  (js/setTimeout #(do (when (= :running @status)
                        (next-cycle!))
                      (forever!))
                 100))

(rum/defc config-cycle < rum/reactive []
  [:.ui.labeled.input
   [:.ui.label "Rounds per match"]
   [:input {:type "number" :min 1 :step 1
            :value (str (get (rum/react config) :rounds-per-match))
            :on-change #(do (swap! config assoc :rounds-per-match
                                   (-> % .-target .-value js/parseInt)) nil)}]
   [:.ui.label "Reproduction size"]
   [:input {:type "number" :min 0 :max (population-size (rum/react init)) :step 1
            :value (str (get (rum/react config) :reproduction-size))
            :on-change #(do (swap! config assoc :reproduction-size
                                   (-> % .-target .-value js/parseInt)) nil)}]])

(rum/defc config-cycle-aggregator < rum/reactive []
  [:.ui.form
   [:.grouped.inline.fields
    [:.ui.label "Payoff aggregator"]
    [:.field
     [:.ui.radio.checkbox
      {:on-click
       #(do (swap! config assoc :payoff-aggregator :mean)
            nil)}
      [:input {:type "radio"
               :checked (= (:payoff-aggregator (rum/react config))
                           :mean)}]
      [:label "Mean"]]
     [:.ui.radio.checkbox
      {:on-click
       #(do (swap! config assoc :payoff-aggregator :present-value)
            nil)}
      [:input {:type "radio"
               :checked (= (:payoff-aggregator (rum/react config))
                           :present-value)}]
      [:label "Discount"]]]]
   (when (= (:payoff-aggregator (rum/react config))
            :present-value)
     [:.ui.labeled.input
      [:.ui.label "Discount rate"]
      [:input {:type "number" :min 0 :max 1 :step 0.1
               :value (str (get (rum/react config) :discount-rate))
               :on-change #(do (swap! config assoc :discount-rate
                                      (-> % .-target .-value js/parseFloat)) nil)}]])])

(rum/defc run < rum/reactive []
  (when (= :stopped (rum/react status))
    [:div
     (init-population)
     (config-cycle)
     (config-cycle-aggregator)
     [:.ui.green.button
      {:on-click #(do (reset! status :running)
                      (init!)
                      (forever!)
                      nil)}
      "Run"]]))

(rum/defc pause-resume < rum/reactive []
  (case (rum/react status)
    :running
    [:.ui.yellow.button {:on-click #(do (reset! status :paused) nil)}
     "Pause"]
    :paused
    [:.ui.teal.button {:on-click #(do (reset! status :running)
                                      (next-cycle!)
                                      nil)}
     "Resume"]
    nil))

(rum/defc stop < rum/reactive []
  (when-not (= :stopped (rum/react status))
    [:.ui.red.button {:on-click #(do (reset! status :stopped)
                                     (reset! population-cycles [])
                                     (reset! payoff-cycles [])
                                     (reset! payoff-mean-cycles [])
                                     (reset! max-payoff-mean 0)
                                     nil)}
     "Stop"]))

(rum/defc column < rum/reactive [i payoff-mean]
  (let [mps (rum/react max-payoff-mean)]
    (when (< 0 mps)
      [:div.column {:key i
                    :style {:background-color
                            (if (= i (rum/react selected-cyle))
                              "yellow" "#eee")}
                    :on-click #(reset! selected-cyle i)}
       [:div.item
        {:style {:background-color
                 (if (= i (rum/react selected-cyle))
                   "red" "#369")
                 :height (str (* 100 (/ payoff-mean mps))
                              "%")}}]])))

(rum/defc chart < rum/reactive []
  [:div.chart {:style {:height (str "300px")}}
   (let [the-cycles (rum/react payoff-mean-cycles)]
     (for [i (range (count the-cycles))]
       (column i (nth the-cycles i))))])

(rum/defc inspector < rum/reactive []
  (when (< 0 (count (rum/react population-cycles)))
    [:div
     [:div (str "Payoffs: "
                (nth (rum/react payoff-cycles)
                     (rum/react selected-cyle)))]
     [:div (str "Payoff mean: "
                (nth (rum/react payoff-mean-cycles)
                     (rum/react selected-cyle)))]
     [:div (str "Population: ")
      (let [automata (nth (rum/react population-cycles)
                           (rum/react selected-cyle))]
        [:div
         (str (-> (fn [acc automaton] (update-in acc [(:name automaton)] inc))
                  (reduce {} automata)))])]]))

(rum/defc launch-board < rum/reactive []
  [:div
   (run)
   (pause-resume)
   (stop)
   (chart)
   (inspector)])

(let [app-root (.getElementById js/document "my-app")]
  (rum/mount (launch-board) app-root))
