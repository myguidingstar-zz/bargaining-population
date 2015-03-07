(ns bargaining-population.app
  (:require [rum :include-macros true]
            [bargaining-population.automaton :refer [pure-strategy]]
            [bargaining-population.cycle :refer [run-cycle]]
            [bargaining-population.match :refer [standard-deviation]]))

(def status (atom :stopped))

(def init (atom {:high 2
                 :medium 2
                 :low 2}))

(def config (atom {:rounds-per-match 3
                   :payoff-aggregator :mean
                   :discount-rate 0.5
                   :reproduction-size 2
                   :mutation-probability 0.5}))

(def population-cycles (atom []))

(def payoff-cycles (atom []))

(def payoff-sd-cycles (atom []))

(def max-payoff-sd (atom 0))

(def selected-cyle (atom 0))

(rum/defc init-population < rum/reactive []
  [:.ui.labeled.input
   (-> (fn [type]
         [[:.ui.label (name type)]
          [:input {:type "number" :min 0 :step 1
                   :value (str (get (rum/react init) type))
                   :on-change #(do (swap! init assoc type
                                          (-> % .-target .-value js/parseInt)) nil)}]])
       (mapcat [:high :medium :low]))])

(defn initialize [init]
  (mapcat (fn [[k v]] (repeat v (pure-strategy k)))
          init))

(defn population-size [init]
  (apply + (vals init)))

(defn update-cycles! [population payoffs]
  (let [payoff-sd (standard-deviation payoffs)]
    (swap! population-cycles #(conj % population))
    (swap! payoff-cycles #(conj % payoffs))
    (swap! payoff-sd-cycles #(conj % payoff-sd))
    (swap! max-payoff-sd #(max % payoff-sd))))

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
                                   (-> % .-target .-value js/parseInt)) nil)}]
   [:.ui.label "Mutation probability"]
   [:input {:type "number" :min 0 :max 1 :step 0.1
            :value (str (get (rum/react config) :mutation-probability))
            :on-change #(do (swap! config assoc :mutation-probability
                                   (-> % .-target .-value js/parseFloat)) nil)}]])

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
                                     (reset! payoff-sd-cycles [])
                                     (reset! max-payoff-sd 0)
                                     nil)}
     "Stop"]))

(rum/defc column < rum/reactive [i payoff-sd]
  (let [mps (rum/react max-payoff-sd)]
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
                 :height (str (* 100 (/ payoff-sd mps))
                              "%")}}]])))

(rum/defc chart < rum/reactive []
  [:div.chart {:style {:height (str "300px")}}
   (let [the-cycles (rum/react payoff-sd-cycles)]
     (for [i (range (count the-cycles))]
       (column i (nth the-cycles i))))])

(rum/defc inspector < rum/reactive []
  (when (< 0 (count (rum/react population-cycles)))
    [:div
     [:div (str "Payoffs: "
                (nth (rum/react payoff-cycles)
                     (rum/react selected-cyle)))]
     [:div (str "Standard deviation: "
                (nth (rum/react payoff-sd-cycles)
                     (rum/react selected-cyle)))]
     [:div (str "Population: ")
      (for [automaton (nth (rum/react population-cycles)
                           (rum/react selected-cyle))]
        [:div (pr-str automaton)])]]))

(rum/defc launch-board < rum/reactive []
  [:div
   (run)
   (pause-resume)
   (stop)
   (chart)
   (inspector)])

(let [app-root (.getElementById js/document "my-app")]
  (rum/mount (launch-board) app-root))
