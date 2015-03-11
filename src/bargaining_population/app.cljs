(ns bargaining-population.app
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [rum :include-macros true]
            [bargaining-population.automaton :refer
             [initial-automaton automaton-name]]
            [bargaining-population.cycle :refer [run-cycle]]
            [bargaining-population.match :refer [mean]]
            [cljs.core.async
             :refer [mix admix toggle chan <! >! timeout]]))

(def status (atom :stopped))

(def init (atom {:high 99
                 :medium 0
                 :low 1
                 :accommodator 0}))

(def config (atom {:rounds-per-match 1
                   :payoff-aggregator :mean
                   :discount-rate 0.5
                   :reproduction-size 5}))

(def ^{:doc "`population-cycles`'s length is always one more other
cycles'. The last cycle is the one that will be fed to the next
`run-cycle`."}
  population-cycles (atom []))

(def payoff-cycles (atom []))

(def payoff-mean-cycles (atom []))

(def max-payoff-mean 8)

(def selected-cyle (atom 0))

(defn previous-selection! []
  (when (< 0 @selected-cyle)
    (swap! selected-cyle dec)))

(defn next-selection! []
  (when (< (inc @selected-cyle) (count @payoff-cycles))
    (swap! selected-cyle inc)))

(defn keyboard-navigate [event]
  (case (.-keyCode event)
    37 (previous-selection!)
    39 (next-selection!)
    nil))

(.addEventListener js/window "keydown" keyboard-navigate)

(rum/defc init-population < rum/reactive []
  [:.ui.labeled.input
   (-> (fn [type]
         [[:.ui.label (name type)]
          [:input {:type "number" :min 0 :step 1
                   :value (str (get (rum/react init) type))
                   :on-change #(do (swap! init assoc type
                                          (-> % .-target .-value js/parseInt)) nil)}]])
       (mapcat [:high :medium :low :accommodator]))])

(defn initialize-population [init]
  (-> (fn [[k v]] (repeat v (initial-automaton k)))
      (mapcat init)
      shuffle))

(defn population-size [init]
  (apply + (vals init)))

(def computation-output-channel (chan 100))

;; ui-update-queue's content is exactly the same as that of
;; computation-output-channel but with a mixer in the middle so be
;; data flowing from computation-output-channel can be paused on the
;; fly
(def ui-update-queue (chan))
(def mixer (mix ui-update-queue))

(defn update-cycles!
  "Appends run-cycle results to relevant atoms."
  [payoffs population-after]
  (let [payoff-mean (mean payoffs)]
    (swap! payoff-cycles #(conj % payoffs))
    (swap! payoff-mean-cycles #(conj % payoff-mean))
    (swap! population-cycles #(conj % population-after))))

(defn start-worker [initial-population]
  (go (loop [population initial-population]
        (<! (timeout 1))
        (let [{:keys [population] :as data} ((run-cycle @config) population)]
          (>! computation-output-channel data)
          (recur population))))
  (go (loop []
        (let [{:keys [population payoffs]} (<! ui-update-queue)]
          (update-cycles! payoffs population))
        (recur))))

(defn pause []
  (reset! status :paused)
  (toggle mixer {computation-output-channel {:mute false
                                             :pause true}})
  nil)

(defn resume []
  (reset! status :running)
  (toggle mixer {computation-output-channel {:mute false
                                             :pause false}})
  nil)

(defn stop []
  (reset! status :stopped)
  (toggle mixer {computation-output-channel {:mute true
                                             :pause false}})
  (reset! population-cycles [])
  (reset! payoff-cycles [])
  (reset! payoff-mean-cycles [])
  (reset! selected-cyle 0)
  nil)

(defn init! []
  (let [population (initialize-population @init)]
    (swap! population-cycles #(conj % population))
    (start-worker population)
    (resume)
    nil))

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

(rum/defc run-button < rum/reactive []
  (when (= :stopped (rum/react status))
    [:div
     (init-population)
     (config-cycle)
     (config-cycle-aggregator)
     [:.ui.green.button
      {:on-click init!}
      "Run"]]))

(rum/defc pause-and-resume-buttons < rum/reactive []
  (case (rum/react status)
    :running
    [:.ui.yellow.button {:on-click pause}
     "Pause"]
    :paused
    [:.ui.teal.button {:on-click resume}
     "Resume"]
    nil))

(rum/defc stop-button < rum/reactive []
  (when-not (= :stopped (rum/react status))
    [:.ui.red.button {:on-click stop}
     "Stop"]))

(rum/defc circle < rum/static [key x y position selected]
  [:circle {:key key
            :style {:stroke "steelblue"
                    :stroke-width 1
                    :fill (if (= position selected)
                            "red"
                            "white")}
            :on-click #(do (reset! selected-cyle position) nil)
            :cx x
            :cy y
            :cz 10
            :r 2}])

(rum/defc line < rum/static [key x1 y1 x2 y2]
  [:line {:key key
          :style {:stroke "steelblue"   ;(inline "rgb" 255 0 0)
                  :stroke-width 1}
          :x1 x1
          :y1 y1
          :x2 x2
          :y2 y2}])

(defn list->points
  "Generates a list of points that can be fed to chart.
  Arguments:

    - x-step: constant distance between two adjacent points' x values.
    - height: the height of the chart.
    - ys: list of main values, each of which is between 0 and 1."
  [x-step height ys]
  (-> #(vector (* x-step (inc %1)) (- height (* height %2)))
      (map-indexed ys)))

(rum/defc column < rum/static
  [i payoff-mean selected]
  [:div.column {:key i
                :style {:background-color
                        (if (= i selected)
                          "yellow" "#eee")}
                :on-click #(reset! selected-cyle i)}
   [:div.item
    {:style {:background-color
             (if (= i selected)
               "red" "#369")
             :height (str (* 100 (/ payoff-mean max-payoff-mean))
                          "%")}}]])

(rum/defc chart < rum/reactive []
  [:div.chart {:style {:height (str "300px")}}
   (let [the-cycles (rum/react payoff-mean-cycles)
         selected (rum/react selected-cyle)]
     (when-not (= :stopped (rum/react status))
       (for [i (range (count the-cycles))]
         (column i (nth the-cycles i) selected))))])

(rum/defc inspector < rum/reactive []
  (when (< 0 (count (rum/react payoff-cycles)))
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
         (str (reduce (fn [acc fsm]
                        (update-in acc [(automaton-name fsm)] inc))
                      {}
                      automata))])]]))

(rum/defc launch-board < rum/reactive []
  [:div
   (run-button)
   (pause-and-resume-buttons)
   (stop-button)
   (chart)
   (inspector)])

(let [app-root (.getElementById js/document "my-app")]
  (rum/mount (launch-board) app-root))
