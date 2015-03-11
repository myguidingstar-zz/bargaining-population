(ns bargaining-population.app
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [rum :include-macros true]
            [bargaining-population.automaton :refer
             [initial-automaton automaton-name aggregate-type-rate]]
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

(def population-type-rate-cycles (atom []))

(defn append-population-cycles! [population]
  (swap! population-cycles #(conj % population))
  (swap! population-type-rate-cycles #(conj % (aggregate-type-rate population))))

(def payoff-cycles (atom []))

(def payoff-mean-cycles (atom []))

(def max-payoff-mean 8)

(def selected-cyle (atom 0))

(def projected-types (atom {:high true
                            :medium true
                            :low false
                            :accommodator false}))

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
    (append-population-cycles! population-after)))

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
  (reset! population-type-rate-cycles [])
  (reset! payoff-cycles [])
  (reset! payoff-mean-cycles [])
  (reset! selected-cyle 0)
  nil)

(defn init! []
  (let [population (initialize-population @init)]
    (append-population-cycles! population)
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

(rum/defc select-projected-types < rum/reactive []
  [:.ui.form
   [:.grouped.inline.fields
    [:.ui.label "Projected types (must be 2)"]
    [:.field
     (let [projected? (rum/react projected-types)]
       (map-indexed
        (fn [i the-type]
          [:.ui.checkbox
           {:key i
            :on-click
            #(do (swap! projected-types update-in [the-type] not)
                 nil)}
           [:input {:type "checkbox"
                    :checked (projected? the-type)}]
           [:label (name the-type)]])
        [:high :medium :low :accommodator]))]]])

(rum/defc run-button < rum/reactive []
  (when (= :stopped (rum/react status))
    [:div
     (init-population)
     (config-cycle)
     (config-cycle-aggregator)
     (select-projected-types)
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
            :style {:fill (if (= position selected)
                            "red"
                            "green")}
            :on-click #(do (reset! selected-cyle position) nil)
            :cx x
            :cy y
            :cz 10
            :r 2}])

(rum/defc line < rum/static [key x1 y1 x2 y2]
  [:line {:key key
          :style {:stroke "steelblue"
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

(rum/defc chart < rum/reactive []
  [:div {:style {:overflow "scroll"}}
   [:svg {:style {:background-color "#eee"}
          :height 300
          :width 5000}
    (let [the-cycles (rum/react payoff-mean-cycles)
          selected (rum/react selected-cyle)]
      (when-not (= :stopped (rum/react status))
        (->> (map #(/ % 8) the-cycles)
             (list->points 3 300)
             (partition-all 2 1)
             (map-indexed (fn [i [[x1 y1] [x2 y2]]]
                            [(circle (* 2 i) x1 y1
                                     i selected)
                             (when x2 (line (inc (* 2 i)) x1 y1 x2 y2))]))
             (apply concat))))]])

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

(rum/defc population-type-rate-chart < rum/reactive []
  [:div {:style {:overflow "scroll"}}
   [:svg {:style {:background-color "#eee"}
          :height 300
          :width 300}
    (let [the-cycles (rum/react population-type-rate-cycles)
          the-types (true-keys (rum/react projected-types))
          selected (rum/react selected-cyle)]
      (when-not (= :stopped (rum/react status))
        (if (= 2 (count the-types))
          (->> (map #(map % the-types) the-cycles)
               (rates->points 300)
               (partition-all 2 1)
               (map-indexed (fn [i [[x1 y1] [x2 y2]]]
                              [(circle (* 2 i) x1 y1
                                       i selected)
                               (when x2 (line (inc (* 2 i)) x1 y1 x2 y2))]))
               (apply concat))
          [:text {:x 10
                  :y 140
                  :font-size 12}
           "There must be exactly 2 projected types."])))]])

(rum/defc inspector < rum/reactive []
  (let [the-cycles (rum/react payoff-cycles)
        selected   (rum/react selected-cyle)]
    (when (< 0 (count the-cycles))
      [:div
       [:div (str "Selected: " selected)]
       [:div (str "Payoffs: "
                  (nth the-cycles selected))]
       [:div (str "Payoff mean: "
                  (nth (rum/react payoff-mean-cycles) selected))]
       [:div (str "Population: ")
        (let [automata (nth (rum/react population-cycles) selected)]
          [:div
           (str (aggregate-type-rate automata))])]])))

(rum/defc launch-board < rum/reactive []
  [:div
   (run-button)
   (pause-and-resume-buttons)
   (stop-button)
   (chart)
   (inspector)
   (population-type-rate-chart)])

(let [app-root (.getElementById js/document "my-app")]
  (rum/mount (launch-board) app-root))
