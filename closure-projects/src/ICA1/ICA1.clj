(ns ICA1.ICA1)
;; Defining the graph structure

(defrecord Graph [vertices edges])

;; Function to create an empty graph

(defn make-graph []

  (Graph. (atom {}) (atom {})))



;; Defining vertex structure

(defrecord Vertex [label neighbors])


;; Function to create a vertex with a given label
(defn make-vertex [label]

  (Vertex. label (atom '())))


;; Function to create a vertex with a given label
(defn graph-add-vertex! [graph label]

  (let [vertices (:vertices graph)

        new-vertex (make-vertex label)]

    (swap! vertices assoc label new-vertex))

  nil)



;; Edge structure definition

(defrecord Edge [from to label weight])



(defn make-edge [from to label weight]

  (Edge. from to label weight))

;; Function to generate a key for an edge based on the vertices it connects

(defn graph-edge-key [from to]

  (sort (list from to)))



;; Function to add an edge to the graph
(defn graph-add-edge! [graph from to label weight]

  (let [vertices (:vertices graph)

        from-vertex (get @vertices from)

        to-vertex (get @vertices to)

        from-vertex-neighbors @(:neighbors from-vertex)

        to-vertex-neighbors @(:neighbors to-vertex)

        new-edge (make-edge from to label weight)

        new-edge-key (graph-edge-key from to)]

    (swap! (:edges graph) assoc new-edge-key new-edge)

    (reset! (:neighbors from-vertex) (conj from-vertex-neighbors to))

    (reset! (:neighbors to-vertex) (conj to-vertex-neighbors from))))



;; Implemented functions to handle the graph

(defn graph-has-vertex? [graph label]

  (contains? @(:vertices graph) label))



(defn graph-has-edge? [graph from to]

  (contains? @(:edges graph) (graph-edge-key from to)))



(defn get-edge-weight [graph from to]

  (:weight (get @(:edges graph) (graph-edge-key from to))))



;; Define  graph data

(def g (make-graph))



;; Adding vertices and edges manually

(doseq [data [["Krakov" "Warsaw" "100"]

              ["Hamburg" "Berlin" "100"]

              ["Warsaw" "Berlin" "300"]

              ["Prague" "Berlin" "200"]

              ["Munich" "Berlin" "100"]

              ["Munich" "Innsbruck" "100"]

              ["Vienna" "Innsbruck" "200"]

              ["Vienna" "Budapest" "300"]

              ["Warsaw" "Budapest" "400"]

              ["Zagreb" "Budapest" "200"]

              ["Vienna" "Rome" "400"]

              ["Napoli" "Rome" "200"]

              ["Napoli" "Rijeka" "100"]

              ["Vienna" "Prague" "200"]

              ["Vienna" "Rijeka" "400"]

              ["Rijeka" "Zagreb" "100"]

              ["Vienna" "Zagreb" "300"]

              ["Munich" "Zagreb" "400"]

              ["Innsbruck" "Rome" "400"]

              ["Budapest" "Rome" "400"]

              ["Budapest" "Berlin" "300"]

              ["Prague" "Brno" "100"]

              ["Prague" "Budapest" "300"]]]

  (let [[from to weight] data]

    (when (not (graph-has-vertex? g from))

      (graph-add-vertex! g from))

    (when (not (graph-has-vertex? g to))

      (graph-add-vertex! g to))

    (graph-add-edge! g from to (str from " " to " " weight) (Integer/parseInt weight))))





;; Function to get neighbors of a vertex in the graph
(defn graph-get-neighbors [graph label]
  (let [vertex (get @(:vertices graph) label)]
    (if vertex
      @(:neighbors vertex)
      (do (println (str " No vertex found for label " label))
          []))))

;; Function to check constraints on cost, budget, and maximum flights
(defn check-constraints [cost budget path max-flights]
  (and (<= cost budget)
       (< (- (count path) 1) max-flights)))

;; Breadth-First Search function to find flight plans
(defn bfs-find-plans [graph start-label end-city-spec budget max-flights]
  (let [start-cost (get-edge-weight graph start-label start-label)
        queue (ref [[{:vertex start-label :cost (or start-cost 0)}]])
        plans (atom [])]
    (while (not (empty? @queue))
      (let [path (first @queue)]
        (dosync
          (ref-set queue (rest @queue)))
        (let [current-vertex (-> path last :vertex)
              current-cost (-> path last :cost)]
          (when (and (and (string? end-city-spec) (= current-vertex end-city-spec))
                     (check-constraints current-cost budget path max-flights))
            (swap! plans conj {:path (map (fn [p] {:city (:vertex p) :cost (:cost p)}) path) :total-cost current-cost}))
          (when (not (= current-vertex end-city-spec))
            (let [neighbors (graph-get-neighbors graph current-vertex)]
              (doseq [neighbor neighbors]
                (let [edge-cost (get-edge-weight graph current-vertex neighbor)
                      total-cost (+ current-cost edge-cost)]
                  (when (and (not (some #(= neighbor (:vertex %)) path))
                             (check-constraints total-cost budget path max-flights))
                    (dosync
                      (alter queue conj (conj path {:vertex neighbor :cost total-cost})))))))))))
    @plans))

;; Function to sort flight plans based on total cost and number of flights
(defn sort-plans [plans]
  (sort-by (juxt (comp - :total-cost) (comp count :path)) plans))

;; Function to remove duplicate flight plans based on the number of flights
(defn remove-duplicate-paths [plans]
  (let [seen-flights (atom #{})]
    (filter (fn [plan]
              (let [num-flights (- (count (:path plan)) 1)]
                (if (contains? @seen-flights num-flights)
                  false
                  (do
                    (swap! seen-flights conj num-flights)
                    true))))
            plans)))

;; Function to find and sort flight plans
(defn find-and-sort-plans [graph start-label end-city-name budget max-flights client-type]
  (let [client-budget (case client-type
                        :family 700
                        :group 1000)
        raw-plans (bfs-find-plans graph start-label end-city-name client-budget max-flights)]
    (let [filtered-plans (filter
                           (fn [plan]
                             (and (<= (:total-cost plan) client-budget)
                                  (< (- (count (:path plan)) 1) max-flights)))
                           raw-plans)]
      (let [sorted-plans (sort-plans filtered-plans)
            distinct-plans (remove-duplicate-paths sorted-plans)
            most-expensive-plan (first distinct-plans)
            cheapest-plan (last distinct-plans)]
        (if (= most-expensive-plan cheapest-plan)
          [most-expensive-plan]
          [most-expensive-plan cheapest-plan])))))

;; Function to format a flight path for display
(defn format-path [path]
  (let [formatted-path (map (fn [{:keys [city cost]}]
                              (str city (if (zero? cost) "" (str " (" cost ")"))))
                            path)]
    (clojure.string/join " - " formatted-path)))

;; Function to extract the city name from a vertex label
(defn extract-city-from-label [label]
  (last (clojure.string/split label #" ")))

;; Function to display a flight plan option
(defn display-option [index path total-cost graph]
  (let [formatted-path (map (fn [{:keys [city cost]}]
                              (let [city-name (extract-city-from-label city)]
                                (str city-name (if (zero? cost) "" (str " (" cost ")")))))
                            path)]
    (println (str "Option " index ":"))
    (let [flight-path (->> formatted-path
                           (remove #{"-"})
                           (clojure.string/join " - "))]
      (if (empty? formatted-path)
        (println "No flight plan exists between these two cities meeting your requirements")
        (println (str "The plan is: " flight-path " with a total cost of " total-cost))))))

;; Function to reverse-engineer costs from a path
(defn reverse-engineer-costs [path]
  (loop [remaining-path (reverse path)
         last-cost (-> path last :cost)
         result []]
    (if (empty? remaining-path)
      (reverse result)
      (let [current-cost (or (-> remaining-path first :cost) 0)
            calculated-cost (- last-cost current-cost)]
        (recur (rest remaining-path) current-cost
               (conj result (assoc (first remaining-path) :cost calculated-cost)))))))

;; Function to print reversed flight plans
(defn print-reversed-plans [plans]
  (doseq [plan plans]
    (let [{:keys [path total-cost]} plan
          reversed-path (reverse-engineer-costs path)
          formatted-path (format-path reversed-path)]
      (println "Path: " formatted-path)
      (println "Total Cost: " total-cost))))

;; Function to get all cities in the graph
(defn get-all-cities [graph]
  (keys @(:vertices graph)))

;; Function to choose a city from a list
(defn choose-city [prompt graph]
  (let [cities (get-all-cities graph)]
    (println prompt)
    (doseq [[idx city] (map vector (range 1 (inc (count cities))) cities)]
      (println (str idx ". " city)))
    (let [choice-str (read-line)
          choice (if (re-matches #"\d+" choice-str)
                   (Integer/parseInt choice-str) 0)]
      (cond (and (>= choice 1) (<= choice (count cities)))
            (nth cities (dec choice))
            (some #{choice-str} cities)
            choice-str
            :else (do
                    (println "Invalid choice. Please choose again.")
                    (recur prompt graph))))))

;; Function to choose a client type
(defn choose-client-type []
  (println "Enter your client type: family/group")
  (let [choice (read-line)]
    (condp = choice
      "family" :family
      "group" :group
      (do (println "Invalid choice. Please choose again.")
          (recur)))))

;; Function to print a flight plan option
(defn print-option [plan index]
  (let [{:keys [path total-cost]} plan
        flight-path (->> path
                         (map :label)
                         (clojure.string/join " - "))
        flight-costs (->> path
                          (map :weight)
                          (clojure.string/join " + "))]
    (println (str "Option " index ":"))
    (println (str flight-path ". " flight-costs " = " total-cost))))

;; Function to display flight plan options
(defn display-options [plans graph]
  (if (seq plans)
    (doseq [plan-info (map vector plans (range 1 (inc (count plans))))]
      (apply print-option plan-info graph))
    (println "Sorry, but no flight plan was found between these two cities meeting your requirements.")))

;; Function to get user input for starting city, destination city, and client type
(defn get-user-input [graph]
  (let [start-city (choose-city "Enter the starting city:" graph)
        end-city (choose-city "Enter the destination city:" graph)
        client-type (choose-client-type)]
    [start-city end-city client-type]))

;; Main function
(defn main [g]
  (when (not (empty? @(:vertices g)))
    (let [[start-city end-city client-type] (get-user-input g)
          plans (find-and-sort-plans g start-city end-city 1000 3 client-type)]
      (if (seq plans)
        (let [second-price-plan (first (sort-by :total-cost plans))
              highest-price-plan (second (sort-by :total-cost plans))
              other-plans (rest (sort-by :total-cost plans))]
          (println "\nPlan:")
          (if (seq other-plans)
            (do
              (display-option 2 (:path highest-price-plan) (:total-cost highest-price-plan) g)
              (display-options other-plans g))
            (println " Option: Unfortunately, only one trip plan meets your requirements."))
          (display-option 1 (:path second-price-plan) (:total-cost second-price-plan) g))
        ;; No need to print "Sorry..." text here
        ))))

(main g)
