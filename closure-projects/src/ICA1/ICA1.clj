(ns ICA1.ICA1)
(defrecord Graph [vertices edges])

(defn make-graph []
  (Graph. (atom {}) (atom {})))

(defrecord Vertex [label neighbors])

(defn make-vertex [label]
  (Vertex. label (atom '())))

(defn graph-add-vertex! [graph label]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label)]
    (swap! vertices assoc label new-vertex))
  nil)

(defrecord Edge [from to label weight])

(defn make-edge [from to label weight]
  (Edge. from to label weight))

(defn graph-edge-key [from to]
  (sort (list from to)))

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

(defn graph-has-vertex? [graph label]
  (contains? @(:vertices graph) label))

(defn graph-has-edge? [graph from to]
  (contains? @(:edges graph) (graph-edge-key from to)))

(defn get-edge-weight [graph from to]
  (:weight (get @(:edges graph) (graph-edge-key from to))))

(def g (make-graph))

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

(defn graph-get-neighbors [graph label]
  (let [vertex (get @(:vertices graph) label)]
    (if vertex
      @(:neighbors vertex)
      (do (println (str " No vertex found for label " label))
          []))))

(defn check-constraints [cost budget path max-flights]
  (and (<= cost budget)
       (< (- (count path) 1) max-flights)))

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

(defn sort-plans [plans]
  (sort-by (juxt (comp - :total-cost) (comp count :path)) plans))

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
            second-longest-plan (second distinct-plans)]
        [most-expensive-plan second-longest-plan]))))


(defn format-path [path]
  (let [formatted-path (map (fn [{:keys [city cost]}]
                              (str city (if (zero? cost) "" (str " (" cost ")"))))
                            path)]
    (clojure.string/join " - " formatted-path)))

(defn extract-city-from-label [label]
  (last (clojure.string/split label #" ")))

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
(defn get-input [prompt]
  (print prompt)
  (flush)
  (read-line))

(defn get-user-input [g]
  (let [start-city (get-input "Departure city? ")
        end-city (get-input "Destination city? ")
        client-type (keyword (get-input "Client type? (family/group) "))]
    [start-city end-city client-type]))

(defn main [g]
  (when (not (empty? @(:vertices g)))
    (let [[start-city end-city client-type] (get-user-input g)
          [most-expensive-plan second-longest-plan] (find-and-sort-plans g start-city end-city 1000 3 client-type)]
      (if (and most-expensive-plan second-longest-plan)
        (do
          (println "\nMost Expensive Plan:")
          (display-option 1 (:path most-expensive-plan) (:total-cost most-expensive-plan) g)
          (println "\nSecond Longest Plan:")
          (display-option 2 (:path second-longest-plan) (:total-cost second-longest-plan) g))
        (if most-expensive-plan
          (do
            (println "\nMost Expensive Plan:")
            (display-option 1 (:path most-expensive-plan) (:total-cost most-expensive-plan) g))
          (println "Sorry, but no flight plan was found between these two cities meeting your requirements."))))))




(main g)
