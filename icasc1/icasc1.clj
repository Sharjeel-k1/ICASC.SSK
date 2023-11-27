(ns closure-projects.icasc1.icasc1
  (:require [clojure.set :as set]))


(def ^:dynamic *departure* nil)
(def ^:dynamic *destination* nil)
(def ^:dynamic *trip-type* nil)
; ------ GRAPH RELATED

; Creates an empty graph, which is represented as an atom that contains an empty map.
(defn make-graph []
  (atom {}))

; Adds a new vertex to the graph. The vertex is represented as a map that includes its coordinates and edges.
; If a vertex with the same name already exists in the graph, it throws an IllegalArgumentException.
(defn graph-add-vertex! [g name lat lon]
  (if (contains? @g name)
    (throw (IllegalArgumentException. (str "Vertex with name " name " already exists")))
    (swap! g assoc name {:coordinates [lat lon] :edges {}})))

; Adds a new edge to the graph, connecting two vertices. The edge is represented as a map that includes its name and price.
; If either vertex does not exist in the graph, or if the price is not provided, it throws an IllegalArgumentException.
(defn graph-add-edge! [g from to edge-name price]
  (let [from-vertex (get @g from)
        to-vertex (get @g to)]
    (when (or (nil? from-vertex) (nil? to-vertex))
      (throw (IllegalArgumentException. (str "Either vertex " from " or " to " not found in graph"))))
    (when (nil? price)
      (throw (IllegalArgumentException. "Price must be provided")))
    (swap! g assoc from (assoc from-vertex :edges (assoc (:edges from-vertex) to {:name edge-name :price price})))
    (swap! g assoc to (assoc to-vertex :edges (assoc (:edges to-vertex) from {:name edge-name :price price})))
    ))


; Returns a specific vertex in the graph. If the vertex does not exist, it returns nil.
(defn graph-find-vertex [g vertex-name]
  (let [graph @g
        vertex (get graph vertex-name)]
    vertex))

; ------- VERTEX-price RELATED

; Creates an empty vertex price map, which is represented as an atom that contains an empty map.
(defn make-vertex-price []
  (atom {}))

; Sets the price for a specified vertex in the vertex price map.
(defn vertex-price-set! [vd name price]
  (swap! vd assoc name price))

; Gets the price for a specified vertex from the vertex price map. If the vertex does not exist, it returns nil.
(defn vertex-price-get [vd name]
  (get @vd name))

; ------- VERTEX-SET RELATED

; Creates an empty vertex set, which is represented as an atom that contains an empty set.
(defn make-vertex-set []
  (atom #{}))

; Adds a vertex to the vertex set.
(defn vertex-set-add! [vs name]
  (swap! vs conj name))

; Deletes a vertex from the vertex set.
(defn vertex-set-del! [vs name]
  (swap! vs disj name))

; Checks if a vertex is in the vertex set. Returns true if the vertex is in the set, and false otherwise.
(defn vertex-set-get [vs name]
  (contains? @vs name))

; Adds all vertices from the given graph to the vertex set.
(defn vertex-set-add-all! [vs g]
  (doseq [v (keys @g)]
    (vertex-set-add! vs v)))

; ------- RELATED TO ALL ABOVE

; This function finds the vertex with the minimum price value that hasn't been completed yet.
(defn minimum-price-vertex [g vd vs]
  (let [dist-keys (keys @vd)  ; get the keys (vertex names) from the vertex price map
        not-completed-keys (set/intersection (set dist-keys) @vs)]  ; find the intersection of dist-keys and the not-completed vertex set
    (if (empty? not-completed-keys)  ; if there are no more vertices to process
      nil  ; return nil
      (apply min-key (fn [k] (get @vd k)) not-completed-keys))))  ; otherwise, find the vertex with the minimum price value that hasn't been completed yet

; This function finds all shortest paths from start to destination.
(defn get-shortest-paths [g vertex-price start destination edge-price-function]
  (let [paths (atom [])]  ; create an atom to hold the paths
    (defn backtrack [current-vertex path]  ; define a recursive function to backtrack from the destination to the start
      (let [current-price (vertex-price-get vertex-price current-vertex)
            current-vertex-data (graph-find-vertex g current-vertex)
            edges (:edges current-vertex-data)
            previous-vertices (filter (fn [[vertex-name _]] (= (- current-price (edge-price-function current-vertex-data vertex-name)) (vertex-price-get vertex-price vertex-name))) edges)]
        (if (not= current-vertex start)
          (doseq [[previous-vertex-name _] previous-vertices]  ; for each previous vertex
            (backtrack previous-vertex-name (cons previous-vertex-name path)))  ; recursively call backtrack with the current vertex and path
          (swap! paths conj path))))  ; when the start vertex is reached, add the path to the paths atom
    (backtrack destination [destination])  ; start the backtrack from the destination
    @paths))  ; return the contents of the paths atom

; Prints paths
(defn print-paths-vertically [paths g edge-price-function]
  (doseq [[i path] (map vector (range 1 (inc (count paths))) paths)]  ; for each path with its index
    (println (str " Path " i ":"))  ; print a header with the path number
    (doseq [v (partition 2 1 path)]  ; for each pair of vertices in the path
      (let [[from to] v
            edge-data (get (:edges (get @g from)) to)
            edge-name (:name edge-data)
            edge-price (edge-price-function (get @g from) to)]
        (println (str "  From " from " take " edge-name " with price " edge-price " to " to))))  ; print the vertex, edge name and price
    (println)))  ; print a newline

; ------- GRAPH DATA GOES HERE



(def g (make-graph))


(graph-add-vertex! g "Krakov" 50.0647 19.9450)
(graph-add-vertex! g "Hamburg" 53.5488 9.9872)
(graph-add-vertex! g "Warsaw" 52.2297 21.0122)
(graph-add-vertex! g "Berlin" 52.5200 13.4050)
(graph-add-vertex! g "Prague" 50.0755 14.4378)
(graph-add-vertex! g "Munich" 48.1351 11.5820)
(graph-add-vertex! g "Vienna" 48.2082 16.3719)
(graph-add-vertex! g "Innsbruck" 47.2692 11.4041)
(graph-add-vertex! g "Budapest" 47.4979 19.0402)
(graph-add-vertex! g "Zagreb" 45.8150 15.9819)
(graph-add-vertex! g "Rome" 41.9028 12.4964)
(graph-add-vertex! g "Napoli" 40.8518 14.2681)
(graph-add-vertex! g "Rijeka" 45.3271 14.4422)
(graph-add-vertex! g "Brno" 49.1951 16.6068)

(graph-add-edge! g "Krakov" "Warsaw" "E40" 100)
(graph-add-edge! g "Warsaw" "Berlin" "E41" 300)
(graph-add-edge! g "Hamburg" "Berlin" "E42" 100)
(graph-add-edge! g "Prague" "Berlin" "E43" 200)
(graph-add-edge! g "Munich" "Berlin" "E44" 100)
(graph-add-edge! g "Munich" "Innsbruck" "E45" 100)
(graph-add-edge! g "Vienna" "Innsbruck" "E46" 200)
(graph-add-edge! g "Vienna" "Budapest" "E47" 300)
(graph-add-edge! g "Warsaw" "Budapest" "E48" 400)
(graph-add-edge! g "Zagreb" "Budapest" "E49" 200)
(graph-add-edge! g "Vienna" "Rome" "E50" 400)
(graph-add-edge! g "Napoli" "Rome" "E51" 200)
(graph-add-edge! g "Napoli" "Rijeka" "E52" 100)
(graph-add-edge! g "Vienna" "Prague" "E53" 200)
(graph-add-edge! g "Vienna" "Rijeka" "E54" 400)
(graph-add-edge! g "Rijeka" "Zagreb" "E55" 100)
(graph-add-edge! g "Vienna" "Zagreb" "E56" 300)
(graph-add-edge! g "Munich" "Zagreb" "E57" 400)
(graph-add-edge! g "Innsbruck" "Rome" "E58" 400)
(graph-add-edge! g "Budapest" "Rome" "E59" 400)
(graph-add-edge! g "Budapest" "Berlin" "E60" 300)
(graph-add-edge! g "Prague" "Brno" "E61" 100)
(graph-add-edge! g "Prague" "Budapest" "E62" 300)

; For search engine get the real price of the edge from vertex structure
(defn search-edge-price-function [vertex adjacent-vertex-name]
  (:price (get (:edges vertex) adjacent-vertex-name)))

; This function performs the Dijkstra's algorithm to find the shortest path(s) from the start to the destination.
(defn shortest-path [g start destination edge-price-function max-price max-edges]
  (when-not (contains? @g start)
    (throw (IllegalArgumentException. (str "Start vertex " start " not found in graph"))))
  (when-not (contains? @g destination)
    (throw (IllegalArgumentException. (str "Destination vertex " destination " not found in graph"))))
  (let [vertex-price (make-vertex-price)
        completed-vertex-set (make-vertex-set)
        not-completed-vertex-set (make-vertex-set)
        path-doesnt-exist (atom nil)
        shortest-path-found (atom nil)]
    (vertex-price-set! vertex-price start 0)
    (vertex-set-add-all! not-completed-vertex-set g)
    (loop []
      (let [vertex-a-name (minimum-price-vertex g vertex-price not-completed-vertex-set)
            vertex-a (graph-find-vertex g vertex-a-name)
            adjacent-vertex-names (keys (:edges vertex-a))
            vertex-a-price (vertex-price-get vertex-price vertex-a-name)
            destination-price (vertex-price-get vertex-price destination)
            destination-not-completed (vertex-set-get not-completed-vertex-set destination)]
        (when (and (nil? vertex-a-name) destination-not-completed)
          (reset! path-doesnt-exist true))

        (when (and (nil? @path-doesnt-exist) (or (nil? vertex-a-name) (and (some? destination-price) (< destination-price vertex-a-price))))
          (reset! shortest-path-found true))

        (when (and (nil? @path-doesnt-exist) (nil? @shortest-path-found))
          (doseq [adjacent-vertex-name adjacent-vertex-names]
            (let [adjacent-vertex (graph-find-vertex g adjacent-vertex-name)
                  adjacent-vertex-price (vertex-price-get vertex-price adjacent-vertex-name)
                  adjacent-vertex-edge-price (edge-price-function vertex-a adjacent-vertex-name)
                  vertex-a-plus-edge-price (+ vertex-a-price adjacent-vertex-edge-price)]
              (when (or (nil? adjacent-vertex-price) (> adjacent-vertex-price vertex-a-plus-edge-price))
                (vertex-price-set! vertex-price adjacent-vertex-name vertex-a-plus-edge-price)
                (vertex-set-add! not-completed-vertex-set adjacent-vertex-name)
                (vertex-set-del! completed-vertex-set adjacent-vertex-name))))
          (vertex-set-add! completed-vertex-set vertex-a-name)
          (vertex-set-del! not-completed-vertex-set vertex-a-name)))

      (if (and (nil? @path-doesnt-exist) (nil? @shortest-path-found))
        (recur)))
    (when (some? @path-doesnt-exist)
      (println (str "Path from " start " to " destination " doesn't exist")))
    (when (some? @shortest-path-found)
      (println (str "Shortest path(s) from " start " to " destination " found with price:" (vertex-price-get vertex-price destination)))
      (println "Shortest path(s):")
      (print-paths-vertically (get-shortest-paths g vertex-price start destination edge-price-function) g edge-price-function))))


; -------- ASKS FOR start, destination, and trip type
(defn -main []
  (println "Enter departure city:")
  (def ^:dynamic *departure* (read-line))

  (println "Enter destination city:")
  (def ^:dynamic *destination* (read-line))

  (println "Enter trip type (family/organized):")
  (def ^:dynamic *trip-type* (read-line))

  (cond
    (= *trip-type* "family")
    (shortest-path g *departure* *destination* search-edge-price-function 700 2)

    (= *trip-type* "organized")
    (shortest-path g *departure* *destination* search-edge-price-function 1000 3)

    :else
    (println "Invalid trip type. Please enter 'family' or 'organized'.")))

(-main)
