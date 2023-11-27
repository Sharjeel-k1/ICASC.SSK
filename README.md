# SC_ICA1 - Flight Search Engine 


A  flight route search engine program developed in Clojure. It aids in finding optimal routes between various cities based on user-defined constraints and a pre-defined graph of connections.


## Features


- Graph Creation: The program constructs a graph representing city connections.
- Path Search: It identifies possible routes between a given departure and destination considering specific constraints such as connections and budget.
- User Interaction: Allows user input for departure city, destination city, and client type (Family/Organized).

## Running the program
To run this project, follow these steps:

1. Clone the repository to your local machine.
2. Open a terminal or command prompt and navigate to the project directory.
3. Ensure you have Clojure installed.
4. Run the `-main` function in the `closure-projects.icasc1.icasc1` namespace to start the shortest path finder.

clojure -m closure-projects.icasc1.icasc1

5. Follow the prompts to input the departure city, destination city, and trip type ('family' or 'organized').
6. The program will output the shortest path(s) based on the provided input.
5. Follow the prompts to input the departure city, destination city, and trip type ('family' or 'organized').
6. The program will output the cities and prices based on the provided input.

## Functions
- `make-graph`: Creates an empty graph representation.
- `graph-add-vertex!`: Adds a new vertex to the graph with coordinates.
- `graph-add-edge!`: Adds a new edge between two vertices.
- `graph-find-vertex`: Returns a specific vertex in the graph.
- `make-vertex-price`: Creates an empty vertex price map.
- `vertex-price-set!`: Sets the price for a specified vertex in the vertex price map.
- `vertex-price-get`: Gets the price for a specified vertex from the vertex price map.
- `make-vertex-set`: Creates an empty vertex set.
- `vertex-set-add!`: Adds a vertex to the vertex set.
- `vertex-set-del!`: Deletes a vertex from the vertex set.
- `vertex-set-get`: Checks if a vertex is in the vertex set.
- `vertex-set-add-all!`: Adds all vertices from the given graph to the vertex set.
- `minimum-price-vertex`: Finds the vertex with the minimum price value that hasn't been completed yet.
- `get-shortest-paths`: Finds all parts from departure to destination.
- `calculate-total-distance`: Calculates the total distance for a given path in the adjacency map.
- `print-possible-paths`: Prints possible paths based on certain criteria (source, destination, trip type).
- `get-user-input`: Gets user input for departure city, destination city, and trip type.
- `-main`: Main function to execute the program logic based on user input.
## Authors

-For any questions or suggestions, feel free to contact:

Contact

- Kenneth Asare (kennneth.asare@praguecollege.cz)
- Sacha Labastie (sacha.labastie@praguecollege.cz)
- Sharjeel Khan (sharjeel.khan@praguecollege.cz)

## LICENSE

This project is licensed under the [MIT License](LICENSE).
