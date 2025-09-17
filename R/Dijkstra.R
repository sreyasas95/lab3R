#' Dijkstra's Shortest Path Algorithm
#'
#' This function implements Dijkstra's algorithm to find the shortest path from a starting node to all other nodes in a graph.
#'
#' @param graph A data frame representing the graph with three columns:
#' \describe{
#'   \item{v1}{The starting node of an edge (integer).}
#'   \item{v2}{The ending node of an edge (integer).}
#'   \item{w}{The weight (cost) of the edge between v1 and v2 (numeric).}
#' }
#' @param init_node The starting node for the path computation (integer).
#'
#' @return A named numeric vector containing the shortest distances from the starting node to all other nodes.
#' The names of the vector correspond to the node labels, and the values represent the shortest distance from the
#' starting node. If a node is unreachable, its distance will be marked as `Inf`.
#'
#' @description
#' Dijkstra's algorithm finds the shortest path between nodes in a graph with non-negative edge weights.
#' The algorithm iteratively selects the node with the smallest tentative distance and explores its neighbors,
#' updating the shortest known distance to each neighbor.
#'
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#' @export


dijkstra <- function(graph, init_node) {
  # Assert correct input
  stopifnot(is.data.frame(graph))
  stopifnot(all(c("v1", "v2", "w") %in% names(graph)))
  stopifnot(is.numeric(init_node), length(init_node) == 1)
  nodes <- sort(unique(c(graph$v1, graph$v2)))
  stopifnot(init_node %in% nodes)

  # Initialize
  n <- max(nodes)
  distances <- rep(Inf, n)
  names(distances) <- as.character(1:n)
  distances[as.character(init_node)] <- 0
  visited <- rep(FALSE, n)
  names(visited) <- as.character(1:n)

  while (any(!visited)) {
    # Pick unvisited node with smallest distance
    unvisited_indices <- which(!visited)
    current <- as.numeric(names(distances[unvisited_indices])[which.min(distances[unvisited_indices])])

    if (is.infinite(distances[as.character(current)])) break # Disconnected node(s)

    # For neighbors, update distances
    neighbors <- graph[graph$v1 == current & !visited[as.character(graph$v2)], ]
    for (i in seq_len(nrow(neighbors))) {
      neighbor <- neighbors$v2[i]
      weight <- neighbors$w[i]
      old_distance <- distances[as.character(neighbor)]
      new_distance <- distances[as.character(current)] + weight
      if (new_distance < old_distance) {
        distances[as.character(neighbor)] <- new_distance
      }
    }
    visited[as.character(current)] <- TRUE
  }

  # Return as numeric, in order of node labels
  distances <- distances[as.character(1:n)]
  distances[!((1:n) %in% nodes)] <- NA # mark non-existent
  as.numeric(distances)
}
wiki_graph <-
  data.frame(
    v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
    v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
    w  = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
  )

dijkstra(wiki_graph, 1) # Returns: 0 7 9 20 20 11
dijkstra(wiki_graph, 3) # Returns: 9 10 0 11 11 2
