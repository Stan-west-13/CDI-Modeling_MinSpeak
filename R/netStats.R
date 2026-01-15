#' Count number of nodes
#'
#' @param X A square matrix representing an associative network
#' @return The number of nodes (i.e., rows) in the network
count_nodes <- function(X) {
  return(nrow(X))
}

#' Count number of edges
#'
#' @param X A square matrix representing an associative network
#' @return The number of edges (i.e., non-zero values) in the network
count_edges <- function(X) {
  #assertthat::assert_that(all(X > 0), msg = "Some network values < 0")
    return(sum(X > 0))

}
#' Compute in-degree by node
#'
#' @param X A square matrix representing an associative network
#' @return For each node, the number of edges pointing towards it.
#'
#' By convention, rows point towards columns in associative networks. For
#' example, in the network:
#'       A B
#'     A 0 1
#'     B 0 0
#' There is one directed edge, A --> B.
#' The in-degree of A is 0, and the in-degree of B is 1.
indegree <- function(X) {
  #assertthat::assert_that(all(X > 0), msg = "Some network values < 0")
   return(colSums(X > 0))
}
indegree_igraph <- function(X) {
  return(igraph::degree(X, mode = "in"))
}

#' Compute out-degree by node
#'
#' @param X A square matrix representing an associative network
#' @return For each node, the number of edges pointing away from it.
#'
#' By convention, rows point towards columns in associative networks. For
#' example, in the network:
#'       A B
#'     A 0 1
#'     B 0 0
#' There is one directed edge, A --> B.
#' The out-degree of A is 1, and the in-degree of B is 0.
outdegree <- function(X) {
 # assertthat::assert_that(all(X > 0), msg = "Some network values < 0")
 # assertthat::are_equal(nrow(X), ncol(X), msg = "Not a square")
  return(rowSums(X > 0))
}
outdegree_igraph <- function(X){
  return(igraph::degree(X, mode = "out"))
}
# Network validations for functions for every input.

