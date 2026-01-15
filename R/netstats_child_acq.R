netStats_child_acq <- function(G) {
  netStats <- function(N) {
    y <- c("mean.indegree" = mean(indegree_igraph(N)),
           "outdegree" = mean(outdegree_igraph(N)),
           "clustering.coefficient" = transitivity(N, type = "global"),
           "mean.distance" = mean_distance(N),
           "median.indegree" = median(indegree_igraph(N)))

    return(y)
  }
  return(netStats(G))
}
