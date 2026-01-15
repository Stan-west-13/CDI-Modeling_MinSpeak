netStats_randAcq <- function(G, vocab_size, nrep) {
  netStats <- function(N) {
    y <- c("indegree" = mean(indegree_igraph(N)),
           "outdegree" = mean(outdegree_igraph(N)),
           "clustering coefficient" = transitivity(N, type = "global"),
           "mean distance" = mean_distance(N),
           "median indegree" = median(indegree_igraph(N)))

    return(y)
  }
  return(replicate(nrep, netStats(random_acq_network_igraph(vocab_size , G))))
}

