acquisition_network <- function(ix, network_of_words){
  assertthat::are_equal(nrow(network_of_words), ncol(network_of_words))
  assertthat::assert_that(all(rownames(network_of_words) %in% colnames(network_of_words)))
    if (all(rownames(network_of_words) == colnames(network_of_words))) {
    ixc <- ix
  } else {
    ixc <- which(colnames(network_of_words) %in% rownames(network_of_words)[ix])
  }
  return(network_of_words[ix, ixc])
}

random_acquisition_network <- function(vocab_size, network_of_words){
  env_size <- nrow(network_of_words)
  ix <- sample(x = env_size, size = vocab_size, replace = FALSE)
  return(acquisition_network(ix, network_of_words))
}

child_acquisition_network <- function(child_vocab, network_of_words){
  ix <- which(rownames(network_of_words) %in% child_vocab)
  return(acquisition_network(ix, network_of_words))
}

random_acq_network_igraph <- function(vocab_size, G){
  env_size <- igraph::V(G)
  ix <- sample(x = env_size, size = vocab_size, replace = TRUE)
  return(igraph::induced_subgraph(G, ix))
}
multiSample <- function(n, x, fct, simplify2vec = FALSE) {
  y <- mapply(sample, split(x, fct), n, SIMPLIFY = FALSE)
  return(if(simplify2vec) unname(do.call(c, y)) else y)
}

balanced_RAN_network <- function(vocab_size, G,G_WG, POS, POS_WG) {
  if (unique(vocab_size$form == "WS")){
    env_size <- igraph::vcount(G)
    assertthat::are_equal(env_size, length(POS))
    assertthat::are_equal(nlevels(POS), length(vocab_size))
    ix <- multiSample(vocab_size$POS, seq_len(env_size), POS, simplify2vec = TRUE)
    return(igraph::induced_subgraph(G, ix))
  }
  else {
    env_size <- igraph::vcount(G_WG)
    assertthat::are_equal(env_size, length(POS_WG))
    assertthat::are_equal(nlevels(POS_WG), length(vocab_size$POS))
    ix_wg <- multiSample(vocab_size$POS, seq_len(env_size), POS_WG, simplify2vec = TRUE)
    return(igraph::induced_subgraph(G_WG, ix_wg))
  }
}


balanced_RAN_network_noun <- function(vocab_size, G, POS) {
    env_size <- igraph::vcount(G)
    assertthat::are_equal(env_size, length(POS))
    assertthat::are_equal(nlevels(POS), length(vocab_size))
    ix <- multiSample(vocab_size$POS, seq_len(env_size), POS, simplify2vec = TRUE)
    return(igraph::induced_subgraph(G, ix))
}



network_stats <- function(g) {
  return(c(indegreemed=median(indegree_igraph(g), na.rm = T), 
           indegreeavg=mean(indegree_igraph(g)),
           clustcoef=igraph::transitivity(g, type = "global"),
           meandist=igraph::mean_distance(g)))
}

balanced_RAN_stats <- function(vocab_size, G,G_WG, POS, POS_WG) {
  return(network_stats(balanced_RAN_network(vocab_size, G,G_WG, POS, POS_WG)))
}

balanced_RAN_stats_noun <- function(vocab_size, G, POS) {
  return(network_stats(balanced_RAN_network_noun(vocab_size, G, POS)))
}
