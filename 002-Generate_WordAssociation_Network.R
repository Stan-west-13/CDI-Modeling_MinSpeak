library(tidyverse)
library(ggplot2)
library(igraph)

source("R/assocNetwork.R")

assoc <- read.csv("data/CoxHae_WordAssociations.csv")

cue_resp <- assoc %>%
  select(cue = CUE, resp = TARGET,COND) %>%
  filter(COND == "toddler",
         resp %in% cue)

adj_mat <- assocNetwork_noLoops(cue_resp)

graph <- graph_from_adjacency_matrix(adj_mat)

plot(graph)
