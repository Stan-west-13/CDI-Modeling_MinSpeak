library(tidyverse)
library(ggplot2)
library(igraph)


assoc <- read.csv("data/CoxHae_WordAssociations.csv")

cue_resp <- assoc %>%
  select(cue = CUE, response = TARGET,COND) %>%
  filter(COND == "toddler",
         response %in% cue)

adj_mat <- as.matrix(ifelse(xtabs(~cue+response, data = cue_resp)>1,1,0 ))

graph <- graph_from_adjacency_matrix(adj_mat)
