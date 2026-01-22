library(furrr)
library(future)
library(purrr)
library(tidyverse)
library(igraph)
library(tibble)
## RAN functions 
source("R/Random_Networks.R")

## Individual graphs
load("data/individual_networks.Rdata")
load("data/child_oriented_graph.Rdata")
cdi <- readRDS("data/cdi-metadata.rds")
cdi$POS <- ifelse(cdi$lexical_class == "nouns"|cdi$lexical_class=="verbs",cdi$lexical_class, "other")


## Tibble with vocab size, graph, and POS
ran_list <- map(vocab_graphs, function(x){
  vocab <- names(as_adj_list(x$graph))
  vocab_size <- length(names(as_adj_list(x$graph)))
  POS <- data.frame(word = vocab) %>%
    left_join(select(cdi,CDI_Metadata_compatible,POS), by = c("word" = "CDI_Metadata_compatible")) 
  POS <- table(POS$POS)
  
  return(list(subjectkey = x$id,vocab = vocab,vocab_size = vocab_size, graph = x$graph, POS = POS))                  
})
  
balanced_RAN_network(ran_list[[1]]$vocab_size,graph,ran_list[[1]]$POS)

multiSample(ran_list[[1]]$vocab_size,V(graph),ran_list[[1]]$POS,simplify2vec = TRUE)
