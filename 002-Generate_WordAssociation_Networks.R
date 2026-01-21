library(tidyverse)
library(ggplot2)
library(igraph)
library(purrr)
library(furrr)
library(future)
library(devtools)

## Association network functions
## Fuzzy matching packages
source("R/assocNetwork.R")
install_github("dgrtwo/fuzzyjoin")
## Load vocabulary and association data
load("data/associations-child.Rdata")
vocab_data <- read_rds("data/all_minspeak.rds")

## Resolve inconsistencies between cues/CDI
x <- data.frame(vocab = unique(vocab_data$CDI_Metadata_compatible)) ## Unique Vocab words
y <- data.frame(cue = as.character(unique(associations_child$CUE))) ## Unique cues
match_set <- y$cue[!y$cue %in% x$vocab] ## Mismatches

x$distances <- stringdist(match_set,
                        x$vocab,
                        method = "lv")


## Cue-response table for association matrix
cue_resp <- associations_child %>%
  select(cue = CUE, resp = RESPONSE,COND) %>%
  filter(resp %in% cue)

adj_mat <- assocNetwork_noLoops(cue_resp)

## igraph from adjacency matrix
graph <- graph_from_adjacency_matrix(adj_mat)

save(adj_mat, file = "data/child_oriented_mat.Rdata")
save(graph, file = "data/child_oriented_graph.Rdata")

## Split individual vocabularies
vocab_splits <- vocab_data %>%
  group_by(subjectkey,interview_age) %>%
  group_split()

## Construct individual association networks
vocab_graphs <- map(vocab_splits, function(x){
  ind_adj_mat <- adj_mat[rownames(adj_mat) %in% x$]
})







