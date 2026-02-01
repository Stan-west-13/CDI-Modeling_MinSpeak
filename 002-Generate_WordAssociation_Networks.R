#devtools::install_github("dgrtwo/fuzzyjoin",force = TRUE)

library(tidyverse)
library(igraph)
library(purrr)
library(fuzzyjoin)

## Association network functions
## Fuzzy matching packages
source("R/assocNetwork.R")

## Load vocabulary and association data
load("data/associations-child.Rdata")
vocab_data <- read_rds("data/combined_CDILetti.rds")

## Resolve inconsistencies between cues/CDI: Association cues in the CoxHae set are slightly different from CDI.
x <- data.frame(vocab = unique(vocab_data$CDI_Metadata_compatible)) ## Unique Vocab words
y <- data.frame(cue = as.character(unique(associations_child$CUE))) ## Unique cues
match_set <- y %>%
  filter(!cue %in% x$vocab)## Mismatches

reference_set <- x %>% ## References to match on
  filter(!vocab %in% y$cue)

best_match <- stringdist_join(match_set,
                              reference_set,
                              by = c("cue" = "vocab"),
                              method = "jw",
                              max_dist = 0.32,
                              mode = "left",
                              ignore_case = TRUE,
                              distance_col = "dst"
                              ) %>%
  group_by(cue) %>%
  slice_min(dst,n =1)
best_match[best_match$cue == "pet (noun)",]$vocab <- "pet's name" ## only one it couldn't match

## Cue-response table for association matrix

cue_resp <- associations_child %>%
  mutate(cue_match = ifelse(associations_child$CUE %in% best_match$cue,best_match$vocab,as.character(associations_child$CUE))) %>% ## new column matching cues with how they appear in vocab data
  select(cue = cue_match, resp = RESPONSE) %>% # use this as our new cue column
  filter(resp %in% cue) 

sum(unique(cue_resp$cue) %in% unique(vocab_data$CDI_Metadata_compatible)) ## 675 complete overlap

## Association matrix
adj_mat <- assocNetwork_noLoops(cue_resp)

## igraph from adjacency matrix
graph_FullNet <- graph_from_adjacency_matrix(adj_mat)

save(adj_mat, file = "data/child_oriented_mat.Rdata")
save(graph_FullNet, file = "data/child_oriented_graph.Rdata")

## Split individual vocabularies
individual_entries <- vocab_data %>%
  ungroup() %>%
  select(subjectkey_intAge,nProduced,Produces) %>%
  filter(Produces, nProduced >=3 ) %>%
  select(-Produces,-nProduced) %>%
  unique() %>%
  arrange(subjectkey_intAge)

vocab_splits_produced <- vocab_data %>%
  select(-interview_date) %>%
  unique() %>%
  filter(Produces, nProduced >= 3) %>%
  group_by(subjectkey_intAge) %>%
  group_split() %>% 
  setNames(individual_entries$subjectkey_intAge)


## Construct individual association networks
vocab_graphs <- map(vocab_splits_produced, function(x){
  ind_adj_mat <- as.matrix(adj_mat[rownames(adj_mat) %in% x$CDI_Metadata_compatible,colnames(adj_mat) %in% x$CDI_Metadata_compatible])
  return(list(graph = graph_from_adjacency_matrix(ind_adj_mat,mode = "directed")))
})

save(vocab_graphs,file = "data/individual_networks.Rdata")






