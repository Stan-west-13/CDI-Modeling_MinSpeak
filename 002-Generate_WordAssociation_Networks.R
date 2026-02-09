#devtools::install_github("dgrtwo/fuzzyjoin",force = TRUE)

library(tidyverse)
library(igraph)
library(purrr)
library(fuzzyjoin)
library(tibble)

## Association network functions
## Fuzzy matching packages
source("R/assocNetwork.R")

## Load vocabulary and association data
load("data/associations-child.Rdata")
vocab_data <- read_rds("data/combined_CDILetti.rds")
cdi_WG <- read_rds("data/cdi_WG.rds")
noun_feats <- read.csv("data/MBCDI_concsFeats_2022-07-14.csv")


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

## 'tv' in the noun features is the only word that does not match 'TV' in the vocab data

noun_feats$definition[noun_feats$definition =="tv"] <- "TV" #fixing discrepancy


## Different feature lists
feats_list <- split(noun_feats, noun_feats$BR_Label)


feat_mats <- map(feats_list, function(x){
  x <- pivot_wider(as.data.frame(xtabs(~definition+Feature, data = x)),
                   names_from = Feature,
                   values_from = Freq)
  x <- as.matrix(column_to_rownames(x, var = "definition"))
  adj_mat <- ifelse(x %*% t(x) > 0,1,0)
  diag(adj_mat) <- 0
  return(list(feat_df = x, adj_mat = adj_mat, graph = graph_from_adjacency_matrix(adj_mat)))
})
  
save(feat_mats, file = "data/feature_graphs.Rdata")


## Cue-response table for association matrix

cue_resp <- associations_child %>%
  mutate(cue_match = ifelse(associations_child$CUE %in% best_match$cue,best_match$vocab,as.character(associations_child$CUE))) %>% ## new column matching cues with how they appear in vocab data
  select(cue = cue_match, resp = RESPONSE) %>% # use this as our new cue column
  filter(resp %in% cue) 

sum(unique(cue_resp$cue) %in% unique(vocab_data$CDI_Metadata_compatible)) ## 675 complete overlap

## Association matrix
adj_mat <- assocNetwork_noLoops(cue_resp)

## Cue-response table for association matrix Words and gestures

cue_resp_WG <- associations_child %>%
  mutate(cue_match = ifelse(associations_child$CUE %in% best_match$cue,best_match$vocab,as.character(associations_child$CUE))) %>% ## new column matching cues with how they appear in vocab data
  select(cue = cue_match, resp = RESPONSE) %>% # use this as our new cue column
  filter(cue %in% cdi_WG$CDI_Metadata_compatible,
         resp %in% cue) 

sum(unique(cue_resp_WG$cue) %in% unique(vocab_data$CDI_Metadata_compatible)) ## 393 of 395 in CDI: associations don't include babysitter name and child name

## Association matrix
adj_mat_wg <- assocNetwork_noLoops(cue_resp_WG)


## igraph from adjacency matrix
graph_FullNet <- graph_from_adjacency_matrix(adj_mat)
graph_WG <- graph_from_adjacency_matrix(adj_mat_wg)

save(adj_mat, file = "data/child_oriented_mat.Rdata")
save(graph_FullNet, file = "data/child_oriented_graph.Rdata")
save(graph_WG, file = "data/child_oriented_graph_wg.Rdata")

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
  return(list(graph = graph_from_adjacency_matrix(ind_adj_mat,mode = "directed"),form = x$form))
})

save(vocab_graphs,file = "data/individual_networks.Rdata")


ind_noun_graphs <- map(vocab_splits_produced, function(x){
  map(feat_mats, function(y){
    ind_adj_mat <- as.matrix(y$adj_mat[rownames(y$adj_mat) %in% x$CDI_Metadata_compatible,colnames(y$adj_mat) %in% x$CDI_Metadata_compatible])
    return(list(graph = graph_from_adjacency_matrix(ind_adj_mat,mode = "directed"),form = x$form))
  })
})

save(ind_noun_graphs,file = "data/individual_noun_networks.Rdata")


