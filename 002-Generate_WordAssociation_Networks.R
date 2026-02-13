#devtools::install_github("dgrtwo/fuzzyjoin",force = TRUE)

library(tidyverse)
library(igraph)
library(purrr)
library(fuzzyjoin)
library(tibble)

## Association network functions
## Fuzzy matching packages
source("R/assocNetwork.R")

noun_net <- function(x){
  d <- xtabs(~definition+Feature, data = x,drop.unused.levels = F)
  adj_mat <- ifelse(d %*% t(d) > 0,1,0)
  diag(adj_mat) <- 0
  return(list(feat_df = d, adj_mat = adj_mat, graph = igraph::simplify(graph_from_adjacency_matrix(adj_mat,mode = "undirected"))))
}


## Load vocabulary and association data
load("data/associations-child.Rdata")
vocab_data <- read_rds("data/combined_CDILetti.rds")
cdi_WG <- read_rds("data/cdi_WG.rds")
noun_feats <- read.csv("data/MBCDI_concsFeats_2022-07-14.csv")
noun_feats_TRA <- noun_feats %>%
  filter(Toddler_Access > 4)



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
sum(unique(noun_feats$definition) %in% unique(vocab_data$CDI_Metadata_compatible))
noun_feats$definition[noun_feats$definition =="tv"] <- "TV" #fixing discrepancy
noun_feats_TRA$definition[noun_feats_TRA$definition =="tv"] <- "TV" #fixing discrepancy




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

## Noun Different feature lists: by Label
feats_maximal <- expand.grid(definition = unique(noun_feats$definition),Label = unique(noun_feats$BR_Label)) %>%
  left_join(select(noun_feats,definition, BR_Label,Feature), by = c("definition", "Label" = "BR_Label"))
feats_list <- split(feats_maximal, feats_maximal$Label,drop = FALSE)


feat_mats <- map(feats_list, ~noun_net(.x))

## By any feature
noun_network <- noun_net(noun_feats)
noun_network_TRA <- noun_net(noun_feats_TRA)
save(feat_mats, file = "data/feature_graphs.Rdata")
save(noun_network, file = "data/graph_full_nounNet.Rdata")
save(noun_network_TRA, file = "data/graph_full_nounNet_TRA.Rdata")


graph_association_nouns <- induced.subgraph(graph = graph_FullNet, vids = names(V(noun_network$graph)))
noun_assoc_adj_mat <- adj_mat[rownames(adj_mat) %in% names(V(graph_association_nouns)),colnames(adj_mat) %in% names(V(graph_association_nouns))]

tmp <- noun_feats %>% 
  filter(definition %in% names(V(graph_WG))) %>% 
  select(definition) %>% 
  unique()
tmp_TRA <- noun_feats_TRA %>% 
  filter(definition %in% names(V(graph_WG))) %>% 
  select(definition) %>% 
  unique()


graph_association_nouns_WG <- induced.subgraph(graph = graph_WG, vids = tmp$definition)
noun_network_WG <- induced.subgraph(noun_network$graph, vids = tmp$definition)
noun_network_WG_TRA <-  induced_subgraph(noun_network_TRA$graph, vids = tmp_TRA$definition)






save(noun_network_WG_TRA, file = "data/noun_network_WG_TRA.Rdata")
save(noun_network_WG, file = "data/noun_network_WG.Rdata")
save(graph_association_nouns, file = "data/association_graph_nouns.Rdata")
save(graph_association_nouns_WG, file = "data/association_graph_nouns_WG.Rdata")
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

## Construct individual noun association networks
vocab_graphs_association_nouns <- map(vocab_splits_produced, function(x){
  x <- x %>%
    filter(lexical_class == "nouns")
  ind_adj_mat <- as.matrix(noun_assoc_adj_mat[rownames(noun_assoc_adj_mat) %in% x$CDI_Metadata_compatible,colnames(noun_assoc_adj_mat) %in% x$CDI_Metadata_compatible])
  return(list(graph = graph_from_adjacency_matrix(ind_adj_mat,mode = "directed"),form = x$form))
})

save(vocab_graphs_association_nouns, file = "data/association_ind_graphs_nouns.Rdata")

save(vocab_graphs,file = "data/individual_networks.Rdata")

## Individual noun networks by label
ind_noun_graphs <- map(vocab_splits_produced, function(x){
  map(feat_mats, function(y){
    ind_adj_mat <- as.matrix(y$adj_mat[rownames(y$adj_mat) %in% x$CDI_Metadata_compatible,colnames(y$adj_mat) %in% x$CDI_Metadata_compatible])
    return(list(graph = graph_from_adjacency_matrix(ind_adj_mat,mode = "undirected"),form = x$form))
  })
})

save(ind_noun_graphs,file = "data/individual_noun_networks.Rdata")

## Individual noun networks any feature
ind_noun_graphs_anyfeat <- map(vocab_splits_produced, function(x){
  x <- x %>%
    filter(lexical_class == "nouns")
  ind_adj_mat <- as.matrix(noun_network$adj_mat[rownames(noun_network$adj_mat) %in% x$CDI_Metadata_compatible,colnames(noun_network$adj_mat) %in% x$CDI_Metadata_compatible])
  return(list(graph = graph_from_adjacency_matrix(ind_adj_mat,mode = "undirected"),form = x$form))
})

save(ind_noun_graphs_anyfeat,file = "data/ind_noun_graphs_anyfeat.Rdata")

## Individual noun networks any feature
ind_noun_graphs_anyfeat_TRA <- map(vocab_splits_produced, function(x){
  x <- x %>%
    filter(lexical_class == "nouns")
  ind_adj_mat <- as.matrix(noun_network_TRA$adj_mat[rownames(noun_network_TRA$adj_mat) %in% x$CDI_Metadata_compatible,colnames(noun_network_TRA$adj_mat) %in% x$CDI_Metadata_compatible])
  return(list(graph = graph_from_adjacency_matrix(ind_adj_mat,mode = "undirected"),form = x$form))
})

save(ind_noun_graphs_anyfeat_TRA,file = "data/ind_noun_graphs_anyfeat_TRA.Rdata")




