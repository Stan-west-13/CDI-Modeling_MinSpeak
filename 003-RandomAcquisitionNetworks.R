library(furrr)
library(future)
library(purrr)
library(tidyverse)
library(igraph)
library(tibble)
library(parallel)
library(assertthat)
## RAN functions 
source("R/Random_Networks.R")
source("R/netStats.R")


subset_CDI_NAs <- function(POS, v_ID, num_item_id){
  assertthat::are_equal(length(POS), length(v_ID))
  assertthat::assert_that(is.numeric(v_ID))
  assertthat::are_equal(length(POS), length(num_item_id))
  z <- num_item_id[!is.na(v_ID)]
  y <- POS[!is.na(v_ID)]
  x <- v_ID[!is.na(v_ID)]
  return(data.frame("v_ID" = x[order(x)], "POS" = y[order(x)], "num_item_id" = z[order(x)]))
}

row_SD <- function(d){
  return(apply(d, 1, sd, na.rm = TRUE))
}

## Individual graphs
load("data/individual_networks.Rdata")
load("data/child_oriented_graph.Rdata")
load("data/child_oriented_graph_wg.Rdata")
load("data/ind_noun_graphs_anyfeat.Rdata")
load("data/graph_full_nounNet.Rdata")
cdi <- readRDS("data/cdi-metadata.rds")
cdi_WG <- readRDS("data/cdi_WG.rds")
cdi$POS <- ifelse(cdi$lexical_class == "nouns"|cdi$lexical_class=="verbs",cdi$lexical_class, "other")


## List with vocab size, graph, and POS
ran_list <- map(vocab_graphs, function(x){
  vocab <- names(as_adj_list(x$graph))
  vocab_size <- length(names(as_adj_list(x$graph)))
  POS <- data.frame(word = vocab) %>%
    left_join(select(cdi,CDI_Metadata_compatible,POS), by = c("word" = "CDI_Metadata_compatible")) 
  POS <- table(factor(POS$POS, levels = c("nouns","other","verbs")))
  
  return(list(vocab = vocab,vocab_size = vocab_size, graph = x$graph, POS = POS, form = x$form))                  
})




POS_numbers_ASD <- map(ran_list,function(x){
  return(list(POS = x$POS, form = x$form))
})


 POS_numbers_df <- map_dfr(POS_numbers_ASD, function(x){
   d <- data.frame(noun = x$POS["nouns"], verb = x$POS["verbs"], other = x$POS["other"])
   return(d)
 })

## POS Vertices WS
vertices_CoxHae_toddler <- data.frame(vId_CoxHae_toddler = seq_len(vcount(graph_FullNet)),
                                      CDI_Metadata_compatible = names(V(graph_FullNet))) ## Make vertices IDs

vertices_POS_child <- merge(vertices_CoxHae_toddler, select(cdi, c("CDI_Metadata_compatible", "POS"))) ## Merge in POS

cdi <- merge(cdi, vertices_CoxHae_toddler, by = "CDI_Metadata_compatible", all.x = TRUE) ## Merge vertices into CDI

vertices_CoxHae_toddler$vId_CoxHae_toddler <- as.numeric(vertices_CoxHae_toddler$vId_CoxHae_toddler) ## Convert to numeric
vertices_POS_child <- subset_CDI_NAs(cdi$POS, cdi$vId_CoxHae_toddler, cdi$num_item_id)

save(vertices_POS_child, file = "data/vertices_POS_child.Rdata")

## POS Vertices WG
vertices_CoxHae_toddler_wg <- data.frame(vId_CoxHae_toddler_wg = seq_len(vcount(graph_WG)),
                                      CDI_Metadata_compatible = names(V(graph_WG))) ## Make vertices IDs

vertices_POS_child_wg <- merge(vertices_CoxHae_toddler_wg, select(cdi, c("CDI_Metadata_compatible", "POS"))) ## Merge in POS

cdi <- merge(cdi, vertices_CoxHae_toddler_wg, by = "CDI_Metadata_compatible", all.x = TRUE) ## Merge vertices into CDI

vertices_CoxHae_toddler_wg$vId_CoxHae_toddler <- as.numeric(vertices_CoxHae_toddler_wg$vId_CoxHae_toddler_wg) ## Convert to numeric
vertices_POS_child_wg <- subset_CDI_NAs(cdi$POS, cdi$vId_CoxHae_toddler_wg, cdi$num_item_id)

save(vertices_POS_child_wg, file = "data/vertices_POS_child_wg.Rdata")



## RAN Simulations
load("data/vertices_POS_child.Rdata")
load("data/vertices_POS_child_wg.Rdata")
# Define the cluster, using all by two cores on the system. This should give
# you 30 "workers". Before you do this, start "top" in another terminal window
# to see what happens.
cl <- makeCluster(detectCores()-2)

# Setting the Random Number Generator Stream for each worker will make sure
# that random processes behave differently on each.
clusterSetRNGStream(cl)



# Export everything needed to run the operation, including any data or
# functions you have defined. If a function you have written relies on a
# package, make sure to use the package::function referencing.
invisible(clusterExport(cl, c("vocab_graphs", "graph_FullNet", "vertices_POS_child","vertices_POS_child_wg",
                              "balanced_RAN_network", "network_stats", "balanced_RAN_stats", "multiSample", "indegree_igraph")))

# Finally, run with parLapply. Running 1000 replications takes less than two minutes.
starttime <- proc.time()
stats_ASD_RAN <- parLapply(cl, POS_numbers_ASD,
                           function(n,G,G_WG,POS,POS_wg) replicate(1000, balanced_RAN_stats_n(n,G,G_WG,POS,POS_wg)),
                           G = graph_FullNet,G_WG = graph_WG, POS = as.factor(vertices_POS_child$POS),POS_wg = as.factor(vertices_POS_child_wg$POS))

save(stats_ASD_RAN, file = "data/Child_Stats_ASD_balRAN_1000.Rdata")



stoptime <- proc.time()
print(stoptime - starttime)

# All done, so release the workers
stopCluster(cl)


## Format RAN stats
mean_RAN <- stats_ASD_RAN %>% 
  map_dfr(., .f = rowMeans, na.rm = TRUE, .id = "subjectkey_intAge") %>%
  rename(indegreeavg_RAN_mean = indegreeavg,
         indegreemed_RAN_mean = indegreemed,
         clustcoef_RAN_mean = clustcoef,
         meandist_RAN_mean = meandist)

sd_RAN <- stats_ASD_RAN %>% 
  map_dfr(., .f = row_SD,  .id = "subjectkey_intAge") %>%
  rename(indegreeavg_RAN_sd = indegreeavg,
         indegreemed_RAN_sd = indegreemed,
         clustcoef_RAN_sd = clustcoef,
         meandist_RAN_sd = meandist)


## Descriptives for 1000 RAN iterations
all_RAN_stats <- left_join(mean_RAN,sd_RAN)

write_rds(all_RAN_stats, "data/all_RAN_stats.rds")





###################################################################################

## List with vocab size, graph, and POS
ran_list_noun <- map(ind_noun_graphs_anyfeat, function(x){
  vocab <- names(as_adj_list(x$graph))
  vocab_size <- length(names(as_adj_list(x$graph)))
  POS <- data.frame(word = vocab) %>%
    mutate(POS = "nouns") 
  POS <- table(factor(POS$POS, levels = c("nouns")))
  
  return(list(vocab = vocab,vocab_size = vocab_size, graph = x$graph, POS = POS ))#POS = POS, form = x$form))                  
})




POS_numbers_ASD_noun <- map(ran_list_noun,function(x){
  return(list(POS = x$POS, form = x$form))
})

POS_numbers_df_noun <- map_dfr(POS_numbers_ASD_noun, function(x){
  d <- data.frame(noun = x$POS["nouns"], verb = x$POS["verbs"], other = x$POS["other"])
  return(d)
},.id = "name")


## POS Vertices nouns
vertices_CoxHae_toddler_noun <- data.frame(vId_CoxHae_toddler_noun = seq_len(vcount(noun_network$graph)),
                                      CDI_Metadata_compatible = names(V(noun_network$graph))) ## Make vertices IDs

vertices_POS_child_noun <- vertices_CoxHae_toddler_noun %>%
  mutate(POS = "nouns") %>%
  left_join(select(cdi, num_item_id, CDI_Metadata_compatible))## Merge in POS

vertices_POS_child_noun <- subset_CDI_NAs(vertices_POS_child_noun$POS, vertices_POS_child_noun$vId_CoxHae_toddler_noun, vertices_POS_child_noun$num_item_id)

save(vertices_POS_child_noun, file = "data/vertices_POS_child_noun.Rdata")




## RAN Simulations Nouns
load("data/vertices_POS_child_noun.Rdata")

# Define the cluster, using all by two cores on the system. This should give
# you 30 "workers". Before you do this, start "top" in another terminal window
# to see what happens.
cl <- makeCluster(detectCores()-2)

# Setting the Random Number Generator Stream for each worker will make sure
# that random processes behave differently on each.
clusterSetRNGStream(cl)



# Export everything needed to run the operation, including any data or
# functions you have defined. If a function you have written relies on a
# package, make sure to use the package::function referencing.
invisible(clusterExport(cl, c("ind_noun_graphs_anyfeat", "noun_network", "vertices_POS_child_noun",
                              "random_acq_network_igraph", "network_stats", "balanced_RAN_stats_noun", "multiSample", "indegree_igraph")))

# Finally, run with parLapply. Running 1000 replications takes less than two minutes.
starttime <- proc.time()
stats_ASD_RAN_noun <- parLapply(cl, POS_numbers_ASD_noun,
                           function(vocab_size,G) replicate(1000, network_stats(random_acq_network_igraph(vocab_size$POS,G))),
                           G = noun_network$graph)


save(stats_ASD_RAN_noun, file = "data/Noun_Child_Stats_ASD_balRAN_1000.Rdata")



stoptime <- proc.time()
print(stoptime - starttime)

# All done, so release the workers
stopCluster(cl)


## Format RAN stats
mean_RAN_noun <- stats_ASD_RAN_noun %>% 
  map_dfr(., .f = rowMeans, na.rm = TRUE, .id = "subjectkey_intAge") %>%
  rename(indegreeavg_RAN_mean = indegreeavg,
         indegreemed_RAN_mean = indegreemed,
         clustcoef_RAN_mean = clustcoef,
         meandist_RAN_mean = meandist)

sd_RAN_noun <- stats_ASD_RAN_noun %>% 
  map_dfr(., .f = row_SD,  .id = "subjectkey_intAge") %>%
  rename(indegreeavg_RAN_sd = indegreeavg,
         indegreemed_RAN_sd = indegreemed,
         clustcoef_RAN_sd = clustcoef,
         meandist_RAN_sd = meandist) 


## Descriptives for 1000 RAN iterations
all_RAN_stats_noun <- left_join(mean_RAN_noun,sd_RAN_noun) %>%
  select(-indegreemed.NA)

write_rds(all_RAN_stats_noun, "data/all_RAN_stats_noun.rds")





