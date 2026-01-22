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
cdi <- readRDS("data/cdi-metadata.rds")
cdi$POS <- ifelse(cdi$lexical_class == "nouns"|cdi$lexical_class=="verbs",cdi$lexical_class, "other")


## List with vocab size, graph, and POS
ran_list <- map(vocab_graphs, function(x){
  vocab <- names(as_adj_list(x$graph))
  vocab_size <- length(names(as_adj_list(x$graph)))
  POS <- data.frame(word = vocab) %>%
    left_join(select(cdi,CDI_Metadata_compatible,POS), by = c("word" = "CDI_Metadata_compatible")) 
  POS <- table(factor(POS$POS, levels = c("nouns","other","verbs")))
  
  return(list(vocab = vocab,vocab_size = vocab_size, graph = x$graph, POS = POS))                  
})

POS_numbers_ASD <- map(ran_list,function(x){
  return(x$POS)
})


POS_numbers_df <- map_dfr(POS_numbers_ASD, .f = data.frame, .id = "name") %>%
  pivot_wider(id_cols = name,
              values_from = Freq,
              names_from = Var1)


## POS Vertices
vertices_CoxHae_toddler <- data.frame(vId_CoxHae_toddler = seq_len(vcount(graph_FullNet)),
                                      CDI_Metadata_compatible = names(V(graph_FullNet))) ## Make vertices IDs

vertices_POS_child <- merge(vertices_CoxHae_toddler, select(cdi, c("CDI_Metadata_compatible", "POS"))) ## Merge in POS

cdi <- merge(cdi, vertices_CoxHae_toddler, by = "CDI_Metadata_compatible", all.x = TRUE) ## Merge vertices into CDI

vertices_CoxHae_toddler$vId_CoxHae_toddler <- as.numeric(vertices_CoxHae_toddler$vId_CoxHae_toddler) ## Convert to numeric
vertices_POS_child <- subset_CDI_NAs(cdi$POS, cdi$vId_CoxHae_toddler, cdi$num_item_id)
save(vertices_POS_child, file = "data/vertices_POS_child.Rdata")

## RAN Simulations
load("data/vertices_POS_child.Rdata")

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
invisible(clusterExport(cl, c("vocab_graphs", "graph_FullNet", "vertices_POS_child",
                              "balanced_RAN_network", "network_stats", "balanced_RAN_stats", "multiSample", "indegree_igraph")))

# Finally, run with parLapply. Running 1000 replications takes less than two minutes.
starttime <- proc.time()
stats_ASD_RAN <- parLapply(cl, POS_numbers_ASD,
                           function(n,G,POS) replicate(1000, balanced_RAN_stats(n,G,POS)),
                           G = graph_FullNet, POS = as.factor(vertices_POS_child$POS))

save(stats_ASD_RAN, file = "data/Child_Stats_ASD_balRAN_1000.Rdata")



stoptime <- proc.time()
print(stoptime - starttime)

# All done, so release the workers
stopCluster(cl)


## Format RAN stats
mean_RAN <- stats_ASD_RAN %>% 
  map_dfr(., .f = rowMeans, na.rm = TRUE, .id = "subjectkey") %>%
  rename(indegreeavg_RAN_mean = indegree_mean,
         indegreemed_RAN_mean = indegree_median,
         clustcoef_RAN_mean = clustcoef,
         meandist_RAN_mean = meandist)

sd_RAN <- stats_ASD_RAN %>% 
  map_dfr(., .f = row_SD,  .id = "subjectkey") %>%
  rename(indegreeavg_RAN_sd = indegree_mean,
         indegreemed_RAN_sd = indegree_median,
         clustcoef_RAN_sd = clustcoef,
         meandist_RAN_sd = meandist)


## Descriptives for 1000 RAN iterations
all_RAN_stats <- left_join(mean_RAN,sd_RAN)

write_rds(all_RAN_stats, "data/all_RAN_stats.rds")












