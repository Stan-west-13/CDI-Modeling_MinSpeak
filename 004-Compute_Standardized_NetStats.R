library(igraph)
library(tidyverse)
source("R/netStats.R")
source("R/Random_Networks.R")
## Load random stats and vocab networks all words associations
ran_stats <- read_rds("data/all_RAN_stats.rds")
load("data/individual_networks.Rdata")

## Load random stats and vocab networks noun associations

ran_stats_assoc_nouns <- read_rds("data/all_RAN_stats_assoc_nouns.rds")
load("data/association_ind_graphs_nouns.Rdata")

## Load random stats and vocab networks (just noun features)
ran_stats_nouns <- read_rds("data/all_RAN_stats_noun.rds")
load("data/ind_noun_graphs_anyfeat.Rdata")

## Load random stats and vocab networks (noun features TAR filtered)
ran_stats_nouns_TAR <- read_rds("data/all_RAN_stats_noun_TRA.rds")
load("data/ind_noun_graphs_anyfeat_TRA.Rdata")




## Compute true netstats
## For associations
true_stats <- map_dfr(vocab_graphs, function(x){
  network_stats(x$graph)
}, .id = "subjectkey_intAge")


## For associations only nouns
true_stats_assoc_N <-map_dfr(vocab_graphs_association_nouns, function(x){
  d <- network_stats(x$graph) 
  d$nProduced <- vcount(x$graph)
  return(d)
}, .id = "subjectkey_intAge")


## For nouns features
true_stats_N <- map_dfr(ind_noun_graphs_anyfeat, function(x){
  d <- network_stats(x$graph) 
  d$nProduced <- vcount(x$graph)
  return(d)
}, .id = "subjectkey_intAge")

## For TAR filtered nouns
true_stats_N_TAR <- map_dfr(ind_noun_graphs_anyfeat_TRA, function(x){
  d <- network_stats(x$graph) 
  d$nProduced <- vcount(x$graph)
  return(d)
}, .id = "subjectkey_intAge")


z <- function(x){
  return((x$value[x$stat == "truestat"] -x$value[x$stat == "mean"] )/x$value[x$stat == "sd"])
}

all_Netstats_z <- true_stats %>% 
  left_join(ran_stats) %>%
  pivot_longer(cols = starts_with(c("indegree","clustcoef","meandist")),
               names_to = c("metric","origin","stat"),
               values_to = "value",
               names_sep = "_") %>%
  mutate(origin = ifelse(is.na(origin), "truenet", origin),
         stat = ifelse(is.na(stat),"truestat",stat)) %>%
  group_by(subjectkey_intAge,metric) %>%
  mutate(z = (value[stat == "truestat"] - value[stat == "mean"])/value[stat == "sd"])


write_rds(all_Netstats_z, "data/all_Netstats_z.rds")

## Noun netstats
all_Netstats_z_N <- true_stats_N %>% 
  left_join(ran_stats_nouns) %>%
  pivot_longer(cols = starts_with(c("indegree","clustcoef","meandist")),
               names_to = c("metric","origin","stat"),
               values_to = "value",
               names_sep = "_") %>%
  mutate(origin = ifelse(is.na(origin), "truenet", origin),
         stat = ifelse(is.na(stat),"truestat",stat)) %>%
  group_by(subjectkey_intAge,metric) %>%
  mutate(z = (value[stat == "truestat"] - value[stat == "mean"])/value[stat == "sd"])


write_rds(all_Netstats_z_N, "data/all_Netstats_z_nouns.rds")

## Noun TAR stats
all_Netstats_z_N_TAR <- true_stats_N_TAR %>% 
  left_join(ran_stats_nouns_TAR) %>%
  pivot_longer(cols = starts_with(c("indegree","clustcoef","meandist")),
               names_to = c("metric","origin","stat"),
               values_to = "value",
               names_sep = "_") %>%
  mutate(origin = ifelse(is.na(origin), "truenet", origin),
         stat = ifelse(is.na(stat),"truestat",stat)) %>%
  group_by(subjectkey_intAge,metric) %>%
  mutate(z = (value[stat == "truestat"] - value[stat == "mean"])/value[stat == "sd"])


write_rds(all_Netstats_z_N_TAR, "data/all_Netstats_z_N_TAR.rds")


## Association noun netstats

all_Netstats_z_assoc_N <- true_stats_assoc_N %>% 
  left_join(ran_stats_assoc_nouns) %>%
  pivot_longer(cols = starts_with(c("indegree","clustcoef","meandist")),
               names_to = c("metric","origin","stat"),
               values_to = "value",
               names_sep = "_") %>%
  mutate(origin = ifelse(is.na(origin), "truenet", origin),
         stat = ifelse(is.na(stat),"truestat",stat)) %>%
  group_by(subjectkey_intAge,metric) %>%
  mutate(z = (value[stat == "truestat"] - value[stat == "mean"])/value[stat == "sd"])


write_rds(all_Netstats_z_assoc_N, "data/all_Netstats_z_assoc_nouns.rds")


## Join in metadata
vocab <- readRDS("data/combined_CDILetti.rds")
all_Netstats_z_meta <- all_Netstats_z %>%
  left_join(unique(select(vocab, subjectkey_intAge, interview_age, nProduced, form, group,source)))

all_Netstats_z_meta_nouns <- all_Netstats_z_N %>%
  left_join(unique(select(vocab, subjectkey_intAge, interview_age, form, group,source)))

all_Netstats_z_meta_nouns_TAR <- all_Netstats_z_N_TAR %>%
  left_join(unique(select(vocab, subjectkey_intAge, interview_age, form, group,source)))


all_Netstats_z_meta_assoc_nouns <- all_Netstats_z_assoc_N %>%
  left_join(unique(select(vocab, subjectkey_intAge, interview_age, form, group,source)))



saveRDS(all_Netstats_z_meta, "data/all_Netstats_z_meta.rds")
saveRDS(all_Netstats_z_meta_nouns, "data/all_Netstats_z_meta_nouns.rds")
saveRDS(all_Netstats_z_meta_assoc_nouns, "data/all_Netstats_z_meta_assoc_nouns.rds")
saveRDS(all_Netstats_z_meta_nouns_TAR, "data/all_Netstats_z_meta_nouns_TAR.rds")




