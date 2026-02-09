library(igraph)
library(tidyverse)
source("R/netStats.R")
source("R/Random_Networks.R")
## Load random stats and vocab networks all words
ran_stats <- read_rds("data/all_RAN_stats.rds")
load("data/individual_networks.Rdata")

## Load random stats and vocab networks (just noun features)
ran_stats_nouns <- read_rds("data/all_RAN_stats_noun.rds")
load("data/ind_noun_graphs_anyfeat.Rdata")


## Compute true netstats
true_stats <- map_dfr(vocab_graphs, function(x){
  network_stats(x$graph)
}, .id = "subjectkey_intAge")


true_stats_N <- map_dfr(ind_noun_graphs_anyfeat, function(x){
  network_stats(x$graph)
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
  left_join(ran_stats) %>%
  pivot_longer(cols = starts_with(c("indegree","clustcoef","meandist")),
               names_to = c("metric","origin","stat"),
               values_to = "value",
               names_sep = "_") %>%
  mutate(origin = ifelse(is.na(origin), "truenet", origin),
         stat = ifelse(is.na(stat),"truestat",stat)) %>%
  group_by(subjectkey_intAge,metric) %>%
  mutate(z = (value[stat == "truestat"] - value[stat == "mean"])/value[stat == "sd"])


write_rds(all_Netstats_z_N, "data/all_Netstats_z_nouns.rds")



## Join in metadata
vocab <- readRDS("data/combined_CDILetti.rds")
all_Netstats_z_meta <- all_Netstats_z %>%
  left_join(unique(select(vocab, subjectkey_intAge, interview_age, nProduced, form, group,source)))

all_Netstats_z_meta_nouns <- all_Netstats_z_N %>%
  left_join(unique(select(vocab, subjectkey_intAge, interview_age, nProduced, form, group,source)))


saveRDS(all_Netstats_z_meta, "data/all_Netstats_z_meta.rds")
saveRDS(all_Netstats_z_meta_nouns, "data/all_Netstats_z_meta_nouns.rds")




