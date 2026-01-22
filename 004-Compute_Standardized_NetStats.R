library(igraph)
library(tidyverse)
source("R/netStats.R")
source("R/Random_Networks.R")
## Load random stats and vocab networks
ran_stats <- read_rds("data/all_RAN_stats.rds")
load("data/individual_networks.Rdata")


## Compute true netstats
true_stats <- map_dfr(vocab_graphs, function(x){
  network_stats(x$graph)
}, .id = "subjectkey")


z <- function(x){
  return((x$value[x$stat == "truestat"] -x$value[x$stat == "mean"] )/x$value[x$stat == "sd"])
}

all_Netstats_z <- true_stats %>% 
  left_join(ran_stats) %>%
  pivot_longer(cols = starts_with(c("indegree","clustcoef","meandist")),
               names_to = c("metric","source","stat"),
               values_to = "value",
               names_sep = "_") %>%
  mutate(source = ifelse(is.na(source), "truenet", source),
         stat = ifelse(is.na(stat),"truestat",stat)) %>%
  group_by(subjectkey,metric) %>%
  mutate(z = (value[stat == "truestat"] - value[stat == "mean"])/value[stat == "sd"])


write_rds(all_Netstats_z, "data/all_Netstats_z.rds")



