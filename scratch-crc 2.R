library(dplyr)
library(purrr)
library(tidyr)
library(igraph)

source("R/assocNetwork.R")

# Load vocabulary and association data ----
assoc_child <- readr::read_rds("data/associations-child.rds") |> as_tibble()
vocab_data <- readr::read_rds("data/combined_CDILetti.rds") |> ungroup()
noun_feats <- readr::read_csv("data/MBCDI_concsFeats_2022-07-14.csv")
cdi_items <- readr::read_rds("data/cdi-item-data.rds") |> as_tibble()

## Create cdi_items "cue" column to match assoc_child "CUE" column ----
cdi_items <- cdi_items |>
  mutate(
    cue = word,
    cue = stringr::str_remove(cue, "\\*"),
    cue = if_else(cue == "buttocks/bottom", "buttocks", cue),
    cue = if_else(cue == "dress (object)", "dress", cue),
    cue = if_else(cue == "park", "park (place)", cue),
    cue = if_else(cue == "sink", "sink (noun)", cue),
    cue = if_else(cue == "tissue/kleenex", "tissue", cue),
    cue = if_else(cue == "soda/pop", "soda", cue),
    cue = if_else(cue == "block", "block (object)", cue),
    cue = if_else(cue == "toy (object)", "toy", cue),
    cue = if_else(cue == "slide (action)", "slide (verb)", cue),
    cue = if_else(cue == "wake", "wake (verb)", cue),
    cue = if_else(cue == "tear", "tear (verb)", cue),
    cue = if_else(cue == "close", "close (verb)", cue),
    cue = if_else(cue == "little (description)", "little", cue),
    cue = if_else(cue == "TV", "tv", cue),
    cue = if_else(cue == "try/try to", "try", cue),
    cue = if_else(cue == "did/did ya", "did", cue),
    cue = if_else(cue == "need/need to", "need", cue),
    cue = if_else(cue == "inside/in", "inside", cue),
    cue = if_else(cue == "turn around", "turn", cue),
    cue = if_else(cue == "yum yum", "yum", cue),
    cue = if_else(cue == "woof woof", "woof", cue),
    cue = if_else(cue == "I", "i", cue),
    cue = if_else(cue == "pet's name", "pet (noun)", cue),
  )

all(
  unique(as.character(assoc_child$CUE)) %in%
    unique(as.character(cdi_items$cue))
)

sum(
  unique(as.character(assoc_child$CUE)) %in%
    unique(as.character(cdi_items$cue))
)

## Create noun_feats "cue" column to match assoc_child "CUE" column ----
noun_feats <- noun_feats |>
  mutate(
    cue = definition,
    cue = stringr::str_remove(cue, "\\*"),
    cue = if_else(cue == "buttocks/bottom", "buttocks", cue),
    cue = if_else(cue == "dress (object)", "dress", cue),
    cue = if_else(cue == "park", "park (place)", cue),
    cue = if_else(cue == "sink", "sink (noun)", cue),
    cue = if_else(cue == "tissue/kleenex", "tissue", cue),
    cue = if_else(cue == "soda/pop", "soda", cue),
    cue = if_else(cue == "block", "block (object)", cue),
    cue = if_else(cue == "toy (object)", "toy", cue)
  )

all(
  unique(as.character(noun_feats$cue)) %in%
    unique(as.character(assoc_child$CUE))
)

sum(
  unique(as.character(noun_feats$cue)) %in%
    unique(as.character(assoc_child$CUE))
)


# Build networks ----

## Association Networks (all words)
a_assoc_ws <- assoc_child |> 
  select(cue = CUE, resp = RESPONSE) |>
  assocNetwork_noLoops()

g_assoc_ws <- a_assoc_ws |>
  igraph::graph_from_adjacency_matrix(mode = "directed", diag = FALSE) |>
  igraph::simplify()

vid_assoc_ws <- tibble(
  vid = seq_len(vcount(g_assoc_ws)),
  cue = names(V(g_assoc_ws))
) |>
  left_join(cdi_items |> select(cue, on_WG), by = join_by(cue == cue))

g_assoc_wg <- vid_assoc_ws |>
  filter(on_WG) |>
  pull(vid) |>
  induced_subgraph(g_assoc_ws, vids = _)

vid_assoc_wg <- tibble(
  vid = seq_len(vcount(g_assoc_wg)),
  cue = names(V(g_assoc_wg))
) |>
  left_join(cdi_items |> select(cue, on_WG), by = join_by(cue == cue))


## Feature Networks (nouns)
g_feat_ws <- noun_feats |>
  filter(Toddler_Access > 4) |>
  xtabs(~ Feature + cue, data = _) |>
  crossprod() |>
  igraph::graph_from_adjacency_matrix(mode = "undirected", diag = FALSE) |>
  igraph::simplify()


vid_feat_ws <- tibble(
  vid = seq_len(vcount(g_feat_ws)),
  cue = names(V(g_feat_ws))
) |>
  left_join(cdi_items |> select(cue, on_WG), by = join_by(cue == cue))

g_feat_wg <- vid_feat_ws |>
  filter(on_WG) |>
  pull(vid) |>
  induced_subgraph(g_feat_ws, vids = _)

vid_feat_wg <- tibble(
  vid = seq_len(vcount(g_feat_wg)),
  cue = names(V(g_feat_wg))
) |>
  left_join(cdi_items |> select(cue, on_WG), by = join_by(cue == cue))


## Association Networks (nouns) ----
g_assoc_ws_shared <- which(names(V(g_assoc_ws)) %in% names(V(g_feat_ws))) |>
  igraph::induced_subgraph(g_assoc_ws, vids = _)

vid_assoc_ws_shared <- tibble(
  vid = seq_len(vcount(g_assoc_ws_shared)),
  cue = names(V(g_assoc_ws_shared))
) |>
  left_join(cdi_items |> select(cue, on_WG), by = join_by(cue == cue))

g_assoc_wg_shared <- vid_assoc_ws_shared |>
  filter(on_WG) |>
  pull(vid) |>
  induced_subgraph(g_assoc_ws_shared, vids = _)


# Data quality inspection ----

## Duplicated subjects ----
# After replacing some words and filtering, there are 3 subjects with impossible
# vocabulary sizes 
vocab_data_first_record <- vocab_data |>
  select(subjectkey, group, form, sex, interview_age, interview_date, CDI_ID, cue = cue_CoxHae, Produces) |> 
  filter(cue %in% names(V(g_feat_ws)), Produces) |>
  select(-Produces) |>
  group_by(subjectkey) |>
  filter(interview_age == min(interview_age)) |>
  ungroup()
  
vocab_data_first_record |>
  count(subjectkey, form, sex, interview_age, interview_date) |>
  filter(n > 400)

# All three subjects were double-entered
vocab_data_first_record |>
  filter(subjectkey == "NDAR_INVMA034ZBX") |>
  count(subjectkey, form, interview_date, interview_age, cue) |>
  count(n)

vocab_data_first_record |>
  filter(subjectkey == "NDAR_INVHD785FP6") |>
  count(subjectkey, form, interview_date, interview_age, cue) |>
  count(n)

vocab_data_first_record |>
  filter(subjectkey == "NDARVZ466UF3") |>
  count(subjectkey, form, interview_date, interview_age, cue) |>
  count(n)

# Since everyone else is uniquely identified by key, form, sex, interview age,
# and interview date, these cases can be addressed by retaining the distinct
# words for each.

vocab_data_first_record <- vocab_data_first_record |>
  distinct() %>%
  group_by(subjectkey) %>%
  slice_min(interview_date,n = 1)

vocab_data_first_record |>
  count(subjectkey, form, sex, interview_age, interview_date) |>
  filter(n > 400)


# Compose vocab_data_shared ----
vid_feat_forms <- bind_rows(WS = vid_feat_ws, WG = vid_feat_wg, .id = "form")

vocab_data_first_record_shared <- vocab_data_first_record |>
  add_count(subjectkey, form, sex, interview_age, interview_date, name = "nproduced") |>
  left_join(vid_feat_forms, by = join_by(form, cue == cue)) |>
  select(-CDI_ID) |>
  nest(vocab = c(vid, cue, on_WG))

vocab_data_first_record_shared |>
  pull(nproduced) |>
  hist()


# RAN simulations ----
g_feat <- list(WS = g_feat_ws, WG = g_feat_wg)
g_assoc_shared <- list(WS = g_assoc_ws_shared, WG = g_assoc_wg_shared)


vocab_stats <- function(vids, g_feat, g_assoc_shared) {
    vid_sample <- sample(vcount(g_feat), length(vids))
    g_feat_sample <- induced_subgraph(g_feat, vid_sample)
    g_assoc_sample <- induced_subgraph(g_assoc_shared, vid_sample)
    c(
        feat_degree = median(degree(g_feat_sample)),
        feat_clust = transitivity(g_feat_sample, type = "global"),
        feat_dist = mean_distance(g_feat_sample),
        assoc_degree = median(degree(g_assoc_sample, mode = "in")),
        assoc_clust = transitivity(g_assoc_sample, type = "global"),
        assoc_dist = mean_distance(g_assoc_sample)
    )
}


vocab_data_first_record_shared <- vocab_data_first_record_shared |>
  mutate(netstats = map2(vocab, form, \(x, form) {
    g_feat_vocab <- induced_subgraph(g_feat[[form]], vids = x$vid)
    g_assoc_vocab <- induced_subgraph(g_assoc_shared[[form]], vids = x$vid)
    ran = replicate(1000, vocab_stats(x$vid, g_feat[[form]], g_assoc_shared[[form]]), simplify = TRUE)
    ran_m <- rowMeans(ran)
    ran_s <- apply(ran, 1, sd)
    tibble(
      feat_degree_vocab = median(degree(g_feat_vocab)),
      feat_degree_randomM = ran_m["feat_degree"],
      feat_degree_randomS = ran_s["feat_degree"],
      feat_clust_vocab = transitivity(g_feat_vocab, type = "global"),
      feat_clust_randomM = ran_m["feat_clust"],
      feat_clust_randomS = ran_s["feat_clust"],
      feat_dist_vocab = igraph::mean_distance(g_feat_vocab),
      feat_dist_randomM = ran_m["feat_dist"],
      feat_dist_randomS = ran_s["feat_dist"],
      assoc_degree_vocab = median(degree(g_assoc_vocab, mode = "in")),
      assoc_degree_randomM = ran_m["assoc_degree"],
      assoc_degree_randomS = ran_s["assoc_degree"],
      assoc_clust_vocab = transitivity(g_assoc_vocab, type = "global"),
      assoc_clust_randomM = ran_m["assoc_clust"],
      assoc_clust_randomS = ran_s["assoc_clust"],
      assoc_dist_vocab = igraph::mean_distance(g_assoc_vocab),
      assoc_dist_randomM = ran_m["assoc_dist"],
      assoc_dist_randomS = ran_s["assoc_dist"]
    )
  }, .progress = TRUE))


saveRDS(vocab_data_first_record_shared, "vocab-netstats-TAR4-2026_02_13c.rds")




# Plotting ----
d <- readRDS("vocab-netstats-TAR4-2026_02_13c.rds") |>
  filter(nproduced >= 3) |>
  select(-vocab) |>
  unnest(netstats) |>
  pivot_longer(cols = feat_degree_vocab:assoc_dist_randomS, names_sep = "_", names_to = c("network", "netstat", "type"), values_to = "value") %>%
  group_by(subjectkey,network, netstat) %>%
  mutate(
    vocab_RANz = (value[type == "vocab"] - value[type == "randomM"]) / value[type == "randomS"])


write_rds(d, "data/formatted_metadata_crc.rds")

d |>
  mutate(
    type = factor(type, levels = c("vocab", "randomM", "randomS"), labels = c("child vocab.", "RAN mean", "RAN std. dev.")),
    network = factor(network, levels = c("assoc", "feat"), labels = c("directed associations", "undirected feature overlap")),
    netstat = factor(netstat, levels = c("degree", "clust", "dist"), labels = c("median (in)degree", "global clust. coef.", "mean shortest path"))
  ) |>
  ggplot(aes(x = nproduced, y = value, color = type)) +
    geom_point(size = .75) +
    xlab("number of nouns produced") +
    ylab("network statistic") +
    facet_grid(netstat ~ network, scale = "free_y") +
    theme_classic(base_size = 12)

ggsave("netstats-featTAR4-byForm.png", width = 6.5, height = 5, units = "in", dpi = 300)

d |>
  pivot_wider(names_from = type, values_from = value) |>
  mutate(
    vocab_RANz = (vocab - randomM) / randomS,
    network = factor(network, levels = c("assoc", "feat"), labels = c("directed\nassociations", "undirected\nfeature\noverlap")),
    netstat = factor(netstat, levels = c("degree", "clust", "dist"), labels = c("median (in)degree", "global clust. coef.", "mean shortest path"))
  ) |>
  ggplot(aes(x = nproduced, y = vocab_RANz, color = network)) +
    geom_point(size = .75, alpha = .3) +
    geom_smooth() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    xlab("number of nouns produced") +
    ylab("RAN-standardized statistic") +
    facet_wrap(vars(netstat)) +
    theme_classic(base_size = 12) +
    theme(legend.key.spacing.y = unit(1, "line"))

ggsave("netstats-RANz-featTAR4-byForm.png", width = 6.5, height = 3.25, units = "in", dpi = 300)

d |>
  filter(nproduced>20) |>
  pivot_wider(names_from = type, values_from = value) |>
  mutate(
    vocab_RANz = (vocab - randomM) / randomS,
    network = factor(network, levels = c("assoc", "feat"), labels = c("directed\nassociations", "undirected\nfeature\noverlap")),
    netstat = factor(netstat, levels = c("degree", "clust", "dist"), labels = c("median (in)degree", "global clust. coef.", "mean shortest path"))
  ) |>
  ggplot(aes(x = nproduced, y = vocab_RANz, color = network)) +
    geom_point(size = .75, alpha = .3) +
    geom_smooth() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    xlab("number of nouns produced (minimum 20)") +
    ylab("RAN-standardized statistic") +
    facet_wrap(vars(netstat)) +
    theme_classic(base_size = 12) +
    theme(legend.key.spacing.y = unit(1, "line"))

ggsave("netstats-RANz-featTAR4-nouns20-byForm.png", width = 6.5, height = 3.25, units = "in", dpi = 300)
