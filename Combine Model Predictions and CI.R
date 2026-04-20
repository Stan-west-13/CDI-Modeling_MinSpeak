library(tidyr)
library(ggplot2)
data_degree <- readRDS("data/1_degree_bs_minspeak.rds")$data
data_dist <- readRDS("data/2_dist_bs_minspeak.rds")$data
data_clust <- readRDS("data/3_clust_bs_minspeak.rds")$data

degree_ci <- readRDS("data/degree_ci.rds")
dist_ci <- readRDS("data/dist_ci.rds")
clust_ci <- readRDS("data/clust_ci.rds")


pred_degree <- data.frame(prediction = predict(readRDS("data/0_degree_lm_minspeak.rds")))
pred_dist <- data.frame(prediction = predict(readRDS("data/1_dist_lm_minspeak.rds")))
pred_clust <- data.frame(prediction = predict(readRDS("data/2_clust_lm_minspeak.rds")))


## Join predictions
df_pred_degree <- cbind(data_degree,pred_degree)
df_pred_dist <- cbind(data_dist,pred_dist)
df_pred_clust <- cbind(data_clust,pred_clust)

df_all <- rbind(df_pred_clust,df_pred_degree,df_pred_dist)
saveRDS(df_all, "data/combined_predictions.RDS")

ggplot(df_all, aes(x = nProduced, y = prediction, color = group_two, linetype = network))+
  geom_line()+
  facet_wrap(~metric_plot)













