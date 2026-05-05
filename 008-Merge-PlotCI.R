library(tidyverse)
library(purrr)
library(ggplot2)

list_merge_CIs <- function(dir,pattern){
  f <- list.files(path = dir, pattern = pattern, full.names = T)
  df <- map_dfr(f, function(x){
    read_rds(x)
  })
  return(df)
}

hues_select <- function(n){
  for (i in n) {
    return(hue_pal()(i))
  }
}

auto_scales <- function(x, contr){
  v <- vector(length = length(contr))
  for(i in 1:length(contr)){
    v[i] <- unique(x$fill[x$contrast == contr[i]])
  }
  return(v)
}

d <- list_merge_CIs("ci","CI")

fill_tbl <- data.frame(contrast = unique(d$contrast),
                       fills = hues_select(6))

d_plot <- d %>%
  mutate(sig = ifelse((pmin(upper, lower) <= 0) & 
                               (pmax(upper, lower) >= 0) & 
                               contrast %in% c("diff_ND_D","diff_TD_D","diff_TD_ND")
                             ,FALSE,TRUE)) %>%
  left_join(fill_tbl) %>%
  mutate(contrast = as.factor(contrast),
         contrasts_plt = factor(contrast, 
                                levels = c("D","ND","TD","diff_ND_D","diff_TD_ND","diff_TD_D"),
                                labels = c("older-autistic","younger-autistic","non-autistic","younger-autistic - older-autistic","non-autistic - no-delay",
                                           "non-autistic - older-autistic")))
## Diff two ASD plot
## Dodge it so no overlap.
d_plot_asd <- d_plot %>% 
  filter(contrast == "D"|contrast == "ND"|contrast == "diff_ND_D")
ggplot(d_plot_asd, aes(x = bin, y = bs,group = contrast, color = contrasts_plt))+
  geom_line()+
  scale_color_manual(values = auto_scales(d_plot_asd,c("D","ND","diff_ND_D")))+
  facet_grid(measure ~ network,
             scales = "free",
             labeller = labeller(measure = as_labeller(c("clust" = "Clustering Coefficient", 
                                                         "degree" = "Median Indegree", 
                                                         "dist" = "ASPL")),
                                 network = as_labeller(c("assoc" = "Associations", "feat" = "Features"))))+
  geom_errorbar(aes(ymin = lower,ymax=upper,group = contrast),
                width = 0,
                color = "black",
                alpha = 0.5,
                position = position_dodge(0.5))+
  geom_point(shape = 21,
             size = 3, 
             fill = ifelse(d_plot_asd$sig,d_plot_asd$fills, "white"),
             position = position_dodge(0.5)) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2, alpha  = 0.2)+
  scale_x_discrete(limits = factor(seq(0,250,25)))+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7,0.3),
    legend.background = element_rect(colour =  "black")
  )+
  labs(x = "nproduced",
       y = "RAN Standardized Value")


d_plot_asd_td_D_TD <- d_plot %>% filter(contrast == "D"|
                                     contrast == "TD"|
                                     contrast == "diff_TD_D"
                                    )
## Diff two ASD TD
ggplot(d_plot_asd_td_D_TD , aes(x = bin, y = bs,group = contrast, color = contrasts_plt))+
  geom_line()+
  scale_color_manual(values = auto_scales(d_plot_asd_td_D_TD, c("D",
                                                           "TD",
                                                           "diff_TD_D")))+
  facet_grid(measure ~ network,
             scales = "free",
             labeller = labeller(measure = as_labeller(c("clust" = "Clustering Coefficient", 
                                                         "degree" = "Median Indegree", 
                                                         "dist" = "ASPL")),
                                 network = as_labeller(c("assoc" = "Associations", "feat" = "Features"))))+
  geom_errorbar(aes(ymin = lower,ymax=upper,group = contrast),
                width = 0,
                color = "black",
                alpha = 0.5,
                position = position_dodge(0.5))+
  geom_point(shape = 21,
             size = 3, 
             fill = ifelse(d_plot_asd_td_D_TD$sig,
                           d_plot_asd_td_D_TD$fills, 
                           "white"),
            position = position_dodge(0.5) ) +
  theme_bw() +
  scale_x_discrete(limits = factor(seq(0,250,25)))+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7,0.3),
    legend.background = element_rect(colour =  "black")
  )+
  labs(x = "nproduced",
       y = "RAN Standardized Value")

d_plot_asd_td_ND_TD <- d_plot %>% filter(contrast == "ND"|
                                          contrast == "TD"|
                                          contrast == "diff_TD_ND"
)

ggplot(d_plot_asd_td_ND_TD , aes(x = bin, y = bs,group =contrast, color = contrasts_plt))+
  geom_line()+
  scale_color_manual(values = auto_scales(d_plot_asd_td_ND_TD, c("ND",
                                                           "TD",
                                                           "diff_TD_ND")))+
  facet_grid(measure ~ network,
             scales = "free",
             labeller = labeller(measure = as_labeller(c("clust" = "Clustering Coefficient", 
                                                         "degree" = "Median Indegree", 
                                                         "dist" = "ASPL")),
                                 network = as_labeller(c("assoc" = "Associations", "feat" = "Features"))))+
  geom_errorbar(aes(ymin = lower,ymax=upper),
                width = 0,
                color = "black",
                alpha = 0.5,
                position = position_dodge(0.5))+
  geom_point(shape = 21,
             size = 3, 
             fill = ifelse(d_plot_asd_td_ND_TD$sig,d_plot_asd_td_ND_TD$fills, "white"),
             position = position_dodge(0.5)) +
  theme_bw() +
  scale_x_discrete(limits = factor(seq(0,250,25)))+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7,0.3),
    legend.background = element_rect(colour =  "black")
  )+
  labs(x = "nproduced",
       y = "RAN Standardized Value")

