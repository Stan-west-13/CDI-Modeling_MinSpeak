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

d <- list_merge_CIs("ci","CI")

colors_tbl <- data.frame(contrast = unique(d$contrast),
                         )

d_plot <- d %>%
  mutate(sig = ifelse((pmin(upper, lower) <= 0) & (pmax(upper, lower) >= 0) & contrast %in% c("diff_ND_D","diff_TD_D","diff_TD_ND"),FALSE,TRUE))

## Diff two ASD plot
d_plot_asd <- d_plot %>% 
  filter(contrast == "D"|contrast == "ND"|contrast == "diff_ND_D")
ggplot(d_plot_asd, aes(x = bin, y = bs, color = contrast))+
  geom_line()+
  facet_grid(measure ~ network,scales = "free")+
  geom_errorbar(aes(ymin = lower,ymax=upper),width = 0,, color = "black")+
  geom_point(shape = 21,size = 3) +
  theme_bw() +
  scale_x_discrete(limits = factor(seq(0,250,25)))


d_plot_asd_td <- d_plot %>% filter(contrast == "D"|
                                     contrast == "ND"|
                                     contrast == "TD"|
                                     contrast == "diff_TD_D"|
                                     contrast == "diff_TD_ND")
## Diff two ASD TD
ggplot(d_plot_asd_td , aes(x = bin, y = bs, color = contrast))+
  geom_line()+
  facet_grid(measure ~ network,scales = "free")+
  geom_errorbar(aes(ymin = lower,ymax=upper),width = 0,color = "black")+
  geom_point(shape = 21, size = 3, fill = d_plot_asd_td$fill_color) +
  theme_bw() +
  scale_x_discrete(limits = factor(seq(0,250,25)))


