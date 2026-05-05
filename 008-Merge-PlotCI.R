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
    v[i] <- unique(x$fill[x$contrasts_plt == contr[i]])
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
                                labels = c("delay","no-delay","non-autistic","no-delay - delay","non-autistic - no-delay",
                                           "non-autistic - delay")))
## Diff two ASD plot
d_plot_asd <- d_plot %>% 
  filter(contrast == "D"|contrast == "ND"|contrast == "diff_ND_D")
ggplot(d_plot_asd, aes(x = bin, y = bs, color = contrasts_plt))+
  geom_line()+
  scale_color_manual(values = auto_scales(d_plot_asd,c("delay","no-delay","no-delay - delay")))+
  facet_grid(measure ~ network,scales = "free")+
  geom_errorbar(aes(ymin = lower,ymax=upper),width = 0,, color = "black")+
  geom_point(shape = 21,size = 3, fill = ifelse(d_plot_asd$sig,d_plot_asd$fills, "white") ) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2, alpha  = 0.2)+
  scale_x_discrete(limits = factor(seq(0,250,25)))


d_plot_asd_td <- d_plot %>% filter(contrast == "D"|
                                     contrast == "ND"|
                                     contrast == "TD"|
                                     contrast == "diff_TD_D"|
                                     contrast == "diff_TD_ND")
## Diff two ASD TD
ggplot(d_plot_asd_td , aes(x = bin, y = bs, color = contrasts_plt))+
  geom_line()+
  scale_color_manual(values = auto_scales(d_plot_asd_td, c("delay",
                                                           "no-delay",
                                                           "non-autistic",
                                                           "non-autistic - no-delay",
                                                           "non-autistic - delay")))+
  facet_grid(measure ~ network,scales = "free")+
  geom_errorbar(aes(ymin = lower,ymax=upper),width = 0,color = "black")+
  geom_point(shape = 21,size = 3, fill = ifelse(d_plot_asd_td$sig,d_plot_asd_td$fills, "white") ) +
  theme_bw() +
  scale_x_discrete(limits = factor(seq(0,250,25)))


