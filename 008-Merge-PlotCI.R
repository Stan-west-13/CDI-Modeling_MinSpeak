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


ggplot(d, aes(x = bin, y = bs, color = contrast))+
  geom_point() +
  facet_grid(measure ~ network,scales = "free")+
  geom_errorbar(aes(ymin = lower,ymax=upper),width = 0)+
  theme_bw()
