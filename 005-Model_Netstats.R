library(ggplot2)
library(dplyr)
library(tidyverse)
## Load data
d <- read_rds("data/all_Netstats_z_meta.rds")


## Grab relevant columns

model_df <- d %>%
  select(subjectkey_intAge, metric, form,group,interview_age,nProduced,source, z) %>%
  unique()
split_mods <- split(model_df, model_df$metric)

## Model without group
map(split_mods, function(x){
  return(summary(lm(z~nProduced*interview_age, data = x)))
})

## Model with group
map(split_mods, function(x){
  return(summary(lm(z~nProduced*interview_age*group, data = x)))
})

## Plot by nProduced
ggplot(model_df, aes(x = nProduced, y = z,color = group,shape = source))+
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~metric)

## Plot by interview age
ggplot(model_df, aes(x = interview_age, y = z,color = group))+
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~metric)

## Plot nProduced with polynomial regress 
ggplot(model_df %>% filter(!group == "mdl"), aes(x = nProduced, y = z,color = group))+
  #geom_point() +
  geom_smooth(method = "lm", formula = y~poly(x,3))+
  facet_wrap(~metric)

