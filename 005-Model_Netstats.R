library(ggplot2)
library(dplyr)

## Load data
d <- read_rds("data/all_Netstats_z_meta.rds")


## Grab relevant columns

model_df <- d %>%
  select(subjectkey_intAge, metric, form,group,interview_age,nProduced, z) %>%
  unique()
split_mods <- split(model_df, model_df$metric)

map(split_mods, function(x){
  return(summary(lm(z~nProduced*interview_age, data = x)))
})

ggplot(model_df, aes(x = nProduced, y = z,color = group))+
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~metric)


