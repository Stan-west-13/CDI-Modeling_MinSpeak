library(tidyverse)
library(readxl)

## Load in Mininmally Speaking and long-formatted CDI data
min_speak_samp <- read_rds("data/CDI_multiple_timpoints.rds")
load("data/ASD_all_GM.Rdata")
cdi <- read_rds("data/cdi-metadata 1 (1).rds")
## Merge in Item-wise data
min_speak_itemwise <- min_speak_samp %>%
  left_join(ASD_all, by = c("subjectkey","interview_age","nProduced")) %>% ## Join over itemwise data
  left_join(select(cdi, word, CDI_ID = num_item_id), by = c("CDI_Metadata_compatible" = "word")) ## Join in CDI codes across forms

