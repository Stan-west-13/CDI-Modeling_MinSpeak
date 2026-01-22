library(tidyverse)
library(readxl)

## Load in Minimally Speaking and long-formatted CDI data
min_speak_samp <- read_rds("data/CDI_multiple_timpoints.rds")
load("data/ASD_all_GM.Rdata")
cdi <- read_rds("data/cdi-metadata.rds")

## Load Letti data all WG forms
d_letti <- read.csv("data/LSEL_CDI_new_mod_for_script.csv")



## Making sure WG and WS have the same CDI-compatible lemmas
sum(unique(ASD_all$CDI_Metadata_compatible[ASD_all$form == "WG"]) %in% unique(ASD_all$CDI_Metadata_compatible[ASD_all$form == "WS"]) )
## 395 words from WG overlap with WS, so using the CDI_Metadata_compatible column to join in CDI word IDs is feasible. 

## Merge in Item-wise data and CDI word IDs that span form.
min_speak_itemwise <- min_speak_samp %>%
  left_join(ASD_all, by = c("subjectkey","interview_age","nProduced","form")) %>% ## Join over itemwise data
  left_join(select(cdi, word, CDI_ID = num_item_id), by = c("CDI_Metadata_compatible" = "word")) %>% ## Join in CDI codes across forms
  filter(nProduced >= 3 ) %>%
  group_by(subjectkey,interview_age) %>%
  mutate(subjectkey_intAge = paste0(subjectkey,"_",interview_age),.after = "subjectkey")

## Select just the first administration of CDI.
first_admins <- min_speak_itemwise %>%
  group_by(subjectkey) %>%
  slice_min(interview_age,n = 1) %>%
  filter(nProduced >= 3)


write_rds(min_speak_itemwise, "data/all_minspeak.rds")
write_rds(first_admins, "data/first_admins_minspeak.rds")
