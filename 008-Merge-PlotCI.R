library(tidyverse)
library(purrr)
library(ggplot2)
library(scales)
library(broom)
library(webshot2)
library(kableExtra)
list_merge_CIs <- function(dir,pattern){
  f <- list.files(path = dir, pattern = pattern, full.names = T)
  df <- map_dfr(f, function(x){
    read_rds(x) %>%
      rename(upper = lower,
             lower = upper)
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

# Function to check if 0 is between two numbers
is_zero_between <- function(a, b, inclusive = TRUE) {
  # Validate inputs
  if (!is.numeric(a) || !is.numeric(b)) {
    stop("'a' and 'b' must be numeric vectors.")
  }
  if (length(a) != length(b)) {
    stop("'a' and 'b' must have the same length.")
  }
  
  # Determine lower and upper bounds for each row
  lower <- pmin(a, b, na.rm = FALSE)
  upper <- pmax(a, b, na.rm = FALSE)
  
  # Check if 0 is between bounds
  if (inclusive) {
    return(lower <= 0 & 0 <= upper)
  } else {
    return(lower < 0 & 0 < upper)
  }
}
  
diff_vec <- c("diff_ND_D","diff_TD_D","diff_TD_ND")

d <- list_merge_CIs("ci","CI")

fill_tbl <- data.frame(contrast = unique(d$contrast),
                       fills = hues_select(6))

d_plot <- d %>%
  mutate(sig = ifelse(is_zero_between(upper,lower, inclusive = T) & 
                               contrast %in% c("diff_ND_D","diff_TD_D","diff_TD_ND")
                             ,FALSE,TRUE)) %>%
  left_join(fill_tbl) %>%
  mutate(contrast = as.factor(contrast),
         contrasts_plt = factor(contrast, 
                                levels = c("D","ND","TD","diff_ND_D","diff_TD_ND","diff_TD_D"),
                                labels = c("older-autistic","younger-autistic","non-autistic","younger-autistic - older-autistic","non-autistic - younger-autistic",
                                           "non-autistic - older-autistic"))) %>%
  mutate(measure = as.factor(measure),
         network = as.factor(network),
         bin = factor(bin,levels = seq.int(1,11,1), labels = seq.int(0,250,25)))
## Diff two ASD plot
## Dodge it so no overlap.
d_plot_asd <- d_plot %>% 
  filter(contrast == "D"|contrast == "ND"|contrast == "diff_ND_D")
ggplot(d_plot_asd, aes(x = bin, y = bs,group = contrast, color = contrasts_plt))+
  geom_line(position = position_dodge(0.5))+
  scale_color_manual(values = auto_scales(d_plot_asd,c("D","ND","diff_ND_D")))+
  facet_grid(measure ~ network,
             scales = "free",
             labeller = labeller(measure = as_labeller(c("clust" = "Clustering Coefficient", 
                                                         "degree" = "Median Indegree", 
                                                         "dist" = "ASPL")),
                                 network = as_labeller(c("assoc" = "Associations", "feat" = "Features"))))+
  geom_errorbar(aes(ymin = lower,ymax=upper,group = contrast),
                width = 0,
                #size = 1.5,
                color = d_plot_asd$fills,
                #alpha = 0.5,
                position = position_dodge(0.5))+
  geom_point(shape = 21,
             size = 3, 
             fill = ifelse(d_plot_asd$sig,d_plot_asd$fills, "white"),
             position = position_dodge(0.5)) +
  theme_bw(base_size = 16) +
  geom_hline(yintercept = 0, linetype = 2, alpha  = 0.5)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7,0.3),
    legend.background = element_rect(colour =  "black")
  )+
  labs(x = "nproduced",
       y = "RAN Standardized Value")
ggsave("Figures/older_younger_ASD_comp.png",
       width = 24, height = 20, units = "cm",dpi = 300)
ggsave("Figures/older_younger_ASD_comp.pdf",
       width = 24, height = 20, units = "cm",dpi = 300)

d_plot_asd_td_D_TD <- d_plot %>% filter(contrast == "D"|
                                     contrast == "TD"|
                                     contrast == "diff_TD_D"
                                    ) %>%
  arrange(measure,network,contrast,bin)
## Diff older autistic, non-autistic
ggplot(d_plot_asd_td_D_TD , aes(x = bin, y = bs,group = contrast, color = contrasts_plt))+
  geom_line(position = position_dodge(0.5))+
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
                color = d_plot_asd_td_D_TD$fills,
                #alpha = 0.5,
                position = position_dodge(0.5))+
  geom_point(shape = 21,
             size = 3, 
             fill = ifelse(d_plot_asd_td_D_TD$sig,
                           d_plot_asd_td_D_TD$fills, 
                           "white"),
            position = position_dodge(0.5) ) +
  theme_bw(base_size = 16) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7,0.3),
    legend.background = element_rect(colour =  "black")
  )+
  labs(x = "nproduced",
       y = "RAN Standardized Value")
ggsave("Figures/older_nonAut.png",
       width = 24, height = 20, units = "cm",dpi = 300)
ggsave("Figures/older_nonAut.pdf",
       width = 24, height = 20, units = "cm",dpi = 300)

d_plot_asd_td_ND_TD <- d_plot %>% filter(contrast == "ND"|
                                          contrast == "TD"|
                                          contrast == "diff_TD_ND"
)%>%
  arrange(measure,network,contrast,bin)

## Diff non-autistic, younger autistic
ggplot(d_plot_asd_td_ND_TD , aes(x = bin, y = bs,group =contrast, color = contrasts_plt))+
  geom_line(position = position_dodge(0.5))+
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
                color = d_plot_asd_td_ND_TD$fills,
                #alpha = 0.5,
                position = position_dodge(0.5))+
  geom_point(shape = 21,
             size = 3, 
             fill = ifelse(d_plot_asd_td_ND_TD$sig,d_plot_asd_td_ND_TD$fills, "white"),
             position = position_dodge(0.5)) +
  theme_bw(base_size = 16) +
  geom_hline(yintercept = 0,linetype=2, alpha = 0.5)+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7,0.3),
    legend.background = element_rect(colour =  "black")
  )+
  labs(x = "nproduced",
       y = "RAN Standardized Value")
ggsave("Figures/younger_nonAut_comp.png",
       width = 24, height = 20, units = "cm",dpi = 300)
ggsave("Figures/younger_nonAut_comp.pdf",
       width = 24, height = 20, units = "cm",dpi = 300)

## Run Models w/ POC
d <- read_rds("data/matched_data_split.rds")

contr.mat <- matrix(c(1,-1,0,0.5,0.5,-1),
                    ncol = 2)
colnames(contr.mat) <- c("younger_vs_older", "younger_older_vs_TD")
rownames(contr.mat) <- c("younger", "older", "TD")

Sys.setenv(CHROMOTE_CHROME =chromote::find_chrome()) 

map(d, function(x){
  x$network <- as.factor(x$network)
  x$group <- as.factor(x$group_two)
  contrasts(x$group) <- contr.mat
  m <- lm(z ~ (linear + quadratic) * group * network , data = x)
  rtn <- tidy(m) %>%  kbl(caption= paste0("Linear Model Output Table ",unique(x$netstat)),
                             format= "html",
                             align="r",) %>%
    kable_classic(full_width = T, html_font = "helvetica") %>%
    save_kable(keep_tex = T, file = paste0("tables/table_",unique(x$netstat),".png")) 
  return(list(m = summary(m), rtn ))
})

## Descriptives
td <- read_rds("data/td_vocab-admins_WG-WS-2026-03-21.rds") %>%
  select(subjectkey, interview_age, nproduced_total = nproduced,n_nouns) %>%
  unique()
ASD_all <- read_rds("data/combined_CDILetti.rds")
ASD <- ASD_all%>%
  group_by(subjectkey,interview_age) %>%
  mutate(n_nouns = sum(Produces & lexical_class == "nouns",na.rm = T)) %>%
  select(subjectkey, interview_age, nproduced_total = nProduced, n_nouns) %>%
  unique() %>%
  ungroup()

all <- rbind(ASD,td)

desc <- d$degree %>% 
  left_join(all) %>%
  select(subjectkey, group_two,interview_age,nproduced_total,n_nouns) %>%
  unique()

desc %>%
  group_by(group_two) %>%
  summarize(n= n(),
            m_age = mean(interview_age),
            sd_age = sd(interview_age),
            m_nproduced_total = mean(nproduced_total,na.rm = T),
            sd_nproduced_total = sd(nproduced_total,na.rm = T),
            m_n_nouns = mean(n_nouns),
            sd_n_nouns = sd(n_nouns)) %>%
  as.data.frame() %>%
  kbl(caption= "Descriptive Stats by Group ",
                     format= "html",
                     align="r",) %>%
  kable_classic(full_width = T, html_font = "helvetica") %>%
  save_kable(keep_tex = T, file = "tables/table_descriptives.pdf")
  



