## Testing boot returns w/ ci just ASD kids
library(boot)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
d <- readRDS("data/asd_TD_minspeak_osg.rds") %>%
  filter(!group_two == "TD")


nsteps <- length(seq(0,250 , by =10 ))
N <- nsteps * 3


orth_poly <- poly(d$nproduced,2)
newdata <- predict(orth_poly, seq(0,250 , by =10 ))
newdata <- tibble(
  group_two = gl(2, nrow(newdata), labels = levels(d$group_two)),
  network = gl(2, nrow(newdata), labels = unique(d$network)),
  linear = rep(newdata[,1], 2),
  quadratic = rep(newdata[,2], 2)
)


bfun <- function(df,ix,.data){
  df <- df[ix, ]
  wt <- ifelse(df$group == 'delay', 2,1)
  m <- lm(z ~ (linear + quadratic) * group_two *network ,weights = wt, data = df[ix,])
  n <- nrow(.data)
  k <- n / 2
  x <- predict(m, .data)
  x <- c("ND" = x[1:k], "D" = x[(k+1):n] ,"diff" = x[1:k] - x[(k+1):n])
}

d <- d[d$netstat == "degree",]

m <- lm(z ~ (linear + quadratic) * group_two, data = d)
summary(m)
d$pred <- predict(m, newdata = newdata)

mm <- model.matrix(m)
bs <- boot(d[!is.na(d$z),],bfun,R = 1000,stype = "i", .data=newdata)

ci <- map_dfr(1:N, function(i, b, conf) {
  ci <- boot.ci(bs, conf = conf, type = "bca", index = i)$bca[4:5]
  d <- data.frame(upper = ci[1], lower = ci[2])
  return(d)
}, b = bs, conf = 1-(0.05 / 26),.progress = TRUE) %>%
  mutate(bs = bs$t0,
         x = rep(seq(1,26,1),3),
         grp = rep(c("ND","D","diff"),each = 26))


ggplot(ci,aes(x = x, y = bs, color = grp) )+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper))

bs_ci <- lapply(c(1:29), function(i,b,conf) {
  boot.ci(x$no_delay, conf = conf, type = "bca", index = i)
},b = bs, conf = 1 - (0.05/29))


bs_ci[[1]]$bca[4:5]

d_plot <- map_dfr(bs_ci, function(x){
  ci <- x$bca[4:5]
  d_ci <- data.frame(lower = ci[1], upper = ci[2])
  d_ci$est <- (d_ci$lower + d_ci$upper)/2
  return(d_ci)
},.id = "source") %>%
  mutate(source = factor(source, levels = c(1:29)))

ggplot(d_plot, aes(x = source, y = est))+
  geom_point()
