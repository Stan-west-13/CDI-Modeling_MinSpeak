assign_percentile_produces <- function(d, norms, TD_threshold = 15) {
  require('dplyr')

  d <- droplevels(subset(d, d$type == "word"))
  d$produces <- d$value == "produces"
  d$value <- NULL

  D <- d %>%
    dplyr::group_by(data_id,age) %>%
    dplyr::summarize(WordsProduced = sum(produces)) %>%
    dplyr::ungroup()

  D$percentile <- 0
  qlist <- c(99,seq(95,5,by=-5))
  for (i in 1:nrow(D)) {
    x <- as.vector(as.matrix(subset(norms, norms$age == D$age[i])[,6:20]))
    qi <- which(D$WordsProduced[i] >= x)[1]
    D$percentile[i] <- qlist[qi]
  }
  D$percentile[is.na(D$percentile)] <- 0

  D$group <- factor(ifelse(D$percentile > TD_threshold, 1, 2), levels = 1:2, labels = c("TD","LT"))
  return(D)
}

assign_percentile_understands <- function(d, norms, TD_threshold = 15) {
  require('dplyr')

  d <- droplevels(subset(d, d$type == "word"))
  d$produces <- d$value == "produces" | d$value == "understands"
  d$value <- NULL

  D <- d %>%
    dplyr::group_by(data_id,age,sex,mom_ed) %>%
    dplyr::summarize(WordsUnderstood = sum(produces)) %>%
    dplyr::ungroup()

  D$percentile <- 0
  qlist <- c(99,seq(95,5,by=-5))
  for (i in 1:nrow(D)) {
    x <- as.vector(as.matrix(subset(norms, norms$age == D$age[i])[,6:25]))
    qi <- which(D$WordsUnderstood[i] >= x)[1]
    D$percentile[i] <- qlist[qi]
  }
  D$percentile[is.na(D$percentile)] <- 0

  D$group <- factor(ifelse(D$percentile > TD_threshold, 1, 2), levels = 1:2, labels = c("TD","LT"))
  return(D)
}
