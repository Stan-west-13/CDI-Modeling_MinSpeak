# Some useful keyboard shortcuts for package authoring:
#
#   Load Package:              'Ctrl + Shift + L'
#   Test Package:              'Ctrl + Shift + T'

#' Derive sorted set from a vector of input
#' 
#' @param x A vector
#' @return Unique values of x in ascending order
sortedSet <- function(x) {
  return(sort(unique(x)))
}

#' Identify which elements of x are cues
#' 
#' @param x A vector
#' @param cues A vector defining the full set of unique cues
#' @return A logical vector with \code{length(x)} elements.
iscue <- function(x, cues) {
  return(x %in% cues)
}

#' Filter cue-response pairs where the response is not a cue word
#' 
#' @param d A data frame with two columns: "cue" and "resp".
#' @return A subset of rows from the original data frame.
filterNonCueResponses <- function(d) {
  return(subset(d, iscue(d$resp, cues = sortedSet(d$cue))))
}

#' Generate a logical response profile
#' 
#' @param resp A vector containing responses associated with a cue
#' @param all_uniq A vector containing the full set of unique values that define the profile.
#' @return A logical vector with \code{length(all_uniq)} elements.
respProfile <- function(resp, all_unique) {
  return(all_unique %in% resp)
}

#' Generate an unweighted associative network
#' 
#' @param d A data frame with two columns: "cue" and "resp".
#' @return A logical matrix with dimensions \code{length(unique(d$cue))} by \code{length(unique(d$cue))}.
assocNetwork <- function(d) {
  d <- filterNonCueResponses(d)
  cue_set <- sortedSet(d$cue)
  x <- tapply(d$resp,
              INDEX = d$cue,
              FUN = respProfile,
              all_unique = cue_set)
  net <- do.call(rbind, x)
  colnames(net) <- cue_set
  return(net)
}

assocNetwork_noLoops <- function(d){
  net <- assocNetwork(d) 
  diag(net) <- FALSE
  return(net)
}

