## After bootstraps are run on OSG, the resulting 10 x 10,000 simulations are 
## routed here to combine before pushing back to OSG for confidence intervals.
library(tidyr)
library(purrr)
library(ggplot2)
library(stringr)
library(dplyr)
library(boot)
library(future)
library(furrr)
#' Combine multiple boot objects into one
#'
#' @param files Character vector of file paths to .RDS files containing boot objects
#' @return A single combined boot object
#' @examples
#' # combined_boot <- combine_boot_objects(c("boot1.rds", "boot2.rds"))
combine_boot_objects <- function(files) {
   # Read all boot objects
  boot_list <- lapply(files, readRDS)
  
  # Validate that all are boot objects
  if (!all(sapply(boot_list, inherits, what = "boot"))) {
    stop("All files must contain objects of class 'boot'.")
  }
  
  # Check compatibility
  ref <- boot_list[[1]]
  for (i in seq_along(boot_list)) {
    b <- boot_list[[i]]
    if (!identical(ref$t0, b$t0)) stop("Statistic t0 differs between boot objects.")
    if (ncol(ref$t) != ncol(b$t)) stop("Statistic dimensions differ.")
    if (length(ref$data) != length(b$data)) stop("Original data length differs.")
  }
  
  # Combine resampled statistics
  combined_t <- do.call(rbind, lapply(boot_list, function(b) b$t))
  
  # Create new boot object
  combined_boot <- ref
  combined_boot$t <- combined_t
  combined_boot$R <- nrow(combined_t)
  
  return(combined_boot)
}


## Comibine boots
degree_boot_feat <- combine_boot_objects(list.files("bs",pattern = "feat_degree", full.names = T))
clust_boot_feat <- combine_boot_objects(list.files("bs",pattern = "feat_clust", full.names = T))
dist_boot_feat <- combine_boot_objects(list.files("bs",pattern = "feat_dist", full.names = T))
saveRDS(degree_boot_feat, "feat_degree_boot_combined.rds")
saveRDS(clust_boot_feat, "feat_clust_boot_combined.rds")
saveRDS(dist_boot_feat, "feat_dist_boot_combined.rds")

degree_boot_assoc <- combine_boot_objects(list.files("bs",pattern = "assoc_degree", full.names = T))
clust_boot_assoc <- combine_boot_objects(list.files("bs",pattern = "assoc_clust", full.names = T))
dist_boot_assoc <- combine_boot_objects(list.files("bs",pattern = "assoc_dist", full.names = T))
saveRDS(degree_boot_assoc, "assoc_degree_boot_combined.rds")
saveRDS(clust_boot_assoc, "assoc_clust_boot_combined.rds")
saveRDS(dist_boot_assoc, "assoc_dist_boot_combined.rds")


 
