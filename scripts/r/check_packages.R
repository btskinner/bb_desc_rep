################################################################################
##
## [ PROJ ] Variation in broadband access among undergraduate populations
##          across the United States
## [ FILE ] check_packages.R
## [ AUTH ] Benjamin Skinner (@btskinner), Taylor Burtch, & Hazel Levy
## [ INIT ] 22 October 2022
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
scr_dir <- file.path(root, "scripts")

## packages required for this project
req_packages <- c("tidyverse",
                  "haven",
                  "cmdstanr",
                  "crosswalkr",
                  "patchwork",
                  "rstan",
                  "xtable")

## packages that are not installed
mask <- (req_packages %in% installed.packages()[,"Package"])
miss_packages <- req_packages[!mask]

## install any missing
if (length(miss_packages)) {
    message("Installing missing packages")
    install.packages(miss_packages)
} else {
    message("All required packages found")
}

## confirm that cmdstan is installed; if not, install
if (is.null(cmdstanr::cmdstan_version())) {
  install_cmdstan(cores = 2)
}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
