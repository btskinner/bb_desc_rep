################################################################################
##
## [ PROJ ] Variation in broadband access among undergraduate populations
##          across the United States
## [ FILE ] run_stan.R
## [ AUTH ] Benjamin Skinner (@btskinner), Taylor Burtch, & Hazel Levy
## [ INIT ] 22 October 2022
##
################################################################################

## libraries
libs <- c("tidyverse", "cmdstanr")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path(root, "estimates")
sta_dir <- file.path(root, "scripts", "stan")

# stan sampler settings
stan_seed <- 12345
stan_adapt_delta <- .9
stan_max_depth <- 15L
stan_num_cores <- 4
stan_num_chains <- 4
stan_num_threads <- 2
stan_num_warmup <- 1000L
stan_num_samples <- 1000L

## helper functions
inv_logit <- function(x) { exp(-log(1 + exp(-x))) }

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

df <- readRDS(file.path(cln_dir, "ipums_analysis.RDS"))

## -----------------------------------------------------------------------------
## loop through models
## -----------------------------------------------------------------------------

## -----------------------
## compile model
## -----------------------

mod <- cmdstan_model(file.path(sta_dir, "mrp.stan"),
                     compile = TRUE,
                     cpp_options = list(stan_threads = TRUE))

for (i in c("broadband", "dataplano")) {

  ## -----------------------
  ## set up data for Stan
  ## -----------------------

  stan_dat <- list(
    R = df |> distinct(region) |> nrow(),
    S = df |> distinct(statefip) |> nrow(),
    N = df |> nrow(),
    J_re = df |> distinct(renum) |> nrow(),
    J_finc = df |> distinct(finc_cat) |> nrow(),
    J_age = df |> distinct(age_cat) |> nrow(),
    J_stre = df |> distinct(stre) |> nrow(),
    y = df |> pull(all_of(i)),
    tot = df |> pull(tot),
    f = df |> pull(female),
    re = df |> pull(renum),
    stre = df |> pull(stre),
    finc = df |> pull(finc_cat),
    age = df |> pull(age_cat),
    region = df |> pull(region),
    state = df |> pull(stnum)
  )

  ## -----------------------
  ## sample
  ## -----------------------

  ## fit
  fit <- mod$sample(stan_dat,
                    seed = stan_seed,
                    parallel_chains = stan_num_cores,
                    chains = stan_num_chains,
                    threads_per_chain = stan_num_threads,
                    iter_warmup = stan_num_warmup,
                    iter_sampling = stan_num_samples,
                    max_treedepth = stan_max_depth,
                    adapt_delta = stan_adapt_delta)

  ## save
  fit$save_output_files(dir = est_dir,
                        basename = paste0("mrp_", i),
                        timestamp = FALSE,
                        random = FALSE)


  ## make design matrix
  dmat <- df |>
    select(region, stnum, finc_cat, age_cat, female, renum, stre) |>
    as.matrix()

  ## 2. generate cell predictions
  files <- list.files(est_dir, paste0("mrp_", i), full.names = TRUE)
  pars <- rstan::read_stan_csv(files) |> rstan::extract()
  preds <- matrix(NA_real_, nrow(dmat), length(pars[["a"]]))

  ## 3. linear combination
  for (j in 1:nrow(preds)) {
    preds[j,] <- pars[["a"]]  +
      pars[["a_region"]][,dmat[j,"region"]] +
      pars[["a_state"]][,dmat[j,"stnum"]] +
      pars[["a_finc"]][,dmat[j,"finc_cat"]] +
      pars[["a_age"]][,dmat[j,"age_cat"]] +
      pars[["b_f"]] * dmat[j,"female"] +
      pars[["a_re"]][,dmat[j,"renum"]] +
      pars[["a_re_f"]][,dmat[j,"renum"]] * dmat[j,"female"] +
      pars[["a_st_re"]][,dmat[j,"stre"]] +
      pars[["a_st_re_f"]][,dmat[j,"stre"]] * dmat[j,"female"]
  }

  ## linear combination to probability scale
  preds <- inv_logit(preds)

  ## save predictions
  saveRDS(preds, file.path(cln_dir, paste0("mrp_", i, "_pred.RDS")))
}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
