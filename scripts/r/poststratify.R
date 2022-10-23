################################################################################
##
## [ PROJ ] Variation in broadband access among undergraduate populations
##          across the United States
## [ FILE ] poststratify.R
## [ AUTH ] Benjamin Skinner (@btskinner), Taylor Burtch, & Hazel Levy
## [ INIT ] 22 October 2022
##
################################################################################

## libraries
libs <- c("tidyverse")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path(root, "estimates")

## helper functions
ps_weight <- function(x, n) {
  if (is.null(dim(x)) & length(x) > 0) {
    x
  } else {
    colSums(x * n / sum(n))
  }
}
make_quantile <- function(x, q = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  tibble(q = q, x = quantile(x, q))
}

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## analysi dataframe
df <- readRDS(file.path(cln_dir, "ipums_analysis.RDS"))

## predictions
pr <- list(bb = readRDS(file.path(cln_dir, "mrp_broadband_pred.RDS")),
           do = readRDS(file.path(cln_dir, "mrp_dataplano_pred.RDS")))

## population counts for weights
pw <- df |> pull(pop)

## -----------------------------------------------------------------------------
## poststratify
## -----------------------------------------------------------------------------

## ---------------------------
## national
## ---------------------------

ps_national <- map2(pr,
                   names(pr),
                   ~ tibble(y = .y,
                            pred = ps_weight(.x, pw)) |>
                     group_by(y) |>
                     summarise(make_quantile(pred),
                               .groups = "drop")) |>
  bind_rows()

## ---------------------------
## state
## ---------------------------

ps_state <- map2(pr,
                 names(pr),
                 function(pred, predname) {
                   map(seq(1, df |> distinct(stnum) |> nrow(), 1),
                       function(s) {
                         mask <- (df |> pull(stnum) == s)
                         tibble(y = predname,
                                stnum = s,
                                pred = ps_weight(pred[mask,], pw[mask])) |>
                           group_by(y, stnum) |>
                           summarise(make_quantile(pred),
                                     .groups = "drop")
                       })
                 }) |>
  bind_rows()

## ---------------------------
## race/ethnicity groups
## ---------------------------

ps_rg <- map2(pr,
              names(pr),
              function(pred, predname) {
                map(seq(1, df |> distinct(re_group) |> nrow(), 1),
                    function(g) {
                      mask <- (df |> pull(re_group) == g)
                      tibble(y = predname,
                             rg = g,
                             pred = ps_weight(pred[mask,], pw[mask])) |>
                        group_by(y, rg) |>
                        summarise(make_quantile(pred),
                                  .groups = "drop")
                    })
              }) |>
  bind_rows()

## ---------------------------
## race/ethnicity groups
## ---------------------------

## overall
ps_re <- map2(pr,
              names(pr),
              function(pred, predname) {
                map(seq(1, df |> distinct(renum) |> nrow(), 1),
                    function(g) {
                      mask <- (df |> pull(renum) == g)
                      tibble(y = predname,
                             renum = g,
                             pred = ps_weight(pred[mask,], pw[mask])) |>
                        group_by(y, renum) |>
                        summarise(make_quantile(pred),
                                  .groups = "drop")
                    })
              }) |>
  bind_rows()

## men
ps_re_m <- map2(pr,
              names(pr),
              function(pred, predname) {
                map(seq(1, df |> distinct(renum) |> nrow(), 1),
                    function(g) {
                      mask <- (df |> pull(renum) == g & df |> pull(female) == 0)
                      tibble(y = predname,
                             renum = g,
                             pred = ps_weight(pred[mask,], pw[mask])) |>
                        group_by(y, renum) |>
                        summarise(make_quantile(pred),
                                  .groups = "drop")
                    })
              }) |>
  bind_rows()

## women
ps_re_f <- map2(pr,
              names(pr),
              function(pred, predname) {
                map(seq(1, df |> distinct(renum) |> nrow(), 1),
                    function(g) {
                      mask <- (df |> pull(renum) == g & df |> pull(female) == 1)
                      tibble(y = predname,
                             renum = g,
                             pred = ps_weight(pred[mask,], pw[mask])) |>
                        group_by(y, renum) |>
                        summarise(make_quantile(pred),
                                  .groups = "drop")
                    })
              }) |>
  bind_rows()

## recombine
ps_re <- bind_rows(ps_re |> mutate(cat = "Overall"),
                   ps_re_m |> mutate(cat = "Men"),
                   ps_re_f |> mutate(cat = "Women"))

## ---------------------------
## aggregations
## ---------------------------

ps_asian <- map2(pr,
                 names(pr),
                 ~ {
                   mask <- (df |> pull(re_group) %in% 4:6)
                   tibble(y = .y,
                          pred = ps_weight(.x[mask,], pw[mask])) |>
                     group_by(y) |>
                     summarise(make_quantile(pred),
                               .groups = "drop")
                 }) |>
  bind_rows()

ps_other <- map2(pr,
                 names(pr),
                 ~ {
                   mask <- (df |> pull(re_group) %in% 7:9)
                   tibble(y = .y,
                          pred = ps_weight(.x[mask,], pw[mask])) |>
                     group_by(y) |>
                     summarise(make_quantile(pred),
                               .groups = "drop")
                 }) |>
  bind_rows()

## ---------------------------
## Hispanic w/ 3 states
## ---------------------------

## overall
ps_h_st <- map2(pr,
                names(pr),
                function(pred, predname) {
                  map(c(6,12,48),
                      function(i) {
                        map(df |> filter(re_group == 10) |>
                              distinct(renum) |> pull(),
                            function(j) {
                              mask <- (df |> pull(statefip) == i &
                                         df |> pull(renum) == j)
                              tibble(y = predname,
                                     statefip = i,
                                     renum = j,
                                     pred = ps_weight(pred[mask,], pw[mask])) |>
                                group_by(y, statefip, renum) |>
                                summarise(make_quantile(pred),
                                          .groups = "drop")
                            })
                      }) |>
                    bind_rows()
                }) |>
  bind_rows()

## men
ps_h_st_m <- map2(pr,
                  names(pr),
                  function(pred, predname) {
                    map(c(6,12,48),
                        function(i) {
                          map(df |> filter(re_group == 10) |>
                                distinct(renum) |> pull(),
                            function(j) {
                              mask <- (df |> pull(statefip) == i &
                                         df |> pull(renum) == j &
                                         df |> pull(female) == 0)
                              tibble(y = predname,
                                     statefip = i,
                                     renum = j,
                                     pred = ps_weight(pred[mask,], pw[mask])) |>
                                group_by(y, statefip, renum) |>
                                summarise(make_quantile(pred),
                                          .groups = "drop")
                            })
                        }) |>
                      bind_rows()
                  }) |>
  bind_rows()

## women
ps_h_st_f <- map2(pr,
                  names(pr),
                  function(pred, predname) {
                    map(c(6,12,48),
                        function(i) {
                          map(df |> filter(re_group == 10) |>
                                distinct(renum) |> pull(),
                            function(j) {
                              mask <- (df |> pull(statefip) == i &
                                         df |> pull(renum) == j &
                                         df |> pull(female) == 1)
                              tibble(y = predname,
                                     statefip = i,
                                     renum = j,
                                     pred = ps_weight(pred[mask,], pw[mask])) |>
                                group_by(y, statefip, renum) |>
                                summarise(make_quantile(pred),
                                          .groups = "drop")
                            })
                        }) |>
                      bind_rows()
                  }) |>
  bind_rows()

## recombine
ps_h_st <- bind_rows(ps_h_st |> mutate(cat = "Overall"),
                     ps_h_st_m |> mutate(cat = "Men"),
                     ps_h_st_f |> mutate(cat = "Women"))

## -----------------------------------------------------------------------------
## save
## -----------------------------------------------------------------------------

saveRDS(ps_national, file.path(cln_dir, "national_post.RDS"))
saveRDS(ps_state, file.path(cln_dir, "state_post.RDS"))
saveRDS(ps_rg, file.path(cln_dir, "rg_post.RDS"))
saveRDS(ps_re, file.path(cln_dir, "re_post.RDS"))
saveRDS(ps_asian, file.path(cln_dir, "asian_post.RDS"))
saveRDS(ps_other, file.path(cln_dir, "other_post.RDS"))
saveRDS(ps_h_st, file.path(cln_dir, "hispanic_st_post.RDS"))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
