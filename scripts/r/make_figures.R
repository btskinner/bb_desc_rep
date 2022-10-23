################################################################################
##
## [ PROJ ] Variation in broadband access among undergraduate populations
##          across the United States
## [ FILE ] make_data.R
## [ AUTH ] Benjamin Skinner (@btskinner), Taylor Burtch, & Hazel Levy
## [ INIT ] 22 October 2022
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "patchwork")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts", "r")

## outcomes
outc <- c("bb","do")

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## analysis data (for crosswalks)
df <- readRDS(file.path(cln_dir, "ipums_analysis.RDS")) |>
  left_join(stcrosswalk, by = c("statefip" = "stfips"))

## race/ethnicity crosswalk
recw <- readRDS(file.path(cln_dir, "re_crosswalk.RDS"))

## poststratified predictions
nl_post <- readRDS(file.path(cln_dir, "national_post.RDS"))
st_post <- readRDS(file.path(cln_dir, "state_post.RDS"))
rg_post <- readRDS(file.path(cln_dir, "rg_post.RDS"))
re_post <- readRDS(file.path(cln_dir, "re_post.RDS"))
asian_post <- readRDS(file.path(cln_dir, "asian_post.RDS"))
other_post <- readRDS(file.path(cln_dir, "other_post.RDS"))
h_st_post <- readRDS(file.path(cln_dir, "hispanic_st_post.RDS"))

## -----------------------------------------------------------------------------
## state differences
## -----------------------------------------------------------------------------

## make each figure
p <- map(outc,
         function(z) {

           ## make plot data
           plot_df <- bind_rows(nl_post |> mutate(stnum = 0),
                                st_post) |>
             pivot_wider(names_from = "q",
                         values_from = "x") |>
             filter(y == z)

           if (z == "bb") {
             plot_df <- plot_df |> arrange(desc(`0.5`))
           } else {
             plot_df <- plot_df |> arrange(`0.5`)
           }

           plot_df <- plot_df |>
             mutate(x = row_number()) |>
             ungroup() |>
             left_join(df |> distinct(stnum, stabbr), by = "stnum") |>
             mutate(stabbr = ifelse(is.na(stabbr), "US", stabbr),
                    nat_est = ifelse(stabbr == "US", 1, 0),
                    x = factor(x, levels = x, labels = stabbr))

           ## make overall lines
           q025 <- plot_df |> filter(y == z, nat_est == 1) |> pull(`0.025`)
           q050 <- plot_df |> filter(y == z, nat_est == 1) |> pull(`0.5`)
           q975 <- plot_df |> filter(y == z, nat_est == 1) |> pull(`0.975`)

           ## plot settings
           if (z == "bb") {
             lab_x = NULL
             lab_y = "Broadband in the home (%)"
             breaks_seq = seq(0.5,1,0.05)
             mbreaks_seq = seq(0.5,1,0.01)
           } else {
             lab_x = "State"
             lab_y = "Cellular data plan only (%)"
             breaks_seq = seq(0,0.3,0.05)
             mbreaks_seq = seq(0,0.3,0.01)
           }

           ## make plot
           ggplot(plot_df |> filter(nat_est == 0),
                  aes(x = x, y = `0.5`)) +
             geom_hline(yintercept = q050, linetype = "dashed") +
             geom_linerange(aes(ymin = `0.025`, ymax = `0.975`,
                                xmin = x, xmax = x)) +
             geom_point(colour = "white", size = 3) +
             geom_point() +
             annotate("rect", xmin = -Inf, xmax = Inf,
                      ymin = q025, ymax = q975,
                      fill = "gray", alpha = 0.3) +
             scale_y_continuous(breaks = breaks_seq,
                                minor_breaks = mbreaks_seq,
                                labels = scales::percent_format(accuracy = 1)) +
             labs(y = lab_y,
                  x = lab_x) +
             theme_bw()

         })

## patchwork together
p <- p[[1]] / p[[2]]

## save plot
ggsave(file.path(fig_dir, "stcomp.pdf"),
       p,
       width = 12,
       height = 8,
       units = "in",
       dpi = "retina")

## save plot
ggsave(file.path(fig_dir, "stcomp_slides.pdf"),
       p,
       width = 16,
       height = 8,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## race/ethnicity overall
## -----------------------------------------------------------------------------

## make each figure
p <- map(outc,
         function(z) {

           ## make plot data
           plot_df <- re_post |>
             pivot_wider(names_from = "q",
                         values_from = "x") |>
             filter(cat == "Overall", y == z) |>
             left_join(df |> distinct(renum, re), by = "renum")

           if (z == "bb") {
             plot_df <- plot_df |> arrange(desc(`0.5`))
           } else {
             plot_df <- plot_df |> arrange(`0.5`)
           }

           plot_df <- plot_df |>
             mutate(x = row_number(),
                    x = factor(x, levels = x, re))

           ## overall values
           nq025 <- nl_post |> filter(y == z, q == 0.025) |> pull(x)
           nq050 <- nl_post |> filter(y == z, q == 0.500) |> pull(x)
           nq975 <- nl_post |> filter(y == z, q == 0.975) |> pull(x)

           ## plot settings
           if (z == "bb") {
             lab_x = "Race/ethnicity"
             lab_y = "Broadband in the home (%)"
             breaks_seq = seq(0.5,1,0.05)
             mbreaks_seq = seq(0.5,1,0.01)
           } else {
             lab_x = NULL
             lab_y = "Cellular data plan only (%)"
             breaks_seq = seq(0,0.4,0.05)
             mbreaks_seq = seq(0,0.4,0.01)
           }

           ## make plot
           p1 <- ggplot(plot_df,
                        aes(x = x, y = `0.5`)) +
             geom_hline(yintercept = nq050, linetype = "dotted",
                        color = "gray", alpha = 0.7) +
             annotate("rect", xmin = -Inf, xmax = Inf,
                      ymin = nq025, ymax = nq975,
                      fill = "gray", alpha = 0.3) +
             geom_linerange(aes(ymin = `0.025`, ymax = `0.975`,
                                xmin = x, xmax = x), size = 0.25) +
             geom_point(colour = "white", size = 1) +
             geom_point(size = 0.5) +
             scale_y_continuous(breaks = breaks_seq,
                                minor_breaks = mbreaks_seq,
                                labels = scales::percent_format(accuracy = 1)) +
             labs(y = lab_y,
                  x = lab_x) +
             coord_flip() +
             theme_bw(base_size = 6)

           p2 <- ggplot(plot_df,
                        aes(x = x, y = `0.5`)) +
             geom_hline(yintercept = nq050, linetype = "dotted",
                        color = "gray", alpha = 0.7) +
             annotate("rect", xmin = -Inf, xmax = Inf,
                      ymin = nq025, ymax = nq975,
                      fill = "gray", alpha = 0.3) +
             geom_linerange(aes(ymin = `0.025`, ymax = `0.975`,
                                xmin = x, xmax = x), size = 0.25) +
             geom_point(colour = "white", size = 1) +
             geom_point(size = 0.5) +
             scale_y_continuous(breaks = breaks_seq,
                                minor_breaks = mbreaks_seq,
                                labels = scales::percent_format(accuracy = 1)) +
             labs(y = lab_y,
                  x = "Race/ethnicity") +
             theme_bw(base_size = 6) +
             theme(axis.text.x = element_text(angle = 45,
                                              hjust = 0.25,
                                              vjust = 0.25))

           list(p1, p2)

         })

## patchwork together
p1 <- p[[1]][[1]] + p[[2]][[1]]

## save plot
ggsave(file.path(fig_dir, "recomp.pdf"),
       p1,
       width = 8,
       height = 10,
       units = "in",
       dpi = "retina")

## patchwork together
p2 <- p[[1]][[2]] / p[[2]][[2]]

## save plot
ggsave(file.path(fig_dir, "recomp_slides.pdf"),
       p2,
       width = 16,
       height = 8,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## race/ethnicity by groups
## -----------------------------------------------------------------------------

groups <- c("aiak", "asian", "other", "hispanic")

for (g in groups) {

  if (g == "aiak") reg_range <- 3
  if (g == "asian") reg_range <- 4:6
  if (g == "other") reg_range <- 7:9
  if (g == "hispanic") reg_range <- 10

  ## make plot
  p <- map(outc,
           function(z) {
             ## make plot data
             plot_df <- re_post |>
               pivot_wider(names_from = "q",
                           values_from = "x") |>
               filter(cat == "Overall", y == z) |>
               left_join(df |> distinct(re_group, renum, re),
                         by = "renum") |>
               filter(re_group %in% reg_range) |>
               select(-re_group, -cat)
             if (z == "bb") {
               plot_df <- plot_df |> arrange(desc(`0.5`))
             } else {
               plot_df <- plot_df |> arrange(`0.5`)
             }
             if (g == "hispanic") {
               plot_df <- plot_df |>
                 mutate(x = row_number(),
                        x = factor(x, x, re - 1000))
             } else {
               plot_df <- plot_df |>
                 mutate(x = row_number(),
                        x = factor(x, x, re))
             }
             if (z == "do") {
               plot_df <- plot_df |>
                 left_join(recw, by = c("re" = "code2"))
             }

             ## overall values
             nq025 <- nl_post |> filter(y == z, q == 0.025) |> pull(x)
             nq050 <- nl_post |> filter(y == z, q == 0.500) |> pull(x)
             nq975 <- nl_post |> filter(y == z, q == 0.975) |> pull(x)
             ## subgroup overall values
             if (g == "asian") {
               q025 <- asian_post |> filter(y == z, q == 0.025) |> pull(x)
               q050 <- asian_post |> filter(y == z, q == 0.500) |> pull(x)
               q975 <- asian_post |> filter(y == z, q == 0.975) |> pull(x)
             } else if (g == "other") {
               q025 <- other_post |> filter(y == z, q == 0.025) |> pull(x)
               q050 <- other_post |> filter(y == z, q == 0.500) |> pull(x)
               q975 <- other_post |> filter(y == z, q == 0.975) |> pull(x)
             } else {
               q025 <- rg_post |> filter(y == z,
                                         rg == reg_range,
                                         q == 0.025) |> pull(x)
               q050 <- rg_post |> filter(y == z,
                                         rg == reg_range,
                                         q == 0.500) |> pull(x)
               q975 <- rg_post |> filter(y == z,
                                         rg == reg_range,
                                         q == 0.975) |> pull(x)
             }

             ## plot settings
             if (z == "bb") {
               lab_x = NULL
               lab_y = "Broadband in the home (%)"
               breaks_seq = seq(0.5,1,0.05)
               mbreaks_seq = seq(0.5,1,0.01)
             } else {
               lab_x = "Race/ethnicity"
               lab_y = "Cellular data plan only (%)"
               breaks_seq = seq(0,0.4,0.05)
               mbreaks_seq = seq(0,0.4,0.01)
             }

             ## make plot
             if (z == "bb") {
               pp <- ggplot(plot_df, aes(x = x, y = `0.5`))
             } else {
               pp <- ggplot(plot_df, aes(x = x, y = `0.5`, colour = label))
             }

             pp <- pp +
               geom_hline(yintercept = nq050, linetype = "dotted",
                          color = "gray", alpha = 0.7) +
               geom_hline(yintercept = q050, linetype = "dashed",
                          color = "gray", alpha = 0.7) +
               annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = q025, ymax = q975,
                        fill = "gray", alpha = 0.3) +
               annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = nq025, ymax = nq975,
                        fill = "gray", alpha = 0.3) +
               geom_linerange(aes(ymin = `0.025`, ymax = `0.975`,
                                  xmin = x, xmax = x)) +
               geom_point(colour = "white", size = 3) +
               geom_point() +
               scale_y_continuous(breaks = breaks_seq,
                                  minor_breaks = mbreaks_seq,
                                  labels = scales::percent_format(accuracy = 1)) +
               labs(y = lab_y,
                    x = lab_x) +
               theme_bw()

             if (z == "do") {
               pp <- pp +
                 scale_colour_manual(name = NULL,
                                     values = rep("black",
                                                  plot_df |>
                                                    distinct(label) |>
                                                    nrow()),
                                     guide = guide_legend(override.aes =
                                                            list(alpha = 0))) +
                 theme(legend.position = "bottom")
             }
             if (g == "other") {
               pp <- pp +
                 theme(axis.text.x = element_text(angle = 45,
                                                  hjust = 0.25,
                                                  vjust = 0.25))
             }
             pp
           })

  ## patchwork together
  p <- p[[1]] / p[[2]]

  ## save plot
  ggsave(file.path(fig_dir, paste0(g, "_disagg.pdf")),
         p,
         width = 12,
         height = 8,
         units = "in",
         dpi = "retina")

  ## save plot
  ggsave(file.path(fig_dir, paste0(g, "_disagg_slides.pdf")),
         p,
         width = 16,
         height = 8,
         units = "in",
         dpi = "retina")

}

## -----------------------------------------------------------------------------
## race/ethnicity X gender by groups
## -----------------------------------------------------------------------------

groups <- c("aiak", "asian", "other", "hispanic")

for (g in groups) {

  if (g == "aiak") reg_range <- 3
  if (g == "asian") reg_range <- 4:6
  if (g == "other") reg_range <- 7:9
  if (g == "hispanic") reg_range <- 10

  ## make plot
  p <- map(outc,
           function(z) {

             plot_df <- re_post |>
               pivot_wider(names_from = "q",
                           values_from = "x") |>
               filter(cat != "Overall", y == z) |>
               left_join(df |> distinct(re_group, renum, re),
                         by = "renum") |>
               filter(re_group %in% reg_range) |>
               filter(`0.5` != 0) |>
               select(-re_group)

             ## diff <- plot_df |>
             ##   select(re, cat, `0.5`) |>
             ##   group_by(re) |>
             ##   summarise(diff = `0.5`[1] - `0.5`[2]) |>
             ##   select(re, diff)

             diff <- plot_df |>
               select(re, cat, `0.5`) |>
               group_by(re) |>
               summarise(diff = mean(`0.5`)) |>
               select(re, diff)

             plot_df <- plot_df |>
               left_join(diff, by = "re")

             if (z == "bb") {
               plot_df <- plot_df |> arrange(desc(diff))
             } else {
               plot_df <- plot_df |> arrange(diff)
             }
             if (g == "hispanic") {
               plot_df <- plot_df |>
                 mutate(x = factor(diff, diff, re - 1000))
             } else {
               plot_df <- plot_df |>
                 mutate(x = factor(diff, diff, re))
             }
             if (z == "do") {
               plot_df <- plot_df |>
                 left_join(recw, by = c("re" = "code2"))
             }

             ## overall values
             nq025 <- nl_post |> filter(y == z, q == 0.025) |> pull(x)
             nq050 <- nl_post |> filter(y == z, q == 0.500) |> pull(x)
             nq975 <- nl_post |> filter(y == z, q == 0.975) |> pull(x)
             ## subgroup overall values
             if (g == "asian") {
               q025 <- asian_post |> filter(y == z, q == 0.025) |> pull(x)
               q050 <- asian_post |> filter(y == z, q == 0.500) |> pull(x)
               q975 <- asian_post |> filter(y == z, q == 0.975) |> pull(x)
             } else if (g == "other") {
               q025 <- other_post |> filter(y == z, q == 0.025) |> pull(x)
               q050 <- other_post |> filter(y == z, q == 0.500) |> pull(x)
               q975 <- other_post |> filter(y == z, q == 0.975) |> pull(x)
             } else {
               q025 <- rg_post |> filter(y == z,
                                         rg == reg_range,
                                         q == 0.025) |> pull(x)
               q050 <- rg_post |> filter(y == z,
                                         rg == reg_range,
                                         q == 0.500) |> pull(x)
               q975 <- rg_post |> filter(y == z,
                                         rg == reg_range,
                                         q == 0.975) |> pull(x)
             }

             ## plot settings
             if (z == "bb") {
               lab_x = NULL
               lab_y = "Broadband in the home (%)"
               breaks_seq = seq(0.5,1,0.05)
               mbreaks_seq = seq(0.5,1,0.01)
             } else {
               lab_x = "Race/ethnicity"
               lab_y = "Cellular data plan only (%)"
               breaks_seq = seq(0,0.4,0.05)
               mbreaks_seq = seq(0,0.4,0.01)
             }

             ## make plot
             if (z == "bb") {
               pp <- ggplot(plot_df, aes(x = x, y = `0.5`, colour = cat,
                                         group = cat))
             } else {
               pp <- ggplot(plot_df, aes(x = x, y = `0.5`, colour = cat,
                                         group = cat, fill = label))
             }

             pp <- pp +
               geom_hline(yintercept = nq050, linetype = "dotted") +
               geom_hline(yintercept = q050, linetype = "longdash") +
               annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = q025, ymax = q975,
                        fill = "gray", alpha = 0.3) +
               annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = nq025, ymax = nq975,
                        fill = "gray", alpha = 0.3) +
               geom_linerange(aes(ymin = `0.025`, ymax = `0.975`,
                                  xmin = x, xmax = x),
                              position = position_dodge(width=0.9)) +
               geom_point(colour = "white", size = 3,
                          position = position_dodge(width=0.9)) +
               geom_point(position = position_dodge(width=0.9)) +
               geom_vline(xintercept = seq(0.5, plot_df |>
                                                  distinct(re) |>
                                                  pull() |>
                                                  length(),
                                           by = 1),
                          colour = "gray",
                          size = .1,
                          alpha = .5) +
               scale_y_continuous(breaks = breaks_seq,
                                  minor_breaks = mbreaks_seq,
                                  labels = scales::percent_format(accuracy = 1)) +
               labs(y = lab_y,
                    x = lab_x) +
               theme_bw() +
               theme(panel.grid.major.x = element_blank(),
                     legend.position = "bottom",
                     legend.box = "vertical")

             if (z == "bb") {
               pp <- pp + guides(colour = "none")
             } else {
               pp <- pp +
                 scale_colour_discrete(name = "",
                                       guide = guide_legend(order = 1)) +
                 scale_fill_manual(name = NULL,
                                   values = rep("black",
                                                plot_df |>
                                                  distinct(label) |>
                                                  nrow()),
                                   guide = guide_legend(override.aes =
                                                          list(alpha = 0),
                                                        order = 2))
             }
             if (g == "other") {
               pp <- pp +
                 theme(axis.text.x = element_text(angle = 45,
                                                  hjust = 0.25,
                                                  vjust = 0.25))
             }
             pp
           })

  ## patchwork together
  p <- p[[1]] / p[[2]]

  ## save plot
  ggsave(file.path(fig_dir, paste0(g, "_mf_disagg.pdf")),
         p,
         width = 12,
         height = 10,
         units = "in",
         dpi = "retina")

  ## save plot
  ggsave(file.path(fig_dir, paste0(g, "_mf_disagg_slides.pdf")),
         p,
         width = 16,
         height = 9,
         units = "in",
         dpi = "retina")

}

## -----------------------------------------------------------------------------
## hispanic populations in three states
## -----------------------------------------------------------------------------

for (i in outc) {

  ## make plot
  plot_df <- h_st_post |>
    pivot_wider(names_from = "q",
                values_from = "x") |>
    filter(cat != "Overall", y == i) |>
    left_join(stcrosswalk |> select(stfips, stname),
              by = c("statefip" = "stfips"))

  diff <- plot_df |>
    select(renum, statefip, cat, `0.5`) |>
    group_by(renum, statefip) |>
    summarise(diff = mean(`0.5`),
              .groups = "drop") |>
    select(renum, statefip, diff)

  plot_df <- plot_df |>
    left_join(diff, by = c("renum", "statefip"))

  if (i == "bb") {
    plot_df <- plot_df |> arrange(statefip, desc(diff))
  } else {
    plot_df <- plot_df |> arrange(statefip, diff)
  }

  plot_df <- plot_df |>
    left_join(df |> distinct(renum, re),
              by = "renum") |>
    left_join(recw, by = c("re" = "code2")) |>
    mutate(x = factor(diff, diff, re - 1000))

  ## overall values
  nq025 <- nl_post |> filter(y == i, q == 0.025) |> pull(x)
  nq050 <- nl_post |> filter(y == i, q == 0.500) |> pull(x)
  nq975 <- nl_post |> filter(y == i, q == 0.975) |> pull(x)
  ## subgroup overall values
  q025 <- rg_post |> filter(y == i, rg == 10, q == 0.025) |> pull(x)
  q050 <- rg_post |> filter(y == i, rg == 10, q == 0.500) |> pull(x)
  q975 <- rg_post |> filter(y == i, rg == 10, q == 0.975) |> pull(x)

  ## plot settings
  if (i == "bb") {
    lab_x = "Race/ethnicity"
    lab_y = "Broadband in the home (%)"
    breaks_seq = seq(0.5,1,0.05)
    mbreaks_seq = seq(0.5,1,0.01)
  } else {
    lab_x = "Race/ethnicity"
    lab_y = "Cellular data plan only (%)"
    breaks_seq = seq(0,0.4,0.05)
    mbreaks_seq = seq(0,0.4,0.01)
  }

  p <- ggplot(plot_df |> filter(`0.5` != 0),
              aes(x = x, y = `0.5`, colour = cat,
                           group = cat, fill = label)) +
    facet_wrap(~ stname, ncol = 1) +
    geom_hline(yintercept = nq050, linetype = "dotted") +
    geom_hline(yintercept = q050, linetype = "longdash") +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = q025, ymax = q975,
             fill = "gray", alpha = 0.3) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = nq025, ymax = nq975,
             fill = "gray", alpha = 0.3) +
    geom_linerange(aes(ymin = `0.025`, ymax = `0.975`,
                       xmin = x, xmax = x),
                   position = position_dodge(width=0.9)) +
    geom_point(colour = "white", size = 3,
               position = position_dodge(width=0.9)) +
    geom_point(position = position_dodge(width=0.9)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = q025, ymax = q975,
             fill = "gray", alpha = 0.3) +
    geom_vline(xintercept = seq(0.5, plot_df |>
                                       distinct(renum) |>
                                       pull() |>
                                       length(),
                                by = 1),
               colour = "gray",
               size = .1,
               alpha = .5) +
    scale_y_continuous(breaks = breaks_seq,
                       minor_breaks = mbreaks_seq,
                       labels = scales::percent_format(accuracy = 1)) +
    scale_colour_discrete(name = "",
                          guide = guide_legend(order = 1)) +
    scale_fill_manual(name = NULL,
                      values = rep("black",
                                   plot_df |>
                                     distinct(label) |>
                                     nrow()),
                      guide = guide_legend(override.aes =
                                             list(alpha = 0),
                                           order = 2)) +
    labs(y = lab_y,
         x = lab_x) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical")

  ## save plot
  ggsave(file.path(fig_dir, paste0(i, "_h_st.pdf")),
         p,
         width = 12,
         height = 10,
         units = "in",
         dpi = "retina")

  ## save plot
  ggsave(file.path(fig_dir, paste0(i, "_h_st_slides.pdf")),
         p,
         width = 16,
         height = 8,
         units = "in",
         dpi = "retina")

}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
