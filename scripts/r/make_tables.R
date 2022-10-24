################################################################################
##
## [ PROJ ] Variation in broadband access among undergraduate populations
##          across the United States
## [ FILE ] make_tables.R
## [ AUTH ] Benjamin Skinner (@btskinner), Taylor Burtch, & Hazel Levy
## [ INIT ] 22 October 2022
##
################################################################################

## libraries
libs <- c("tidyverse", "xtable", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
scr_dir <- file.path(root, "scripts", "r")
tab_dir <- file.path(root, "tables")

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## analysis data (for crosswalks)
df <- readRDS(file.path(cln_dir, "ipums_analysis.RDS")) |>
  left_join(stcrosswalk, by = c("statefip" = "stfips"))

## race/ethnicity crosswalk
recw <- readRDS(file.path(cln_dir, "re_crosswalk.RDS"))

## -----------------------------------------------------------------------------
## make overall table
## -----------------------------------------------------------------------------

## race/ethnicity-specific posterior
re_post <- readRDS(file.path(cln_dir, "re_post.RDS")) |>
  filter(cat == "Overall", q %in% c(0.025, 0.5, 0.975)) |>
  pivot_wider(names_from = "q",
              values_from = "x") |>
  left_join(df |> distinct(renum, re), by = "renum") |>
  select(-c(renum, cat)) |>
  pivot_wider(names_from = "y",
              values_from = c(`0.025`, `0.5`, `0.975`)) |>
  mutate(code2 = as.character(re)) |>
  select(-re) |>
  mutate(`0.5_bb` = sprintf("%0.2f", round(`0.5_bb`,2)),
         `0.5_do` = sprintf("%0.2f", round(`0.5_do`,2)),
         `0.025_bb` = sprintf("%0.3f", round(`0.025_bb`,3)),
         `0.025_do` = sprintf("%0.3f", round(`0.025_do`,3)),
         `0.975_bb` = sprintf("%0.3f", round(`0.975_bb`,3)),
         `0.975_do` = sprintf("%0.3f", round(`0.975_do`,3)))

con <- recw |>
  mutate(label = str_replace(label, "^[[:digit:]]+ - (.+)$", "\\1"),
         code = as.character(code),
         code2 = as.character(code2)) |>
  left_join(re_post, by = "code2") |>
  mutate(bb = paste0(`0.5_bb`," [",`0.025_bb`,",",`0.975_bb`,"]"),
         do = paste0(`0.5_do`," [",`0.025_do`,",",`0.975_do`,"]")) |>
  select(code, code2, label, bb, do)

## table notes
notes <- c(paste("Census codes (column 1), adjusted codes for figure with",
                 "all racial/ethnic groups (column 2) and labels (column 3)",
                 "from the Integrated Public Use Microdata System. We",
                 "show racial/ethnic labels as they are reported by the census.",
                 "{\\itshape W.I.}: write in;",
                 "{\\itshape N.E.C.}: not otherwise coded;",
                 "{\\itshape N.S.}: not specified.",
                 "Median posterior estimates with 95\\% credible intervals in",
                 "brackets."))

## header
head <- c("\\begingroup\\footnotesize\\begin{longtable}{lllcc}",
          paste("\\caption{Overall estimates of broadband access by",
                "race/ethnicity}"),
          "\\label{tbl:overall}\\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&\\multicolumn{2}{c}{Overall}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){4-5}",
          "Census&Figure&Label&Broadband in home&Mobile only\\\\",
          "\\midrule",
          "\\endfirsthead",
          "\\multicolumn{5}{l}{\\emph{...table \\thetable{} continued}} \\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&\\multicolumn{2}{c}{Overall}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){4-5}",
          "Census&Figure&Label&Broadband in home&Mobile only\\\\",
          "\\midrule",
          "\\endhead",
          "\\bottomrule",
          "\\multicolumn{5}{r}{\\emph{Continued on next page...}} \\\\",
          "\\endfoot",
          "\\endlastfoot"
          )

## primary contents
contents <- print(xtable(con),
                  hline.after = c(-1),
                  sanitize.text.function = function(x) {x},
                  booktabs = TRUE,
                  tabular.environment = "longtable",
                  floating = FALSE,
                  caption.placement = "top",
                  include.rownames = FALSE,
                  include.colnames = FALSE,
                  timestamp = NULL,
                  print.results = FALSE,
                  only.contents = TRUE,
                  comment = FALSE)

## footer
foot <- c("\\bottomrule",
          "\\multicolumn{5}{p{.98\\linewidth}}{\\footnotesize ",
          "\\vspace{.1em}",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{longtable}\\endgroup")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "overall.tex"))

## -----------------------------------------------------------------------------
## make men-specific table
## -----------------------------------------------------------------------------

## race/ethnicity-specific posterior
re_post <- readRDS(file.path(cln_dir, "re_post.RDS")) |>
  filter(cat == "Men", q %in% c(0.025, 0.5, 0.975)) |>
  pivot_wider(names_from = "q",
              values_from = "x") |>
  left_join(df |> distinct(renum, re), by = "renum") |>
  select(-c(renum, cat)) |>
  pivot_wider(names_from = "y",
              values_from = c(`0.025`, `0.5`, `0.975`)) |>
  mutate(code2 = as.character(re)) |>
  select(-re) |>
  mutate(`0.5_bb` = sprintf("%0.2f", round(`0.5_bb`,2)),
         `0.5_do` = sprintf("%0.2f", round(`0.5_do`,2)),
         `0.025_bb` = sprintf("%0.3f", round(`0.025_bb`,3)),
         `0.025_do` = sprintf("%0.3f", round(`0.025_do`,3)),
         `0.975_bb` = sprintf("%0.3f", round(`0.975_bb`,3)),
         `0.975_do` = sprintf("%0.3f", round(`0.975_do`,3)))

con <- recw |>
  mutate(label = str_replace(label, "^[[:digit:]]+ - (.+)$", "\\1"),
         code = as.character(code),
         code2 = as.character(code2)) |>
  left_join(re_post, by = "code2") |>
  mutate(bb = paste0(`0.5_bb`," [",`0.025_bb`,",",`0.975_bb`,"]"),
         do = paste0(`0.5_do`," [",`0.025_do`,",",`0.975_do`,"]")) |>
  select(code, code2, label, bb, do)

## table notes
notes <- c(paste("Census codes (column 1), adjusted codes for figure with",
                 "all racial/ethnic groups (column 2) and labels (column 3)",
                 "from the Integrated Public Use Microdata System. We",
                 "show racial/ethnic labels as they are reported by the census.",
                 "{\\itshape W.I.}: write in;",
                 "{\\itshape N.E.C.}: not otherwise coded;",
                 "{\\itshape N.S.}: not specified.",
                 "Median posterior estimates with 95\\% credible intervals in",
                 "brackets."))

## header
head <- c("\\begingroup\\footnotesize\\begin{longtable}{lllcc}",
          paste("\\caption{Estimates of broadband access by",
                "race/ethnicity: men}"),
          "\\label{tbl:men}\\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&\\multicolumn{2}{c}{Men}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){4-5}",
          "Census&Figure&Label&Broadband in home&Mobile only\\\\",
          "\\midrule",
          "\\endfirsthead",
          "\\multicolumn{5}{l}{\\emph{...table \\thetable{} continued}} \\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&\\multicolumn{2}{c}{Men}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){4-5}",
          "Census&Figure&Label&Broadband in home&Mobile only\\\\",
          "\\midrule",
          "\\endhead",
          "\\bottomrule",
          "\\multicolumn{5}{r}{\\emph{Continued on next page...}} \\\\",
          "\\endfoot",
          "\\endlastfoot"
          )

## primary contents
contents <- print(xtable(con),
                  hline.after = c(-1),
                  sanitize.text.function = function(x) {x},
                  booktabs = TRUE,
                  tabular.environment = "longtable",
                  floating = FALSE,
                  caption.placement = "top",
                  include.rownames = FALSE,
                  include.colnames = FALSE,
                  timestamp = NULL,
                  print.results = FALSE,
                  only.contents = TRUE,
                  comment = FALSE)

## footer
foot <- c("\\bottomrule",
          "\\multicolumn{5}{p{.98\\linewidth}}{\\footnotesize ",
          "\\vspace{.1em}",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{longtable}\\endgroup")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "men.tex"))

## -----------------------------------------------------------------------------
## make women-specific table
## -----------------------------------------------------------------------------

## race/ethnicity-specific posterior
re_post <- readRDS(file.path(cln_dir, "re_post.RDS")) |>
  filter(cat == "Women", q %in% c(0.025, 0.5, 0.975)) |>
  pivot_wider(names_from = "q",
              values_from = "x") |>
  left_join(df |> distinct(renum, re), by = "renum") |>
  select(-c(renum, cat)) |>
  pivot_wider(names_from = "y",
              values_from = c(`0.025`, `0.5`, `0.975`)) |>
  mutate(code2 = as.character(re)) |>
  select(-re) |>
  mutate(`0.5_bb` = sprintf("%0.2f", round(`0.5_bb`,2)),
         `0.5_do` = sprintf("%0.2f", round(`0.5_do`,2)),
         `0.025_bb` = sprintf("%0.3f", round(`0.025_bb`,3)),
         `0.025_do` = sprintf("%0.3f", round(`0.025_do`,3)),
         `0.975_bb` = sprintf("%0.3f", round(`0.975_bb`,3)),
         `0.975_do` = sprintf("%0.3f", round(`0.975_do`,3)))

con <- recw |>
  mutate(label = str_replace(label, "^[[:digit:]]+ - (.+)$", "\\1"),
         code = as.character(code),
         code2 = as.character(code2)) |>
  left_join(re_post, by = "code2") |>
  mutate(bb = paste0(`0.5_bb`," [",`0.025_bb`,",",`0.975_bb`,"]"),
         do = paste0(`0.5_do`," [",`0.025_do`,",",`0.975_do`,"]")) |>
  select(code, code2, label, bb, do)

## table notes
notes <- c(paste("Census codes (column 1), adjusted codes for figure with",
                 "all racial/ethnic groups (column 2) and labels (column 3)",
                 "from the Integrated Public Use Microdata System. We",
                 "show racial/ethnic labels as they are reported by the census.",
                 "{\\itshape W.I.}: write in;",
                 "{\\itshape N.E.C.}: not otherwise coded;",
                 "{\\itshape N.S.}: not specified.",
                 "Median posterior estimates with 95\\% credible intervals in",
                 "brackets."))

## header
head <- c("\\begingroup\\footnotesize\\begin{longtable}{lllcc}",
          paste("\\caption{Estimates of broadband access by",
                "race/ethnicity: women}"),
          "\\label{tbl:women}\\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&\\multicolumn{2}{c}{Women}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){4-5}",
          "Census&Figure&Label&Broadband in home&Mobile only\\\\",
          "\\midrule",
          "\\endfirsthead",
          "\\multicolumn{5}{l}{\\emph{...table \\thetable{} continued}} \\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&\\multicolumn{2}{c}{Women}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){4-5}",
          "Census&Figure&Label&Broadband in home&Mobile only\\\\",
          "\\midrule",
          "\\endhead",
          "\\bottomrule",
          "\\multicolumn{5}{r}{\\emph{Continued on next page...}} \\\\",
          "\\endfoot",
          "\\endlastfoot"
          )

## primary contents
contents <- print(xtable(con),
                  hline.after = c(-1),
                  sanitize.text.function = function(x) {x},
                  booktabs = TRUE,
                  tabular.environwoment = "longtable",
                  floating = FALSE,
                  caption.placewoment = "top",
                  include.rownames = FALSE,
                  include.colnames = FALSE,
                  timestamp = NULL,
                  print.results = FALSE,
                  only.contents = TRUE,
                  comwoment = FALSE)

## footer
foot <- c("\\bottomrule",
          "\\multicolumn{5}{p{.98\\linewidth}}{\\footnotesize ",
          "\\vspace{.1em}",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{longtable}\\endgroup")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "women.tex"))

## -----------------------------------------------------------------------------
## make Hispanic by state: broadband in home
## -----------------------------------------------------------------------------

## hispanic state specific
h_st_post <- readRDS(file.path(cln_dir, "hispanic_st_post.RDS")) |>
  left_join(stcrosswalk |> select(stfips, stname),
            by = c("statefip" = "stfips")) |>
  filter(y == "bb", cat != "Overall", q %in% c(0.025, 0.5, 0.975)) |>
  select(-statefip) |>
  pivot_wider(names_from = "q",
              values_from = "x") |>
  left_join(df |> distinct(renum, re), by = "renum") |>
  select(-c(renum, y)) |>
  pivot_wider(names_from = "cat",
              values_from = c(`0.025`, `0.5`, `0.975`)) |>
  mutate(code2 = as.character(re)) |>
  select(-re) |>
  mutate(`0.5_Men` = sprintf("%0.2f", round(`0.5_Men`,2)),
         `0.5_Women` = sprintf("%0.2f", round(`0.5_Women`,2)),
         `0.025_Men` = sprintf("%0.3f", round(`0.025_Men`,3)),
         `0.025_Women` = sprintf("%0.3f", round(`0.025_Women`,3)),
         `0.975_Men` = sprintf("%0.3f", round(`0.975_Men`,3)),
         `0.975_Women` = sprintf("%0.3f", round(`0.975_Women`,3)))

con <- recw |>
  mutate(label = str_replace(label, "^[[:digit:]]+ - (.+)$", "\\1"),
         code = as.character(code),
         code2 = as.character(code2)) |>
  left_join(h_st_post, by = "code2") |>
  filter(!is.na(stname)) |>
  mutate(Men = paste0(`0.5_Men`," [",`0.025_Men`,",",`0.975_Men`,"]"),
         Women = paste0(`0.5_Women`," [",`0.025_Women`,",",`0.975_Women`,"]")) |>
  select(code, code2, stname, label, Men, Women) |>
  arrange(stname, code)

## table notes
notes <- c(paste("Census codes (column 1), adjusted codes for figure with",
                 "Hispanic groups (column 2), state name (column 3),",
                 "and labels (column 3)",
                 "from the Integrated Public Use Microdata System. We",
                 "show racial/ethnic labels as they are reported by the census.",
                 "{\\itshape N.E.C.}: not otherwise coded;",
                 "{\\itshape N.S.}: not specified.",
                 "Median posterior estimates with 95\\% credible intervals in",
                 "brackets."))

## header
head <- c("\\begingroup\\footnotesize\\begin{longtable}{llllcc}",
          paste("\\caption{Estimates of in-home broadband access for Hispanic",
                "populations in California, Florida, and Texas}"),
          "\\label{tbl:hispbb}\\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&&\\multicolumn{2}{c}{Broadband in the home}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){5-6}",
          "Census&Figure&State&Label&Men&Women\\\\",
          "\\midrule",
          "\\endfirsthead",
          "\\multicolumn{6}{l}{\\emph{...table \\thetable{} continued}} \\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&&\\multicolumn{2}{c}{Broadband in the home}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){5-6}",
          "Census&Figure&State&Label&Men&Women\\\\",
          "\\midrule",
          "\\endhead",
          "\\bottomrule",
          "\\multicolumn{6}{r}{\\emph{Continued on next page...}} \\\\",
          "\\endfoot",
          "\\endlastfoot"
          )

## primary contents
contents <- print(xtable(con),
                  hline.after = c(-1),
                  sanitize.text.function = function(x) {x},
                  booktabs = TRUE,
                  tabular.environwoment = "longtable",
                  floating = FALSE,
                  caption.placewoment = "top",
                  include.rownames = FALSE,
                  include.colnames = FALSE,
                  timestamp = NULL,
                  print.results = FALSE,
                  only.contents = TRUE,
                  comwoment = FALSE)

## footer
foot <- c("\\bottomrule",
          "\\multicolumn{6}{p{.98\\linewidth}}{\\footnotesize ",
          "\\vspace{.1em}",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{longtable}\\endgroup")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "hispanic_bb.tex"))


## -----------------------------------------------------------------------------
## make Hispanic by state: mobile only
## -----------------------------------------------------------------------------

## hispanic state specific
h_st_post <- readRDS(file.path(cln_dir, "hispanic_st_post.RDS")) |>
  left_join(stcrosswalk |> select(stfips, stname),
            by = c("statefip" = "stfips")) |>
  filter(y == "do", cat != "Overall", q %in% c(0.025, 0.5, 0.975)) |>
  select(-statefip) |>
  pivot_wider(names_from = "q",
              values_from = "x") |>
  left_join(df |> distinct(renum, re), by = "renum") |>
  select(-c(renum, y)) |>
  pivot_wider(names_from = "cat",
              values_from = c(`0.025`, `0.5`, `0.975`)) |>
  mutate(code2 = as.character(re)) |>
  select(-re) |>
  mutate(`0.5_Men` = sprintf("%0.2f", round(`0.5_Men`,2)),
         `0.5_Women` = sprintf("%0.2f", round(`0.5_Women`,2)),
         `0.025_Men` = sprintf("%0.3f", round(`0.025_Men`,3)),
         `0.025_Women` = sprintf("%0.3f", round(`0.025_Women`,3)),
         `0.975_Men` = sprintf("%0.3f", round(`0.975_Men`,3)),
         `0.975_Women` = sprintf("%0.3f", round(`0.975_Women`,3)))

con <- recw |>
  mutate(label = str_replace(label, "^[[:digit:]]+ - (.+)$", "\\1"),
         code = as.character(code),
         code2 = as.character(code2)) |>
  left_join(h_st_post, by = "code2") |>
  filter(!is.na(stname)) |>
  mutate(Men = paste0(`0.5_Men`," [",`0.025_Men`,",",`0.975_Men`,"]"),
         Women = paste0(`0.5_Women`," [",`0.025_Women`,",",`0.975_Women`,"]")) |>
  select(code, code2, stname, label, Men, Women) |>
  arrange(stname, code)

## table notes
notes <- c(paste("Census codes (column 1), adjusted codes for figure with",
                 "Hispanic groups (column 2), state name (column 3),",
                 "and labels (column 3)",
                 "from the Integrated Public Use Microdata System. We",
                 "show racial/ethnic labels as they are reported by the census.",
                 "{\\itshape N.E.C.}: not otherwise coded;",
                 "{\\itshape N.S.}: not specified.",
                 "Median posterior estimates with 95\\% credible intervals in",
                 "brackets."))

## header
head <- c("\\begingroup\\footnotesize\\begin{longtable}{llllcc}",
          paste("\\caption{Estimates of mobile only broadband access for Hispanic",
                "populations in California, Florida, and Texas}"),
          "\\label{tbl:hispdo}\\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&&\\multicolumn{2}{c}{Mobile only}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){5-6}",
          "Census&Figure&State&Label&Men&Women\\\\",
          "\\midrule",
          "\\endfirsthead",
          "\\multicolumn{6}{l}{\\emph{...table \\thetable{} continued}} \\\\",
          "\\toprule",
          "\\multicolumn{2}{c}{Code}&&&\\multicolumn{2}{c}{Mobile only}\\\\",
          "\\cmidrule(lr){1-2}\\cmidrule(lr){5-6}",
          "Census&Figure&State&Label&Men&Women\\\\",
          "\\midrule",
          "\\endhead",
          "\\bottomrule",
          "\\multicolumn{6}{r}{\\emph{Continued on next page...}} \\\\",
          "\\endfoot",
          "\\endlastfoot"
          )

## primary contents
contents <- print(xtable(con),
                  hline.after = c(-1),
                  sanitize.text.function = function(x) {x},
                  booktabs = TRUE,
                  tabular.environwoment = "longtable",
                  floating = FALSE,
                  caption.placewoment = "top",
                  include.rownames = FALSE,
                  include.colnames = FALSE,
                  timestamp = NULL,
                  print.results = FALSE,
                  only.contents = TRUE,
                  comwoment = FALSE)

## footer
foot <- c("\\bottomrule",
          "\\multicolumn{6}{p{.98\\linewidth}}{\\footnotesize ",
          "\\vspace{.1em}",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{longtable}\\endgroup")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "hispanic_do.tex"))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
