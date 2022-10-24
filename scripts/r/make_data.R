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
libs <- c("tidyverse", "haven")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
ipm_dir <- file.path(dat_dir, "ipums")
cln_dir <- file.path(dat_dir, "clean")

## -----------------------------------------------------------------------------
## read in CPI data
## -----------------------------------------------------------------------------

cpi <- read_csv(file.path(ipm_dir, "USACPIALLAINMEI.csv"),
                col_types = cols()) |>
  rename_all(tolower) |>
  mutate(year = lubridate::year(date),
         adj19 = usacpiallainmei[year == 2019],
         inf_adj = usacpiallainmei / adj19) |>
  filter(year >= 2013) |>
  select(year, inf_adj)

## -----------------------------------------------------------------------------
## read in IPUMS data
## -----------------------------------------------------------------------------

## NB: IPUMS data are selected from the IPUMS website
##
## Sample
## ------
##
## 2013 ACS     1.0%
## 2014 ACS     1.0%
## 2015 ACS     1.0%
## 2016 ACS     1.0%
## 2017 ACS     1.0%
## 2018 ACS     1.0%
## 2019 ACS     1.0%
##
## Variables
## ---------
## Type Variable    Label
## H    YEAR        Census year
## H    SAMPLE      IPUMS sample identifier
## H    SERIAL      Household serial number
## H    CBSERIAL    Original Census Bureau household serial number
## H    NUMPREC     Number of person records following
## H    HHWT        Household weight
## H    CLUSTER     Household cluster for variance estimation
## H    REGION      Census region and division
## H    STATEFIP    State (FIPS code)
## H    COUNTYFIP   County (FIPS code)
## H    DENSITY     Population-weighted density of PUMA
## H    STRATA      Household strata for variance estimation
## H    GQ          Group quarters status
## H    ROOMS       Number of rooms
## H    CINETHH     Access to internet
## H    CILAPTOP    Laptop, desktop, or notebook computer
## H    CISMRTPHN   Smartphone
## H    CITABLET    Tablet or other portable wireless computer
## H    CIHAND      Handheld computer
## H    CIOTHCOMP   Other computer equipment
## H    CIDATAPLN   Cellular data plan for a smartphone or other mobile device
## H    CIHISPEED   Broadband (high speed) Internet service such as cable,
##                    fiber optic, or DSL service
## H    CISAT       Satellite internet service
## H    CIDIAL      Dial-up service
## H    CIOTHSVC    Other internet service
## H    NFAMS       Number of families in household
## P    PERNUM      Person number in sample unit
## P    PERWT       Person weight
## P    SEX         Sex
## P    AGE         Age
## P    RACE        Race [general version]
## P    RACED       Race [detailed version]
## P    HISPAN      Hispanic origin [general version]
## P    HISPAND     Hispanic origin [detailed version]
## P    SCHOOL      School attendance
## P    EDUC        Educational attainment [general version]
## P    EDUCD       Educational attainment [detailed version]
## P    GRADEATT    Grade level attending [general version]
## P    GRADEATTD   Grade level attending [detailed version]
## P    EMPSTAT     Employment status [general version]
## P    EMPSTATD    Employment status [detailed version]
## P    INCTOT      Total personal income
## P    FTOTINC     Total family income
## P    POVERTY     Poverty status

df <- read_dta(file.path(ipm_dir, "usa_00007.dta"),
               ## drop variables we don't need to reduce size
               col_select = !any_of(c("cbserial",
                                      "serial",
                                      "sample",
                                      "numprec",
                                      "nfams",
                                      "cluster",
                                      "countyfip",
                                      "density",
                                      "strata",
                                      "school",
                                      "gradeattd",
                                      "educd",
                                      "empstat",
                                      "empstatd",
                                      "pernum",
                                      "hhwt",
                                      "rooms"))) |>
  ## drop if group quarters are institutions / other since no bb measures
  filter(!(gq %in% c(3,4))) |>
  ## drop gq since no longer need
  select(-gq) |>
  ## filter to 2016+ since broadband definition changes
  filter(year >= 2016) |>
  ## drop if cihispeed == 0 [n/a (gq)]
  filter(cihispeed != 0) |>
  ## keep only those at undergraduate level of schooling post HS
  filter(gradeatt == 6 & educ >= 6) |>
  ## join CPI data
  left_join(cpi, by = "year") |>
  ## replace missing income codes with actual NA values
  ## NB: for the != value, case_when() will return NA of same type,
  ##     so we keep labels on values that aren't missing
  mutate(across(contains("inc"),
                ~ case_when(.x != 9999999 ~ .x))) |>
  ## inflation adjust income data to 2019 dollars
  mutate(across(contains("inc"),
                ~ .x / inf_adj)) |>
  ## create new race/ethnicity catergorical variable
  mutate(hispanic = ifelse(hispan == 0, 0, 1),
         re_group = case_when(
           hispanic == 0 ~ race,
           hispanic == 1 ~ 10
         ),
         re = case_when(
           hispanic == 0 ~ raced,
           hispanic == 1 ~ hispand + 1000
         )) |>
  ## create dummy for gender (binary due to IPUMS)
  mutate(female = sex - 1) |>
  ## create income and age bands
  mutate(inc_cat = case_when(
           inctot < 0 ~ 1,
           inctot >= 0 & inctot < 10000 ~ 2,
           inctot >= 10000 & inctot < 20000 ~ 3,
           inctot >= 20000 & inctot < 30000 ~ 4,
           inctot >= 30000 & inctot < 40000 ~ 5,
           inctot >= 40000 & inctot < 50000 ~ 6,
           inctot >= 50000 & inctot < 60000 ~ 7,
           inctot >= 60000 & inctot < 70000 ~ 8,
           inctot >= 70000 & inctot < 80000 ~ 9,
           inctot >= 80000 & inctot < 90000 ~ 10,
           inctot >= 90000 & inctot < 100000 ~ 11,
           inctot >= 100000 ~ 12,
           is.na(inctot) ~ 13
         ),
         finc_cat = case_when(
           ftotinc < 0 ~ 1,
           ftotinc >= 0 & ftotinc < 10000 ~ 2,
           ftotinc >= 10000 & ftotinc < 20000 ~ 3,
           ftotinc >= 20000 & ftotinc < 30000 ~ 4,
           ftotinc >= 30000 & ftotinc < 40000 ~ 5,
           ftotinc >= 40000 & ftotinc < 50000 ~ 6,
           ftotinc >= 50000 & ftotinc < 60000 ~ 7,
           ftotinc >= 60000 & ftotinc < 70000 ~ 8,
           ftotinc >= 70000 & ftotinc < 80000 ~ 9,
           ftotinc >= 80000 & ftotinc < 90000 ~ 10,
           ftotinc >= 90000 & ftotinc < 100000 ~ 11,
           ftotinc >= 100000 ~ 12,
           is.na(ftotinc) ~ 13
         ),
         age_cat = case_when(
           age < 10 ~ 0,
           age >= 10 & age < 20 ~ 1,
           age >= 20 & age < 30 ~ 2,
           age >= 30 & age < 40 ~ 3,
           age >= 40 & age < 50 ~ 4,
           age >= 50 & age < 60 ~ 5,
           age >= 60 & age < 70 ~ 6,
           age >= 70 & age < 80 ~ 7,
           age >= 80 & age < 90 ~ 8,
           age >= 90 & age < 100 ~ 9,
           age >= 100 ~ 10
         )) |>
  ## create general broadband indicator
  mutate(broadband = ifelse(cihispeed >= 10 & cihispeed < 20, 1, 0),
         dataplano = ifelse(cihispeed == 20 & cidatapln == 1, 1, 0))

## -----------------------------------------------------------------------------
## make data files for model and save
## -----------------------------------------------------------------------------

## get sufficient stats cell counts
df <- df |>
  group_by(region, statefip, female, re, re_group, finc_cat, age_cat) |>
  summarise(pop = sum(perwt),
            tot = n(),
            broadband = sum(broadband),
            dataplano = sum(dataplano),
            .groups = "drop") |>
  mutate(region = as.integer(region / 10)) |>
  mutate(across(everything(), as.integer))

## create indicator values for stan: states
stnum <- df |>
  distinct(statefip) |>
  arrange(statefip) |>
  mutate(stnum = row_number())

## create indicator values for stan: raceeth
renum <- df |>
  distinct(re) |>
  arrange(re) |>
  mutate(renum = row_number())

## create indicator values for stan: state X raceeth
stre <- df |>
  distinct(statefip, re) |>
  arrange(statefip) |>
  mutate(stre = row_number())

## join back into main data and arrange
df <- df |>
  left_join(stnum, by = "statefip") |>
  left_join(renum, by = "re") |>
  left_join(stre, by = c("statefip", "re")) |>
  arrange(stnum, region, finc_cat, age_cat, female, renum, stre)

## save
saveRDS(df, file.path(cln_dir, "ipums_analysis.RDS"))

## -----------------------------------------------------------------------------
## make crosswalks
## -----------------------------------------------------------------------------

df <- read_dta(file.path(ipm_dir, "usa_00007.dta"),
               ## only race/ethnicity variables for crosswalk
               col_select = any_of(c("race", "raced", "hispan", "hispand")))

## race/ethnicity
raced <- df |>
  distinct(raced) |>
  mutate(code = as.numeric(raced),
         code2 = code,
         label = paste0(code, " - ", as_factor(raced) |> str_to_title())) |>
  select(code, code2, label) |>
  arrange(code) |>
  ## correct some labels
  mutate(label = gsub(":$", "", label),
         label = gsub(" And ", " and ", label),
         label = gsub("\\(S\\)", "(s)", label),
         label = gsub("\\(2000,[Aa][Cc][Ss]\\)", "", label),
         label = gsub("\\(2000/ACS\\)", "", label),
         label = gsub("\\(2000 1%\\)", "", label),
         label = gsub("Acs", "ACS", label),
         label = gsub("Aian", "AI/AN", label),
         label = gsub(" Pi,", " PI,", label),
         label = gsub(" Pi", " PI", label),
         label = gsub("PIma", "Pima", label),
         label = gsub("Write_in", "(W.I.)", label),
         label = gsub(", N\\.s", " (N.S.)", label),
         label = gsub(", N\\.e\\.c", " (N.E.C.)", label),
         label = gsub("\\(Hindu 1920_1940\\)", "", label),
         label = gsub("^(324 - .+) \\(.+$", "\\1", label),
         label = gsub("^(976 - ).+$",
                      "\\1Two Specified Asian, NH/PI, and Other Race", label),
         label = gsub("^(944 - ).+$",
                      "\\1Asian, NH/PI, and Other Race", label),
         label = str_trim(label))

## hispanic
hispand <- df |>
  distinct(hispand) |>
  mutate(code = as.numeric(hispand),
         code2 = code + 1000,
         label = paste0(code, " - ", as_factor(hispand) |> str_to_title())) |>
  filter(code != 0) |>
  select(code, code2, label) |>
  arrange(code) |>
  mutate(label = gsub(", N\\.s", " (N.S.)", label),
         label = gsub(", N\\.e\\.c", " (N.E.C.)", label))

## save combined
saveRDS(bind_rows(raced, hispand), file.path(cln_dir, "re_crosswalk.RDS"))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
