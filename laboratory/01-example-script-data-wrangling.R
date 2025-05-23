# example script 1: data wrangling
# spring 2025
# example estimation of average treatment effect - script 1
# questions: joseph.bulbulia@vuw.ac.nz

# restart fresh session if needed
# rstudioapi::restartSession()


# load libraries ---------------------------------------------------------
# install and load 'margot' package if not already installed
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot") # ensure version is at least 1.0.21
}

# install and load 'boilerplate' package if not already installed
if (!require(boilerplate, quietly = TRUE)) {
  devtools::install_github("go-bayes/boilerplate")
}

# load required libraries
library(margot)
library(boilerplate)
library(tidyverse)
library(qs)
library(here)


# set seed ----------------------------------------------------------------

# set seed for reproducibility
set.seed(123)

# save paths -------------------------------------------------------------------
# specify the path where data will be saved
# this is the path used by joseph
# push_mods <- here::here('/Users/joseph/v-project\ Dropbox/data/courses/25-psych-434')
# replace with your own path after creating a data file
pull_data <- here::here("data")

# create a folder called "models" in the main directory
dir.create("models")

# then create a link to this directory using `here::here()`
push_mods <- here::here("models")

# data directory
pull_mods <- here::here("data")



# # save data to your directory for later use and comment the above
# margot::here_save_qs(df_nz_long,"df_nz_long" ,push_mods)

# comment the above and henceforth read your data: 
# remove old data 
rm(df_nz_long)

# read data from saved file
df_nz_long <- margot::here_read_qs("df_nz_long", pull_mods)

# define study variables --------------------------------------------------------

# view variable names in the dataset
glimpse(df_nz_long)

# count the number of unique participants
length(unique(df_nz_long$id))

# define exposure variable
name_exposure <- c("hours_community")
var_labels_exposure = c("hours_community" = "Weekly Hours Community Socialising",
                        "hours_community_binary" = "Weekly Hours Community Socialising (binary)")

# save variable labels for manuscript
here_save(var_labels_exposure, "var_labels_exposure")

# define variable names for binary exposure
name_exposure_binary <- paste0(name_exposure, "_binary")
t0_name_exposure_continuous <- paste0("t0_", name_exposure)
t0_name_exposure_binary <- paste0("t0_", name_exposure, "_binary")

# define wide variable names
t0_name_exposure <- paste0("t0_", name_exposure)
t0_name_exposure_continuous <- paste0("t0_", name_exposure)
t0_name_exposure_binary <- paste0("t0_", name_exposure, "_binary")

# define variable names for exposures used in models
t1_name_exposure <- paste0("t1_", name_exposure)
t1_name_exposure_binary <- paste0("t1_", name_exposure, "_binary")
# define study waves -----------------------------------------------------------
baseline_wave <- "2018"
exposure_waves <- c("2019")
outcome_wave <- "2020"

# define wave combinations for analysis
all_waves <- c(baseline_wave, exposure_waves, outcome_wave)
baseline_and_exposure_waves <- c(baseline_wave, exposure_waves)
baseline_and_outcome_waves <- c(baseline_wave, outcome_wave)

# define scale ranges ----------------------------------------------------------
scale_range_exposure <- c(0, 20)  # used for descriptive graphs
scale_ranges_outcomes <- c(1, 7)  # used for descriptive graphs

# check package versions
packageVersion(pkg = 'margot')    # make sure it is 1.0.19 or greater
packageVersion(pkg = 'boilerplate')

# load required packages -------------------------------------------------------
pacman::p_load(
  # causal inference
  clarify,      # sensitivity analysis
  cobalt,       # covariate balance tables and plots
  lmtp,         # longitudinal targeted maximum likelihood estimation
  margot,       # functions for causal inference
  MatchIt,      # matching methods
  MatchThem,    # matching for multiply imputed datasets
  policytree,   # causal inference with policy trees
  WeightIt,     # weighting methods for covariate balancing
  
  # data processing
  data.table,   # fast data wrangling
  fastDummies,  # fast creation of dummy variables
  fs,           # cross-platform file system operations
  here,         # simple and robust file referencing
  janitor,      # data cleaning and validation
  naniar,       # handling and visualization of missing data
  skimr,        # summary statistics for data frames
  tidyverse,    # collection of "R" packages for data science
  
  # machine learning
  glmnet,       # lasso and elastic-net regularized models
  grf,          # generalized random forests
  ranger,       # fast implementation of random forests
  SuperLearner, # ensemble learning
  xgboost,      # extreme gradient boosting
  
  # visualization
  DiagrammeR,   # graph and network visualization
  ggbeeswarm,   # data visualization   
  ggplot2,      # data visualization
  gt,           # "HTML" tables for data frames
  gtsummary,    # summary tables for regression models
  kableExtra,   # advanced table formatting
  
  # parallel processing
  doParallel,   # parallel processing with foreach
  progressr,    # progress reporting for "R"
  
  # analysis
  parameters,   # parameters and performance metrics
  EValue        # compute e-values
)
# data preparation -------------------------------------------------------------
# import and prepare data

# get total sample size
n_total <- skimr::n_unique(df_nz_long$id)
n_total <- margot::pretty_number(n_total)
margot::here_save(n_total, "n_total")

# view
n_total

# Define Baseline Variables ----------------------------------------------------
baseline_vars <- c(
  # note all outcomes will be added to baseline vars later. 
  # demographics
  "age",
  "born_nz_binary",
  "education_level_coarsen",
  "employed_binary",
  "eth_cat",
  "male_binary",
  "not_heterosexual_binary",
  "parent_binary",
  "partner_binary",
  "rural_gch_2018_l",
  "sample_frame_opt_in_binary",
  
  # personality traits
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "neuroticism",
  "openness",
  
  # health and lifestyle
  "alcohol_frequency",
  "alcohol_intensity",
  "hlth_disability_binary",
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_household_inc",
  # "log_hours_community", # commented out because using as exposure
  "short_form_health",
  "smoker_binary",
  
  # social and psychological
  "belong",
  "nz_dep2018",
  "nzsei_13_l",
  "political_conservative",
  "religion_identification_level"
)

# Sort baseline variables
baseline_vars <- sort(baseline_vars)


# baseline vars no log ----------------------------------------------------

baseline_vars_no_log <- c(
  # Demographics
  "age",
  "born_nz_binary",
  "education_level_coarsen",
  "eth_cat",
  "employed_binary",
  "male_binary",
  "not_heterosexual_binary",
  "parent_binary",
  "partner_binary",
  "rural_gch_2018_l",
  "sample_frame_opt_in_binary",
  
  # Personality traits
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "neuroticism",
  "openness",
  
  # Health and lifestyle
  "alcohol_frequency",
  "alcohol_intensity",
  "hlth_disability_binary",
  "log_hours_children",
  "hours_children",
  "log_hours_commute",
  "hours_commute",
  "log_hours_exercise",
  "hours_exercise",
  "log_hours_housework",
  "hours_housework",
  "log_household_inc",
  "household_inc",
  # "log_hours_community",
  "hours_community",
  "short_form_health",
  "smoker_binary",
  
  # Social and psychological
  "belong",
  "nz_dep2018",
  "nzsei_13_l",
  "political_conservative",
  "religion_identification_level"
)

# define outcome variables ----------------------------------------------------
# define outcome variables without log transformation
# outcomes
outcome_vars <- c(
  "alcohol_frequency",
  "alcohol_intensity",
  "belong",
  "bodysat",
  "forgiveness",
  "gratitude",
  "hlth_bmi",
  "hlth_fatigue",
  "hlth_sleep_hours",
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "lifesat",
  "log_hours_exercise",
  "meaning_purpose", 
  "meaning_sense",
  "neighbourhood_community",
  "perfectionism",
  "pwi", 
  "rumination",
  "self_control",
  "self_esteem",
  "sexual_satisfaction",
  "short_form_health",
  "support"
)

# sort and save outcome variables
outcome_vars <- sort(outcome_vars)
here_save(outcome_vars, "outcome_vars")

# Create standardized outcome variables
t2_outcome_vars_z <- paste0("t2_", outcome_vars, "_z")

# view the result
t2_outcome_vars_z


# define outcome variables without log transformation
# for tables
outcome_vars_no_log <- c(
  "alcohol_frequency",
  "alcohol_intensity",
  "belong",
  "bodysat",
  "forgiveness",
  "gratitude",
  "hlth_bmi",
  "hlth_fatigue",
  "hlth_sleep_hours",
  "hours_exercise", # logged 
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "lifesat",
  "meaning_purpose",
  "meaning_sense",
  "neighbourhood_community",
  "perfectionism",
  "pwi", 
  "rumination",
  "self_control",
  "self_esteem",
  "sexual_satisfaction",
  "short_form_health",
  "support"
)

# exposure_var
exposure_var = c(name_exposure, name_exposure_binary)

# check
# exposure_var_continuous = exposure_var[[1]]
# exposure_var_continuous
# 
# exposure_var_binary =  exposure_var[[2]]
# exposure_var_binary

# raw outcomes 
raw_outcomes_health <- c(
  "alcohol_frequency",
  "alcohol_intensity",
  "hlth_bmi", 
  "log_hours_exercise", 
  "hlth_sleep_hours", 
  "short_form_health"
)
# sort
raw_outcomes_health <- sort(raw_outcomes_health)

# save 
here_save(raw_outcomes_health, "raw_outcomes_health")

# define psych outcomes
raw_outcomes_psych <- c( 
  "hlth_fatigue", 
  "kessler_latent_anxiety", 
  "kessler_latent_depression",  
  "rumination"
)

# sort
raw_outcomes_psych <- sort(raw_outcomes_psych)

# save
here_save(raw_outcomes_psych, "raw_outcomes_psych")

# define present outcomes
raw_outcomes_present <- c(
  "bodysat",
  "forgiveness",
  "perfectionism", 
  "self_control" , 
  "self_esteem", 
  "sexual_satisfaction" 
)

# sort
raw_outcomes_present <- sort(raw_outcomes_present)

# save
here_save(raw_outcomes_present, "raw_outcomes_present")

# define life outcomes
raw_outcomes_life <- c( 
  "gratitude", 
  "lifesat", 
  "meaning_purpose", 
  "meaning_sense",
  "pwi"  # move personal well-being here if not using individual facents
)

# sort
raw_outcomes_life <- sort(raw_outcomes_life)

# save
here_save(raw_outcomes_life, "raw_outcomes_life")

# define social outcomes
raw_outcomes_social <- c(
  "belong",
  "neighbourhood_community", 
  "support" 
)
# sort
raw_outcomes_social <- sort(raw_outcomes_social)

# save
here_save(raw_outcomes_social, "raw_outcomes_social")

# save for publication
raw_outcomes_all <- c(raw_outcomes_health, 
                      raw_outcomes_psych,
                      raw_outcomes_present, 
                      raw_outcomes_life, 
                      raw_outcomes_social)

# save
here_save(raw_outcomes_all, "raw_outcomes_all")

#table1::table1(~ religion_spiritual_identification |wave, data = dat)
outcome_vars <-sort(outcome_vars)
outcome_vars_no_log<-sort(outcome_vars_no_log)
extra_vars <- c("id", "wave", "year_measured", "not_lost", "sample_weights") 
all_vars <- c(baseline_vars, exposure_var, outcome_vars, extra_vars)

extra_vars_table <- extra_vars
not_all_vars<- c(baseline_vars_no_log, exposure_var, outcome_vars_no_log, extra_vars_table)

# sort
all_vars <- sort(all_vars)
not_all_vars <- sort(not_all_vars)

# define columns that will later be handled in a special way
# define continuous columns that we will not standardise
continuous_columns_keep <- c("t0_sample_weights")


# define ordinal columns that we will expand into binary variables
ordinal_columns <- c("t0_education_level_coarsen", "t0_eth_cat", "t0_rural_gch_2018_l")

# checks
overlap <- intersect(baseline_vars, outcome_vars)
print(overlap)

# read and preprocess data ------------------------------------------------
df_nz_long$hours_community
yola
# prepare initial dataframe -----------------------------------------------
dat_prep <- df_nz_long |>
  arrange(id, wave) |>
  as.data.frame() |>
  margot::remove_numeric_attributes() |>
  mutate(sample_weights = sample_weights) |> 
  mutate(religion_church = round( ifelse(religion_church > 8, 8, religion_church)),1) |>  # to simplify
  arrange(id, wave) |>
  droplevels()

# define variable names using paste0 # *-- needed in this study --*
# name_exposure_cat <- paste0(name_exposure, "_cat")
# name_exposure_binary <- paste0(name_exposure, "_binary")

# define wide variable names
t0_name_exposure <- paste0("t0_", name_exposure)
t1_name_exposure <- paste0("t1_", name_exposure)
here_save(t0_name_exposure , 't0_name_exposure')

# eligibility  ------------------------------------------------------------
# select baseline and exposure ids based on eligibility criteria 
ids_baseline <- dat_prep |>
  filter(year_measured == 1, wave == baseline_wave) |> 
  filter(!is.na(!!sym(name_exposure))) |> # exposure observed at baseline
  pull(id)

# get number of participants at baseline
n_participants <- length(ids_baseline)
n_participants <- margot::pretty_number(n_participants)

# check number of participants
n_participants

# save number of participants
here_save(n_participants, "n_participants")

# select ids for baseline cohort ------------------------------------------
# eligibility: participated in baseline wave, no missingness in the exposure
# may have been lost to follow up
dat_long_1 <- dat_prep |>
  filter(id %in% ids_baseline & wave %in% c(baseline_wave, exposure_waves, outcome_wave)) |> 
  droplevels()

# check wave structure
str(dat_long_1$wave)

# aside -------------------------------------------------------------------
# example of censoring with more conditions

# censor exposure if lost
# ids_baseline_2 <- dat_long_censored_0 |>
#   filter(year_measured == 1, wave == baseline_wave, employed == 1, hours_work >= 20) |>
#   pull(id)
# 
# dat_long_censored <- dat_long_censored_0 |>
#   filter(id %in% ids_baseline_2 & wave %in% c(baseline_wave, exposure_waves, outcome_wave)) |> 
#   droplevels()

# check for specific conditions in a subset
# subset_vals <- dat_long_censored$year_measured[
#   dat_long_censored$wave == 2021 & dat_long_censored$employed == 0
# ]

# check if all year_measured are zero in this subset
# all_zero <- all(subset_vals == 0, na.rm = TRUE)

# view result
# all_zero

# data with eligibility criteria
# dat_long_1 <- dat_long_censored


# aside over --------------------------------------------------------------

# evaluate exposure variable(s)
dat_long_exposure <- dat_long_1 |>
  filter(wave == exposure_waves) |>
  droplevels()

# check wave counts
table(dat_long_exposure$wave)

# check missingness in religion_church
table(is.na(dat_long_exposure[[exposure_var]]))

# check exposure variable name
name_exposure

# check missingness in exposure
# naniar::gg_miss_var(dat_long_exposure)

# check quantile breaks for exposure
quantile(dat_long_exposure[[name_exposure]], na.rm = TRUE, probs = seq(0, 1, .25))
mean(dat_long_exposure[[name_exposure]], na.rm = TRUE)
median(dat_long_exposure[[name_exposure]], na.rm = TRUE)
max(dat_long_exposure[[name_exposure]], na.rm = TRUE)
sd(dat_long_exposure[[name_exposure]], na.rm = TRUE)

# count above and below break at one
sum(dat_long_exposure[[name_exposure]] >= 1, na.rm = TRUE)
sum(dat_long_exposure[[name_exposure]] < 1, na.rm = TRUE)

# binary graph ------------------------------------------------------------
# graph binary break at lower quartile (5)
graph_exposure_binary <- margot::margot_plot_categorical(
  dat_long_exposure,
  col_name = name_exposure,
  cutpoint_inclusive = "lower",
  # n_divisions = 2,
  custom_breaks = c(0,1),
  binwidth = .5)

# view binary graph
print(graph_exposure_binary)

# check graph size
margot_size(graph_exposure_binary)

# save binary graph for manuscript
margot::here_save_qs(graph_exposure_binary, "graph_exposure_binary", push_mods)

# resume wrangling --------------------------------------------------------
# create categorical variable (if desired)
# check exposure variable name
name_exposure

dat_long_2 <- margot::create_ordered_variable(
  dat_long_1,
  var_name = name_exposure,
  cutpoint_inclusive = "lower",
  custom_breaks = c(0, 1)
)

# check binary exposure variable name
name_exposure_binary

# check missingness in binary exposure variable
table(is.na(dat_long_2[[name_exposure_binary]]))

# convert binary factors to 0, 1
# use custom function
dat_long_3 <- margot_process_binary_vars(dat_long_2)

# make 'not lost' variable ------------------------------------------------
# used for attrition handling
dat_long_4 <- dat_long_3 |>
  arrange(id, wave) |>
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    not_lost = ifelse(is.na(not_lost) & year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  droplevels()

# log-transform 'hours_' variables ----------------------------------------
dat_long_table <- margot_log_transform_vars(
  dat_long_4,
  vars = c(starts_with("hours_"), "household_inc"),
  exceptions = name_exposure,
  prefix = "log_",
  keep_original = TRUE # set to TRUE for tables
) |>
  select(all_of(not_all_vars)) |>
  droplevels()

# make wave a factor
dat_long_table$wave <- as.factor(dat_long_table$wave)



# summary tables ----------------------------------------------------------
# useful function that creates summary tables in one place
summary_tables  <- margot_summary_tables(
  data = dat_long_table,
  baseline_wave = baseline_wave,
  exposure_waves = exposure_waves,
  outcome_wave = outcome_wave,
  name_exposure = name_exposure,
  baseline_vars = baseline_vars_no_log,
  outcome_vars = outcome_vars,
  extra_vars = extra_vars
)

# show details
summary_tables$baseline_table
summary_tables$exposure_table
summary_tables$outcome_table
summary_tables$n_participants


# newer tables (for manuscript) -------------------------------------------
# sort
var_labels_health <- list(
  "alcohol_frequency" = "Alcohol Frequency",
  "alcohol_intensity" = "Alcohol Intensity",
  "hlth_bmi" = "Body Mass Index", 
  "hlth_sleep_hours" = "Sleep", 
  "hours_exercise" = "Hours of Exercise",   # logged in models
  "short_form_health" = "Short Form Health" 
)

# define psych outcomes 
var_labels_psych <- list(
  "hlth_fatigue" = "Fatigue", 
  "kessler_latent_anxiety" = "Anxiety", 
  "kessler_latent_depression" = "Depression",  
  "rumination" = "Rumination"
)

# define present outcomes
var_labels_present <- list(
  "bodysat" = "Body Satisfaction",
  "foregiveness" = "Forgiveness",
  "perfectionism" = "Perfectionism",
  "self_control" = "Self Control",
  "self_esteem" = "Self Esteem",
  "sexual_satisfaction" = "Sexual Satisfaction"
)

# define life outcomes
var_labels_life <- list(
  "gratitude" = "Gratitude", 
  "lifesat" = "Life Satisfaction", 
  "meaning_purpose" = "Meaning: Purpose", # exposure variable
  "meaning_sense" = "Meaning: Sense",
  "pwi" = "Personal Well-being Index"
)

# define social outcome names
var_labels_social <- list(
  "belong" = "Belonging",
  "neighbourhood_community" = "Neighbourhood Belonging", 
  "support" = "Support" 
)

# make labels
var_labels_baseline <- list(
  "sdo" = "Social Dominance Orientation",
  "belong" = "Social Belonging",
  "born_nz_binary" = "Born in New Zealand (binary)",
  "rural_gch_2018_l" = "Rural Gch 2018 Levels",
  "education_level_coarsen" = "Education Level",
  "employed_binary" = "Employed (binary)",
  "eth_cat" = "Ethnicity",
  "household_inc" = "Household Income",
  "log_household_inc" = "Log Household Income",
  "male_binary" = "Male (binary)",
  "nz_dep2018" = "NZ Deprevation Index 2018",
  "nzsei_13_l" = "NZSEI (Occupational Prestige Index)",
  "parent_binary" = "Parent (binary)",
  "rwa" = "Right Wing Authoritarianism",
  "sample_frame_opt_in_binary" = "Sample Frame Opt-In (binary)",
  "sdo" = "Social Dominance Orientation",
  "smoker_binary" = "Smoker (binary)",
  "support" = "Social Support (perceived)",
  "hours_community" = "Hours Community Socialising"
)

# labels for factors
rural_labels <- c(
  "High Urban Accessibility",
  "Medium Urban Accessibility",
  "Low Urban Accessibility",
  "Remote",
  "Very Remote"
)

# save labels -------------------------------------------------------------
here_save(var_labels_baseline, "var_labels_baseline")
here_save(var_labels_health, "var_labels_health")
here_save(var_labels_psych, "var_labels_psych")
here_save(var_labels_present, "var_labels_present")
here_save(var_labels_life, "var_labels_life")
here_save(var_labels_social, "var_labels_social")


# combine all variable labels
var_labels_measures <- c(
  var_labels_baseline,
  var_labels_health,
  var_labels_psych,
  var_labels_present,
  var_labels_life,
  var_labels_social
)

# save
here_save(var_labels_measures, "var_labels_measures")

# new df for table
dat_long_table_x <- dat_long_table
dat_long_table_x$rural_gch_2018_l <- factor(
  dat_long_table_x$rural_gch_2018_l,
  levels = 1:5,
  labels = rural_labels,
  ordered = TRUE  # optional: if the levels have an inherent order
)

# nicer for church attendance -- if you use it
# dat_long_table_x$religion_church <- factor(
#   dat_long_table_x$religion_church,
#   levels = 0:8,
#   ordered = TRUE  
# )

# create dataframes for each wave
dat_long_table_baseline <- dat_long_table_x |> 
  filter(wave %in% c(baseline_wave, exposure_waves, outcome_wave))

# create dataframes for each wave
dat_long_table_exposure_waves <- dat_long_table_x |> 
  filter(wave %in% c(baseline_wave, exposure_waves))

# create dataframes for each wave
dat_long_table_outcome_waves <- dat_long_table_x |> 
  filter(wave %in% c(baseline_wave, outcome_wave))

# make tables
markdown_table_baseline <- margot_make_tables(
  data = dat_long_table_baseline,
  vars = baseline_vars_no_log,
  by = "wave",
  labels = var_labels_baseline,
  factor_vars = c("rural_gch_2018_l", "eth_cat"),
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)

# view
markdown_table_baseline

# save
margot::here_save(markdown_table_baseline, "markdown_table_baseline")

# make exposure table
markdown_table_exposures <- margot_make_tables(
  data = dat_long_table_exposure_waves,
  vars = name_exposure,
  by = "wave",
  labels = var_labels_exposure,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)

# view
markdown_table_exposures

# save
here_save(markdown_table_exposures, "markdown_table_exposures")

# names of outcome vars for health no logs
raw_outcomes_health_no_log <- c(
  "alcohol_intensity",
  "alcohol_frequency",
  "hlth_bmi", 
  "hlth_sleep_hours", 
  "hours_exercise",
  "short_form_health"
)

# make tables
# make tables
# latex_table_outcomes_all <- margot_make_tables(
#   data = dat_long_table_outcome_waves,
#   vars = raw_outcomes_all,
#   by = "wave",
#   labels = var_labels_measures,
#   table1_opts = list(overall = FALSE, transpose = FALSE),
#   format = "latex",
#   kable_opts = list(
#     booktabs = TRUE,
#     longtable = TRUE,
#     font_size = 10,
#     latex_options = c("hold_position", "repeat_header", "striped", "longtable")
#   ),
#   quarto_label = "tbl-appendix-outcomes"  # This is the key addition!
# )
# 
# cat(latex_table_outcomes_all)
# # view
# here_save(latex_table_outcomes_all, "latex_table_outcomes_all")

markdown_table_outcomes_all <- margot_make_tables(
  data = dat_long_table_outcome_waves,
  vars = raw_outcomes_all,
  by = "wave",
  labels = var_labels_measures,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown",
  quarto_label = "tbl-sample-outcomes"  # This is the key addition!
)

# view
markdown_table_outcomes_all

# save
here_save(markdown_table_outcomes_all, "markdown_table_outcomes_all")


# data for study, remove originals ----------------------------------------
## create gender weights if needed
dat_long_selected<- dat_long_4


# aside -------------------------------------------------------------------
# only if gender weights
# dat_long_selected$gender_weights <- margot_compute_gender_weights_by_wave(
#   dat_long_selected,
#   male_col = "male_binary",
#   wave_col = "wave",
#   target_wave = baseline_wave,
#   target_male_prop = 0.52 # source https://www.stats.govt.nz/news/women-in-paid-work
# )

# end aside  --------------------------------------------------------------
# checks
table(dat_long_selected$sample_weights[dat_long_selected$wave == baseline_wave])
# hist(dat_long_selected$g_sample_weights)
# hist(dat_long_selected$gender_weights)

# prepare data with log transformations
dat_long_prepare <- margot::margot_log_transform_vars(
  dat_long_selected,
  vars = c(starts_with("hours_"), "household_inc"),
  exceptions = name_exposure,
  prefix = "log_",
  keep_original = TRUE # set to false if you do not want to keep originals
) |>
  # uncomment the next two lines if using gender weights
  # select(-sample_weights) |>
  # dplyr::rename(sample_weights = gender_weights) |>
  select(all_of(all_vars)) |>
  droplevels()

# check distribution and missingness
hist(dat_long_selected$sample_weights)
table(is.na(dat_long_selected$sample_weights))
table(is.na(dat_long_selected$male_binary))

# extract baseline data
dat_baseline <- dat_long_prepare |>
  filter(wave == baseline_wave)

# check for missing values in baseline data
table(is.na(dat_baseline$sample_weights))
table(is.na(dat_baseline$male_binary))

# extract sample weights from baseline
t0_sample_weights <- dat_baseline$sample_weights # use age/gender/ethnicity
hist(dat_baseline$sample_weights)
hist(t0_sample_weights)

# alternative weights option (commented out)
# t0_sample_weights <- dat_baseline$gender_weights

# save sample weights
margot::here_save(t0_sample_weights, "t0_sample_weights")

# create tables -----------------------------------------------------------
exposure_waves

# create transition matrix for continuous exposure -----------------------
# prepare data for transition matrix
dt_positivity <- dat_long_selected |>
  filter(wave == baseline_wave | wave %in% exposure_waves) |>
  select(!!sym(name_exposure), id, wave) |>
  mutate(exposure = as.numeric(!!sym(name_exposure))) |>
  mutate(exposure = round(ifelse(exposure > 5, 5, exposure), 0)) |> 
  droplevels()

# create transition table
transition_tables <- margot_transition_table(dt_positivity, "exposure", "id", wave_var = "wave")

# check explanation of transition tables
cat(transition_tables$explanation)

# view first transition table
transition_tables$tables[[1]]

# generate code for quarto document
transition_tables$quarto_code()

# save transition tables for later use
here_save(transition_tables, "transition_tables")

# create transition matrix for binary exposure ---------------------------
# prepare data for binary transition matrix
dt_positivity_binary <- dat_long_selected |>
  filter(wave == baseline_wave | wave %in% exposure_waves) |>
  select(!!sym(name_exposure_binary), id, wave) |>
  mutate(exposure_binary = as.numeric(!!sym(name_exposure_binary))) |>
  droplevels()

# create binary transition table
transition_tables_binary <- margot_transition_table(
  dt_positivity_binary, 
  "exposure_binary", 
  "id", 
  wave_var = "wave"
)

# view binary transition table
transition_tables_binary$tables[[1]]

# check explanation for binary transition tables
cat(transition_tables_binary$explanation)

# generate code for quarto document
transition_tables_binary$quarto_code()

# save binary transition tables
margot::here_save(transition_tables_binary, "transition_tables_binary")

# check missing values ---------------------------------------------------
# examine overall missingness
naniar::miss_var_summary(dat_long_prepare) |> print(n = 100)

# filter to baseline wave
dat_baseline <- dat_long_prepare |> filter(wave == baseline_wave)

# calculate percent missing at baseline
percent_missing_baseline <- naniar::pct_miss(dat_baseline)

# view missing percentage
percent_missing_baseline

# save for manuscript
here_save(percent_missing_baseline, "percent_missing_baseline")

# visualise missingness patterns
naniar::vis_miss(dat_baseline, warn_large_data = FALSE)

# visualise missingness by variable
naniar::gg_miss_var(dat_baseline)

# save the data ----------------------------------------------------------
# save prepared dataset
margot::here_save(dat_long_prepare, "dat_long_prepare")
margot::here_save(name_exposure, "name_exposure")

# save variable names for later use
margot::here_save(baseline_vars, "baseline_vars")
margot::here_save(exposure_var, "exposure_var")
margot::here_save(outcome_vars, "outcome_vars")
margot::here_save(baseline_wave, "baseline_wave")
margot::here_save(exposure_waves, "exposure_waves")
margot::here_save(outcome_wave, "outcome_wave")
margot::here_save(continuous_columns_keep, "continuous_columns_keep")
margot::here_save(ordinal_columns, "ordinal_columns")

# visualise individual patterns ------------------------------------------
# create plot of individual response trajectories
individual_plot_exposure <- margot_plot_individual_responses(
  dat_long_1,
  y_vars = name_exposure,
  id_col = "id",
  waves = c(2018:2019), # only measured for two waves
  theme = theme_classic(),
  random_draws = 80,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  # save_path = here::here(push_mods), # uncomment to save
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)

# display individual trajectories plot
individual_plot_exposure

# check plot dimensions
margot_size(individual_plot_exposure)

