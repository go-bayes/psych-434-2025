# 02-data-wrangling-grf-model
# get data into wide format and ready for modelling using grf
# joseph.bulbulia@gmail.com
# april 2025

# restart fresh session
# rstudioapi::restartSession()
# set reproducibility
set.seed(123)

# save paths -------------------------------------------------------------------
# create a save path to your on computer
# this is mine
# push_mods <- here::here('/Users/joseph/v-project\ Dropbox/data/courses/25-psych-434')
# yours might be (after creating a data file)
push_mods <- here::here("data")

# load libraries ---------------------------------------------------------
# install and load 'margot' package
# make sure you have at least margot 1.0.21
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
}

if (!require(boilerplate, quietly = TRUE)) {
  devtools::install_github("go-bayes/boilerplate")
}
# load margot library
library(margot)

# load necessary libraries
pacman::p_load(
  clarify,     # sensitivity analysis for causal inference
  cobalt,      # covariate balance tables and plots
  DiagrammeR,  # graph and network visualization
  doParallel,  # parallel processing with foreach
  fastDummies, # fast creation of dummy variables
  fs,          # cross-platform file system operations
  ggbeeswarm,  # data visualisation
  ggplot2,     # data visualisation
  glmnet,      # lasso and elastic-net regularized models
  grf,         # generalized random forests
  gt,          # html tables for data frames
  gtsummary,   # summary tables for regression models
  here,        # simple and robust file referencing
  janitor,     # data cleaning and validation
  kableExtra,  # advanced table formatting
  lmtp,        # longitudinal targeted maximum likelihood estimation
  MatchIt,     # matching methods for causal inference
  MatchThem,   # matching methods for multiply imputed datasets
  naniar,      # handling and visualization of missing data
  parameters,  # parameters and performance metrics
  policytree,  # causal inference with policy trees
  progressr,   # progress reporting for R
  ranger,      # fast implementation of random forests
  skimr,       # summary statistics for data frames
  SuperLearner,# ensemble learning
  tidyverse,   # collection of R packages for data science
  WeightIt,    # weighting methods for covariate balancing
  xgboost,     # extreme gradient boosting
  EValue,      # compute Evalues
  data.table,  # fast data wrangling
  maq,         # qini curves
  purrr,       # data wrangling
  patchwork,   # multiple plots
  labelled,
  purrr,
  boilerplate
)

# check
push_mods

# read data in
dat_long_prepare <- margot::here_read("dat_long_prepare")
name_exposure <- margot::here_read("name_exposure")

# check
name_exposure_binary = paste0(name_exposure, "_binary")
name_exposure_continuous = name_exposure

# check
name_exposure_binary
name_exposure_continuous

# get vars
baseline_vars <- margot::here_read("baseline_vars")
outcome_vars <- margot::here_read("outcome_vars")

baseline_wave <- margot::here_read("baseline_wave")
exposure_waves <- margot::here_read("exposure_waves")
outcome_wave <- margot::here_read("outcome_wave")
t0_sample_weights <- margot::here_read("t0_sample_weights")

# define wide variable names
t0_name_exposure_binary <- paste0("t0_", name_exposure_binary)

# make exposure names (continuous not generally used)
t1_name_exposure_binary <- paste0("t1_", name_exposure_binary)
t1_name_exposure_binary

t0_name_exposure_continuous <- paste0("t0_", name_exposure)
t0_name_exposure_continuous

# make exposure names (continuous not generally used)
t1_name_exposure_continuous <- paste0("t1_", name_exposure)
t1_name_exposure_continuous

# ordinal use
ordinal_columns <- c(
  "t0_education_level_coarsen",
  "t0_eth_cat",
  "t0_rural_gch_2018_l",
  "t0_gen_cohort"
)

# keep sample weights without standardisation
continuous_columns_keep <- c("t0_sample_weights")

# prepare data for analysis ----------------------
dat_long_prepare <- margot::remove_numeric_attributes(dat_long_prepare)

# update wave to numeric
dat_long_prepare_updated <- dat_long_prepare %>%
  mutate(wave = as.numeric(factor(wave, levels = sort(unique(wave)))))

# make both exposure variables
name_exposure_both <- c(name_exposure_binary,name_exposure_continuous)
# check
name_exposure_both

# wide data
df_wide <- margot::margot_wide(
  dat_long_prepare_updated,
  baseline_vars = baseline_vars,
  exposure_var = name_exposure_both,
  outcome_vars = outcome_vars
)

# check
head(df_wide)

# check
# remove baseline binary
t0_sample_weights

# add weights back to data
df_wide$t0_sample_weights <- t0_sample_weights

# make sure that rural is a factor
df_wide$t0_rural_gch_2018_l <- as.factor(df_wide$t0_rural_gch_2018_l)

# save the wide data
margot::here_save(df_wide, "df_wide")

# naniar::vis_miss(df_wide, warn_large_data = FALSE)
# read back if needed
# df_wide <- margot::here_read("df_wide")

# check
colnames(df_wide)

# this will order the data correctly
# see margot documentation
# standardise outcomes, create not-lost indicator
df_wide_encoded <- margot::margot_process_longitudinal_data_wider(
  df_wide,
  ordinal_columns = ordinal_columns,
  continuous_columns_keep = continuous_columns_keep,
  not_lost_in_following_wave = "not_lost_following_wave",
  lost_in_following_wave = "lost_following_wave",
  remove_selected_columns = TRUE,
  exposure_var = name_exposure_both,
  scale_continuous = TRUE,
  censored_if_any_lost = FALSE # not relevant here
)

# check
colnames(df_wide_encoded)

# check
table(df_wide_encoded$t0_not_lost_following_wave)

# make the binary variable numeric! 
df_wide_encoded[[t0_name_exposure_binary]] <- as.numeric(df_wide_encoded[[t0_name_exposure_binary]]) - 1
df_wide_encoded[[t1_name_exposure_binary]] <- as.numeric(df_wide_encoded[[t1_name_exposure_binary]]) - 1

# check
table(df_wide_encoded[[t0_name_exposure_binary]])
table(df_wide_encoded[[t1_name_exposure_binary]])

# save data for models
here_save_qs(df_wide_encoded, "df_wide_encoded", push_mods)

# save data for models
# make exposure numeric

# check
colnames(df_wide_encoded)
table(is.na(df_wide_encoded[[t1_name_exposure_binary]]))
table(df_wide_encoded$t0_lost_following_wave)

# check
stopifnot(
  all(
    is.na(df_wide_encoded[[t1_name_exposure_binary]]) |
      df_wide_encoded[["t0_not_lost_following_wave"]] == 1
  )
)

# naniar::vis_miss(df_wide_encoded, warn_large_data = FALSE)
naniar::gg_miss_var(df_wide_encoded)

df_wide_encoded$t0_sample_weights

# predict attrition and create censoring weights --------------------------
# step 1: prepare baseline covariates
E <- df_wide_encoded %>%
  select(
    -all_of(t0_name_exposure_binary), # inserted by function but irrelevant
    -"t0_sample_weights") %>%
  select(starts_with("t0_"),
         -ends_with("_lost"),
         -ends_with("lost_following_wave")) %>%
  colnames() %>%
  sort()

# note for baseline confounding we use the continuous var
E # we use the continuous variable

# save baseline covariates
margot::here_save(E, "E")

# view
print(E)

# step 2: calculate weights for t0
D_0 <- as.factor(df_wide_encoded$t0_lost_following_wave)

# get covariates
cen_0 <- df_wide_encoded[, E]

# probability forest for censoring
cen_forest_0 <- probability_forest(cen_0, D_0)

# save if needed, very large!
# here_save_qs(cen_forest_0, "cen_forest_0", push_mods)

# predict
predictions_grf_0 <- predict(cen_forest_0, newdata = cen_0, type = "response")

# get probability of censoring
pscore_0 <- predictions_grf_0$pred[, 2]

# view
hist(pscore_0)

# check correct sample weights  
hist(t0_sample_weights)

# use margot_adjust_weights for t0
t0_weights <- margot_adjust_weights(
  pscore = pscore_0,
  trim = TRUE,
  normalize = TRUE,
  # lower trimming
  lower_percentile = 0.01,
  upper_percentile = 0.99,
  # upper trimming
  censoring_indicator = df_wide_encoded$t0_lost_following_wave,
  sample_weights = df_wide_encoded$t0_sample_weights 
)

# view
length(t0_weights$adjusted_weights)
length(df_wide_encoded$t0_sample_weights)

# check
nrow(df_wide_encoded)
hist(t0_weights$adjusted_weights)

# assign weights
df_wide_encoded$t0_adjusted_weights <- t0_weights$adjusted_weights

# if you only wanted censoring weights
# df_wide_encoded$t0_propensity_score_model_weights <- t0_weights$censoring_weights

# check
naniar::vis_miss(df_wide_encoded, warn_large_data = FALSE)

# view
head(df_wide_encoded)
colnames(df_wide_encoded)

# remove lost next wave (censored)
df_wide_encoded_1 <- df_wide_encoded %>%
  filter(t0_lost_following_wave == 0) %>%
  droplevels()

# step 4: calculate weights for t1
E_and_exposure <- c(E, t1_name_exposure_continuous)
D_1 <- as.factor(df_wide_encoded_1$t1_lost_following_wave)
cen_1 <- df_wide_encoded_1[, E_and_exposure]

# probability forest for censoring
cen_forest_1 <- probability_forest(cen_1, D_1, sample.weights = df_wide_encoded_1$t0_adjusted_weights)

# save if needed, very large
# here_save(cen_forest_1, "cen_forest_1")

predictions_grf_1 <- predict(cen_forest_1, newdata = cen_1, type = "response")
pscore_1 <- predictions_grf_1$pred[, 2]

# check
hist(pscore_1)

# use margot_adjust_weights for t1
t1_weights <- margot_adjust_weights(
  pscore = pscore_1,
  trim = TRUE,
  normalize = TRUE,
  lower_percentile = 0.01,
  upper_percentile = 0.99,
  # upper trimming
  censoring_indicator = df_wide_encoded_1$t1_lost_following_wave,
  sample_weights = df_wide_encoded_1$t0_adjusted_weights # combine with weights
)

# check
hist(t1_weights$adjusted_weights)

# add weights -- these will be the weights we use
df_wide_encoded_1$t1_adjusted_weights <- t1_weights$adjusted_weights

# calculate summary statistics
t0_adjusted_weight_summary <- summary(df_wide_encoded$t0_adjusted_weights)
t1_adjusted_weight_summary <- summary(df_wide_encoded_1$t1_adjusted_weights)

# view summaries
t0_adjusted_weight_summary
t1_adjusted_weight_summary

# check
naniar::vis_miss(df_wide_encoded_1, warn_large_data = FALSE)

# save
here_save(df_wide_encoded_1, "df_wide_encoded_1")

# read if needed
# df_wide_encoded_1 <- here_read("df_wide_encoded_1")

# check names
colnames(df_wide_encoded_1)

# check
df_wide_encoded_1[[t1_name_exposure_binary]]

# step 5: prepare final dataset
nrow(df_wide_encoded_1)
table(df_wide_encoded_1$t1_lost_following_wave)

# arrange
df_grf <- df_wide_encoded_1 |>
  filter(t1_lost_following_wave == 0) |>
  select(
    where(is.factor),
    ends_with("_binary"),
    ends_with("_lost_following_wave"),
    ends_with("_z"),
    ends_with("_weights"),
    starts_with("t0_"),
    starts_with("t1_"),
    starts_with("t2_"),
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
  relocate("t0_not_lost_following_wave", .before = starts_with("t1_")) |>
  relocate(all_of(t1_name_exposure_binary), .before = starts_with("t2_")) |>
  droplevels()

# save final data
margot::here_save(df_grf, "df_grf")

# check final dataset
#check
colnames(df_grf)

# visualise missing
naniar::vis_miss(df_grf, warn_large_data = FALSE)

# check weights
df_grf$t1_adjusted_weights

#checks
colnames(df_grf)
str(df_grf)

# check exposures
table(df_grf[[t1_name_exposure_binary]])

# check
hist(df_grf$t1_adjusted_weights)

# calculate summary statistics
t0_weight_summary <- summary(df_wide_encoded)

# check
glimpse(df_grf$t0_adjusted_weights)

# visualise weight distributions
hist(df_grf$t0_adjusted_weights, main = "t0_stabalised weights", xlab = "Weight")

# visualise and check missing values
naniar::vis_miss(df_grf, warn_large_data = FALSE)
naniar::gg_miss_var(df_grf)
naniar::miss_var_summary(dat_long_prepare) |> print(n = 100)

# check n
n_observed_grf <- nrow(df_grf)

# view
n_observed_grf

# save
margot::here_save(n_observed_grf, "n_observed_grf")


# inspect propensity scores -----------------------------------------------
# get data # read in if needed
# df_grf <- here_read('df_grf')

# assign weights var name
weights_var_name = "t0_adjusted_weights"

# baseline covariates  # E already exists and is defined
E

# must be a data frame, no NA in exposure

# df_grf is a data frame - we must process this data frame in several steps
# user to specify which columns are outcomes, default to 'starts_with("t2_")'
df_propensity_org <- df_grf |> select(!starts_with("t2_"))

# Remove NAs and print message that this has been done
df_propensity <- df_propensity_org |> drop_na() |> droplevels()

# first run model for baseline propensity if this is selected.  The default should be to not select it.
propensity_model_and_plots <- margot_propensity_model_and_plots(
  df_propensity = df_propensity,
  exposure_variable = t1_name_exposure_binary,
  baseline_vars = E,
  weights_var_name = weights_var_name,
  estimand = "ATE",
  method = "ebal",
  focal = NULL
)

# visualise
summary(propensity_model_and_plots$match_propensity)

# key plot
propensity_model_and_plots$love_plot

# other diagnostic plots
propensity_model_and_plots$summary_plot
propensity_model_and_plots$balance_table
propensity_model_and_plots$diagnostics


# check size
size_bytes <- object.size(propensity_model_and_plots)
print(size_bytes, units = "auto") # Mb

# use qs to save only if you have space
here_save_qs(propensity_model_and_plots,
             "propensity_model_and_plots",
             push_mods)

