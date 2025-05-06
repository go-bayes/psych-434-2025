# script 2: causal workflow for estimating average treatment effects using margot
# may 2025
# questions: joseph.bulbulia@vuw.ac.nz

# restart fresh session for a clean workspace
rstudioapi::restartSession()

# set seed for reproducibility
set.seed(123)


# save paths -------------------------------------------------------------------
push_mods <- here::here("models_example_2")


# packages needed ---------------------------------------------------------

# load libraries ----------------------------------------------------------
pacman::p_load(
  DiagrammeR,   # graph and network visualization
  doParallel,   # parallel processing with foreach
  fastDummies,  # fast creation of dummy variables
  fs,           # cross-platform file system operations
  ggplot2,      # data visualisation
  grf,          # generalized random forests
  here,         # simple and robust file referencing
  janitor,      # data cleaning and validation
  kableExtra,   # advanced table formatting
  # margot,       # functions for casual inference
  naniar,       # handling and visualization of missing data
  parameters,   # parameters and performance metrics
  policytree,   # causal inference with policy trees
  progressr,    # progress reporting for R
  tidyverse,    # collection of R packages for data science
  EValue,       # compute Evalues
  data.table,   # fast data wrangling
  maq,          # qini curves
  purrr,        # data wrangling
  patchwork,     # multiple plots
  labelled,
  cli,
  rlang
)

# read variables
baseline_vars <- margot::here_read("baseline_vars")
exposure_var <- margot::here_read("exposure_var")
outcome_vars <- margot::here_read("outcome_vars")
t0_sample_weights <- margot::here_read("t0_sample_weights")
baseline_wave <- margot::here_read("baseline_wave")
exposure_waves <- margot::here_read("exposure_waves")
outcome_wave <- margot::here_read("outcome_wave")

# define continuous columns to keep
continuous_columns_keep <- c("t0_sample_weights")

# define ordinal columns that we will expand into binary variables
ordinal_columns <- c("t0_education_level_coarsen",
                     "t0_eth_cat",
                     "t0_rural_gch_2018_l")

# read data
dat_long_prepare <- margot::here_read("dat_long_prepare")

# read exposure
name_exposure <- margot::here_read("name_exposure")
name_exposure_binary = paste0(name_exposure, "_binary")
name_exposure_continuous = name_exposure

#check
name_exposure_binary
name_exposure_continuous

# define wide variable names
t0_name_exposure_binary <- paste0("t0_", name_exposure_binary)
t0_name_exposure_binary


# make exposure names (continuous not genreally used)
t1_name_exposure_binary <- paste0("t1_", name_exposure_binary)
t1_name_exposure_binary

# treatments (continuous verion)
t0_name_exposure <- paste0("t0_", name_exposure_continuous)
t1_name_exposure <- paste0("t1_", name_exposure_continuous)
t0_name_exposure_continuous <- paste0("t0_", name_exposure)
t1_name_exposure_continuous <- paste0("t1_", name_exposure)

# raw outcomes
# read health outcomes
raw_outcomes_health <- here_read("raw_outcomes_health")
t2_outcome_health_z <- paste0("t2_", raw_outcomes_health, "_z")
t2_outcome_health_z <- sort(t2_outcome_health_z)
t2_outcome_health_z

# read raw outcomes
raw_outcomes_psych <- here_read("raw_outcomes_psych")
t2_outcome_psych_z <- paste0("t2_", raw_outcomes_psych, "_z")
t2_outcome_psych_z <- sort(t2_outcome_psych_z)
t2_outcome_psych_z

# read raw outcomes
raw_outcomes_present <- here_read("raw_outcomes_present")
t2_outcome_present_z <- paste0("t2_", raw_outcomes_present, "_z")
t2_outcome_present <- sort(t2_outcome_present_z)
t2_outcome_present_z

# read raw outcomes
raw_outcomes_life <- here_read("raw_outcomes_life")
t2_outcome_life_z <- paste0("t2_", raw_outcomes_life, "_z")
t2_outcome_life_z <- sort(t2_outcome_life_z)
t2_outcome_life_z

# read raw outcomes
raw_outcomes_social <- here_read("raw_outcomes_social")
t2_outcome_social_z <- paste0("t2_", raw_outcomes_social, "_z")
t2_outcome_social_z <- sort(t2_outcome_social_z)
t2_outcome_social_z


# time-varying confounders * ONLY IF USED **
confounder_vars <- here_read("confounder_vars")
# ensure unique
confounder_vars <- unique(confounder_vars)
# check
confounder_vars

# check
str(dat_long_prepare)

# check
naniar::gg_miss_var(dat_long_prepare)

# impute data --------------------------------------------------------------

# get vars
baseline_vars <- margot::here_read("baseline_vars")
outcome_vars <- margot::here_read("outcome_vars")

# make both
name_exposure_both <- c(name_exposure_binary, name_exposure_continuous)

#check
name_exposure_both

# for predictive models for censoring/ use continuous variable for better


# ordinal use
ordinal_columns <- c(
  "t0_education_level_coarsen",
  "t0_eth_cat",
  "t0_rural_gch_2018_l",
  "t0_gen_cohort"
)

# for
continuous_columns_keep <- c("t0_sample_weights")

# prepare data for analysis ----------------------
dat_long_prepare <- margot::remove_numeric_attributes(dat_long_prepare)
name_exposure_both

# wide data
df_wide <- margot_wide_machine(
  dat_long_prepare,
  id = "id",
  wave = "wave",
  baseline_vars,
  exposure_var = name_exposure_both,
  outcome_vars,
  confounder_vars = NULL,
  imputation_method = "none",
  include_exposure_var_baseline = TRUE,
  include_outcome_vars_baseline = TRUE,
  extend_baseline = FALSE,
  include_na_indicators = FALSE
)

# check
colnames(df_wide)

#
head(df_wide)

# add weights back to data
df_wide$t0_sample_weights <- t0_sample_weights

# make sure that rural is a factor
df_wide$t0_rural_gch_2018_l <- as.factor(df_wide$t0_rural_gch_2018_l)

# save the wide data
margot::here_save(df_wide, "df_wide")

#df_wide <- margot::here_read("df_wide")
naniar::vis_miss(df_wide, warn_large_data = FALSE)

# order data with missingness assigned to work with grf and lmtp
# if any outcome is censored all are censored
# create version for model reports

# check
colnames(df_wide)


# made data wide in correct format
# ignore warning
df_wide_encoded  <- margot::margot_process_longitudinal_data_wider(
  df_wide,
  ordinal_columns = ordinal_columns,
  continuous_columns_keep = continuous_columns_keep,
  not_lost_in_following_wave = "not_lost_following_wave",
  lost_in_following_wave = "lost_following_wave",
  remove_selected_columns = TRUE,
  exposure_var = name_exposure_both,
  scale_continuous = TRUE,
  censored_if_any_lost = FALSE
)

# check
colnames(df_wide_encoded)

# check
table(df_wide_encoded$t0_not_lost_following_wave)

# make the binary variable numeric
df_wide_encoded[[t0_name_exposure_binary]] <-
  as.numeric(df_wide_encoded[[t0_name_exposure_binary]]) - 1
df_wide_encoded[[t1_name_exposure_binary]] <-
  as.numeric(df_wide_encoded[[t1_name_exposure_binary]]) - 1

# view
df_wide_encoded[[t0_name_exposure_binary]]
df_wide_encoded[[t1_name_exposure_binary]]

# 1. ensure both binaries only take values 0 or 1 (ignore NA)
stopifnot(all(df_wide_encoded[[t0_name_exposure_binary]][!is.na(df_wide_encoded[[t0_name_exposure_binary]])] %in% 0:1),
          all(df_wide_encoded[[t1_name_exposure_binary]][!is.na(df_wide_encoded[[t1_name_exposure_binary]])] %in% 0:1))

# 2. ensure NA‐patterns match between t1_exposure and t0_lost flag
# count n-as in t1 exposure
n_na_t1 <- sum(is.na(df_wide_encoded[[t1_name_exposure_binary]]))

# count how many were lost at t0
n_lost_t0 <- sum(df_wide_encoded$t0_lost_following_wave == 1, na.rm = TRUE)

# print them for inspection
message("NAs in ", t1_name_exposure_binary, ": ", n_na_t1)
message("t0_lost_following_wave == 1: ", n_lost_t0)

# stop if they don’t match
stopifnot(n_na_t1 == n_lost_t0)

# 3. ensure if t1 is non‐NA then subject was not lost at t0
stopifnot(all(is.na(df_wide_encoded[[t1_name_exposure_binary]]) |
                df_wide_encoded[["t0_not_lost_following_wave"]] == 1))

# now it’s safe to save
here_save_qs(df_wide_encoded, "df_wide_encoded", push_mods)
df_wide_encoded <- here_read_qs("df_wide_encoded", push_mods)

# view
head(df_wide_encoded)

#naniar::vis_miss(df_wide_encoded, warn_large_data = FALSE)
naniar::gg_miss_var(df_wide_encoded)


# predict attrition and create censoring weights --------------------------
# step 1: prepare baseline covariates
# select all t0_ variables except the exposure binary and any _lost indicators, then sort their names
t0_var_names <- df_wide_encoded |>
  select(-all_of(t0_name_exposure_binary)) |>
  select(starts_with("t0_"),-ends_with("_lost"),-ends_with("lost_following_wave")) |>
  colnames() |>
  sort()

# get unique values (to be safe)
E <- unique(t0_var_names)

# view
print(E)

# save baseline covariates
margot::here_save(E, "E")

# view
print(E)

# step 2: calculate weights for t0
D_0 <- as.factor(df_wide_encoded$t0_lost_following_wave)

# get co-variates
cen_0 <- df_wide_encoded[, E]

# probability forest for censoring
# this will take time
cen_forest_0 <- probability_forest(cen_0, D_0)

# get predictions
predictions_grf_0 <- predict(cen_forest_0, newdata = cen_0, type = "response")

# get propensity scores
pscore_0 <- predictions_grf_0$pred[, 2]

# use margot_adjust_weights for t0
t0_weights <- margot_adjust_weights(
  pscore = pscore_0,
  trim = TRUE,
  normalize = TRUE,
  # lower trimming
  lower_percentile = 0.00,
  # upper trimming
  upper_percentile = 0.99,
  censoring_indicator = df_wide_encoded$t0_lost_following_wave,
  sample_weights = df_wide_encoded$t0_sample_weights
)

# view
hist(t0_weights$adjusted_weights)

# give weights
df_wide_encoded$t0_adjusted_weights <- t0_weights$adjusted_weights

#check
naniar::vis_miss(df_wide_encoded, warn_large_data = FALSE)

# remove lost next wave (censored)
df_wide_encoded_1 <- df_wide_encoded %>%
  filter(t0_lost_following_wave == 0) %>%
  droplevels()

# step 4: calculate weights for t1
E_and_exposure <- c(E, t1_name_exposure_continuous)
D_1 <- as.factor(df_wide_encoded_1$t1_lost_following_wave)
cen_1 <- df_wide_encoded_1[, E_and_exposure]

# probability forest for censoring
#  *** this will take time ***
cen_forest_1 <- probability_forest(cen_1, D_1, sample.weights = df_wide_encoded_1$t0_adjusted_weights)

# predict forest
predictions_grf_1 <- predict(cen_forest_1, newdata = cen_1, type = "response")

# get propensity score
pscore_1 <- predictions_grf_1$pred[, 2]

# check
hist(pscore_1)

# use margot_adjust_weights for t1
t1_weights <- margot_adjust_weights(
  pscore = pscore_1,
  trim = TRUE,
  normalize = TRUE,
  lower_percentile = 0.00,
  # upper trimming
  upper_percentile = 0.99,
  censoring_indicator = df_wide_encoded_1$t1_lost_following_wave,
  sample_weights = df_wide_encoded_1$t0_adjusted_weights # combine with weights
)

# add weights -- these will be the weights we use
df_wide_encoded_1$t1_adjusted_weights <- t1_weights$adjusted_weights

#check
naniar::vis_miss(df_wide_encoded_1, warn_large_data = FALSE)

# save
here_save(df_wide_encoded_1, "df_wide_encoded_1")

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
df_grf <- margot::here_read("df_grf")

# check final dataset
colnames(df_grf)

# visualise missing
# should have no missing in t1 and t2 variables
# handled by IPCW
naniar::vis_miss(df_grf, warn_large_data = FALSE)

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
glimpse(df_grf$t1_adjusted_weights)

# visualise weight distributions
hist(df_grf$t1_adjusted_weights, main = "t0_stabalised weights", xlab = "Weight")

# visualise and check missing values
naniar::gg_miss_var(df_grf)

# check n
n_observed_grf <- nrow(df_grf)

# view
n_observed_grf

# save
margot::here_save(n_observed_grf, "n_observed_grf")



# this is just for your interest ------------------------------------------
# not used in final manuscript


# inspect propensity scores -----------------------------------------------
# get data
df_grf <- here_read('df_grf')

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

# E_propensity_names
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

# other plots
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
