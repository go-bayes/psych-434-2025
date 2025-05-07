# script 2: causal workflow for estimating average treatment effects using margot
# may 2025
# questions: joseph.bulbulia@vuw.ac.nz

# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+

# restart fresh session for a clean workspace
rstudioapi::restartSession()

# set seed for reproducibility
set.seed(123)

# libraries ---------------------------------------------------------------
if (!requireNamespace("margot", quietly = TRUE)) {
  message("installing 'margot' from GitHub")
  devtools::install_github("go-bayes/margot", upgrade = "never")
}
library(margot)

# install and load other packages from CRAN if missing
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# install and load other packages from CRAN if missing
if (!requireNamespace("grf", quietly = TRUE)) {
  install.packages("grf")
}
library(grf)

# load packages -------------------------------------------------------------
# pacman will install missing packages automatically
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,       # data wrangling + plotting
  qs,              # fast data i/o
  here,            # project-relative file paths
  data.table,      # fast data manipulation
  fastDummies,     # dummy variable creation
  naniar,          # missing data handling
  skimr,           # summary statistics
  grf,             # machine learning forests
  kableExtra,      # tables
  ggplot2,         # graphs
  doParallel,      # parallel processing
  grf,             # causal forests
  janitor,         # variables names
  stringr,         # variable names
  patchwork,       # graphs
  table1           # tables
)

# save paths -------------------------------------------------------------------
push_mods <- here::here("save_directory") 

# read data
dat_long_final <- margot::here_read("dat_long_final")

# read baseline sample weights
t0_sample_weights <- margot::here_read("t0_sample_weights")

# read exposure
name_exposure <- margot::here_read("name_exposure")
name_exposure_binary = paste0(name_exposure, "_binary")
name_exposure_continuous = name_exposure

# read variables
baseline_vars <- margot::here_read("baseline_vars")
exposure_var <- margot::here_read("exposure_var")
outcome_vars <- margot::here_read("outcome_vars")
baseline_wave <- margot::here_read("baseline_wave")
exposure_waves <- margot::here_read("exposure_waves")
outcome_wave <- margot::here_read("outcome_wave")

# define continuous columns to keep
continuous_columns_keep <- c("t0_sample_weights")

# define ordinal columns that we will expand into binary variables
ordinal_columns <- c("t0_education_level_coarsen",
                     "t0_eth_cat",
                     "t0_rural_gch_2018_l")



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
outcome_vars <- here_read("outcome_vars")
t2_outcome_z <- paste0("t2_", outcome_vars, "_z")

# view
t2_outcome_z

# check
str(dat_long_final)

# check
naniar::gg_miss_var(dat_long_final)

# impute data --------------------------------------------------------------
# ordinal use
ordinal_columns <- c(
  "t0_education_level_coarsen",
  "t0_eth_cat",
  "t0_rural_gch_2018_l",
  "t0_gen_cohort"
)

# define cols we will not standardise
continuous_columns_keep <- c("t0_sample_weights")

# remove sample weights
dat_long_final_2 <- dat_long_final |> select(-sample_weights)

# prepare data for analysis ----------------------
dat_long_final_2 <- margot::remove_numeric_attributes(dat_long_final_2)
# wide data
df_wide <- margot_wide_machine(
  dat_long_final,
  id = "id",
  wave = "wave",
  baseline_vars,
  exposure_var = exposure_var,
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

# return sample weights
df_wide$t0_sample_weights <-  t0_sample_weights

#df_wide <- margot::here_read("df_wide")
naniar::vis_miss(df_wide, warn_large_data = FALSE)

# order data with missingness assigned to work with grf and lmtp
# if any outcome is censored all are censored
# create version for model reports

# check
colnames(df_wide)


# made data wide in correct format
# ** ignore warning *** 
df_wide_encoded  <- margot::margot_process_longitudinal_data_wider(
  df_wide,
  ordinal_columns = ordinal_columns,
  continuous_columns_keep = continuous_columns_keep,
  not_lost_in_following_wave = "not_lost_following_wave",
  lost_in_following_wave = "lost_following_wave",
  remove_selected_columns = TRUE,
  exposure_var = exposure_var,
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

# view
head(df_wide_encoded)

#naniar::vis_miss(df_wide_encoded, warn_large_data = FALSE)
naniar::gg_miss_var(df_wide_encoded)


# predict attrition and create censoring weights --------------------------
# step 1: prepare baseline covariates
# select all t0_ variables except the exposure binary and any _lost indicators, then sort their names
t0_var_names <- df_wide_encoded |>
  select(-all_of(t0_name_exposure_binary)) |>
  select(starts_with("t0_"),-ends_with("_lost"),-ends_with("lost_following_wave"), -ends_with("_weights")) |>
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
# make final missing data graph
missing_final_data_plot <- naniar::vis_miss(df_grf, warn_large_data = FALSE)
missing_final_data_plot

# save plot
margot_save_png(missing_final_data_plot, prefix = "missing_final_data")

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

# check n
n_observed_grf <- nrow(df_grf)

# view
n_observed_grf

# save
margot::here_save(n_observed_grf, "n_observed_grf")



# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+


# +--------------------------+
# |     END                  |
# +--------------------------+

# this is just for your interest ------------------------------------------
# not used in final manuscript
# FOR INTEREESTS
# inspect propensity scores -----------------------------------------------
# get data
# df_grf <- here_read('df_grf')
# 
# # assign weights var name
# weights_var_name = "t0_adjusted_weights"
# 
# # baseline covariates  # E already exists and is defined
# E
# 
# # must be a data frame, no NA in exposure
# 
# # df_grf is a data frame - we must process this data frame in several steps
# # user to specify which columns are outcomes, default to 'starts_with("t2_")'
# df_propensity_org <- df_grf |> select(!starts_with("t2_"))
# 
# # Remove NAs and print message that this has been done
# df_propensity <- df_propensity_org |> drop_na() |> droplevels()
# 
# # E_propensity_names
# # first run model for baseline propensity if this is selected.  The default should be to not select it.
# propensity_model_and_plots <- margot_propensity_model_and_plots(
#   df_propensity = df_propensity,
#   exposure_variable = t1_name_exposure_binary,
#   baseline_vars = E,
#   weights_var_name = weights_var_name,
#   estimand = "ATE",
#   method = "ebal",
#   focal = NULL
# )
# 
# # visualise
# summary(propensity_model_and_plots$match_propensity)
# 
# # key plot
# propensity_model_and_plots$love_plot
# 
# # other plots
# propensity_model_and_plots$summary_plot
# propensity_model_and_plots$balance_table
# propensity_model_and_plots$diagnostics
# 
# 
# # check size
# size_bytes <- object.size(propensity_model_and_plots)
# print(size_bytes, units = "auto") # Mb
# 
# # use qs to save only if you have space
# here_save_qs(propensity_model_and_plots,
#              "propensity_model_and_plots",
#              push_mods)
