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
# essential libraries ---------------------------------------------------------
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
}

if (packageVersion("margot") < "1.0.140") {
  stop("please install margot >= 1.0.140 for this workflow\n
       run: devtools::install_github(\"go-bayes/margot\")
")
}
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
  table1,           # tables
  cli,
  boilerplate
)

library(margot)
# save paths -------------------------------------------------------------------
push_mods <- here::here("examples") 

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

# check is this the exposure variable that you want? 
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

# save
margot::here_save(df_wide, "df_wide")

#df_wide <- margot::here_read("df_wide")
naniar::vis_miss(df_wide, warn_large_data = FALSE)

# view
glimpse(df_wide)


# order data with missingness assigned to work with grf and lmtp
# if any outcome is censored all are censored
# create version for model reports

# check
colnames(df_wide)

# made data wide in correct format
# ** ignore warning *** 
# devtools::load_all("/Users/joseph/GIT/margot/")

df_wide_encoded  <- margot_process_longitudinal_data_wider(
  df_wide,
  ordinal_columns = ordinal_columns,
  continuous_columns_keep = continuous_columns_keep,
  not_lost_in_following_wave = "not_lost_following_wave",
  lost_in_following_wave = "lost_following_wave",
  remove_selected_columns = TRUE,
  exposure_var = exposure_var,
  scale_continuous = TRUE 
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
glimpse(df_wide_encoded)

#naniar::vis_miss(df_wide_encoded, warn_large_data = FALSE)
naniar::gg_miss_var(df_wide_encoded)


#save data
here_save(df_wide_encoded, "df_wide_encoded")

# new weights approach ---------------------------------------------------------


# panel attrition workflow using grf (two-stage IPCW + design weights)
# -----------------------------------------------------------------------------
# builds weights in two stages:
#   w0 : baseline -> t1  (baseline covariates)
#   w1 : t1 survivors -> t2  (baseline + time-1 exposure)
# final weight = t0_sample_weights × w0 × w1, then trimmed & normalised.
# -----------------------------------------------------------------------------

# ── 0 setup ───────────────────────────────────────────────────────────────────

library(tidyverse)        # wrangling
library(glue)             # strings
library(grf)              # forests
library(cli)              # progress

set.seed(123)

# -----------------------------------------------------------------------------
# 1 import full, unfiltered baseline file
# -----------------------------------------------------------------------------

df <- margot::here_read("df_wide_encoded")
cli::cli_alert_info(glue("{nrow(df)} rows × {ncol(df)} columns loaded"))

# -----------------------------------------------------------------------------
# 2 stage‑0 censoring: dropout between t0 → t1
# -----------------------------------------------------------------------------

baseline_covars <- df %>%
  select(starts_with("t0_"), -ends_with("_lost"), -ends_with("lost_following_wave"), -ends_with("_weights")) %>%
  colnames() %>% sort()

X0 <- as.matrix(df[, baseline_covars])
D0 <- factor(df$t0_lost_following_wave, levels = c(0, 1))   # 0 = stayed, 1 = lost

cli::cli_h1("stage 0: probability forest for baseline dropout …")

pf0 <- probability_forest(X0, D0)
P0  <- predict(pf0, X0)$pred[, 2]               # P(dropout by t1)
w0  <- ifelse(D0 == 1, 0, 1 / (1 - P0))         # IPCW for stage 0
df$w0 <- w0

# -----------------------------------------------------------------------------
# 3 stage‑1 censoring: dropout between t1 → t2 (baseline + exposure)
# -----------------------------------------------------------------------------

exposure_var <- t1_name_exposure_binary       # ← edit if needed

df1 <- df %>% filter(t0_lost_following_wave == 0)

# remove rows with missing exposure for stage‑1 model
cen1_data <- df1 %>% filter(!is.na(.data[[exposure_var]]))

X1 <- as.matrix(cbind(
  cen1_data[, baseline_covars],
  cen1_data[[exposure_var]]
))
colnames(X1)[ncol(X1)] <- exposure_var

D1 <- factor(cen1_data$t1_lost_following_wave, levels = c(0, 1))

cli::cli_h1("stage 1: probability forest for second‑wave dropout …")

pf1 <- probability_forest(X1, D1)               # 1 = lost before t2
P1  <- predict(pf1, X1)$pred[, 2]
w1  <- ifelse(D1 == 1, 0, 1 / (1 - P1))

# map w1 back to df1 (rows with NA exposure get weight 0)
df1$w1 <- 0
df1$w1[match(cen1_data$id, df1$id)] <- w1

# -----------------------------------------------------------------------------
# 4 combine design × IPCW weights
# -----------------------------------------------------------------------------

# bring forward w0 for the matching rows (safe join)
w0_vec <- df$w0[match(df1$id, df$id)]

# combined weight before trim / normalise
raw_w <- df1$t0_sample_weights * w0_vec * df1$w1

df1$raw_weight <- raw_w

# trim + normalise (exclude NA & zeros)
pos <- raw_w[!is.na(raw_w) & raw_w > 0]

lb  <- quantile(pos, 0.00, na.rm = TRUE)
ub  <- quantile(pos, 0.99, na.rm = TRUE)

trimmed <- pmin(pmax(raw_w, lb), ub)
normalised <- trimmed / mean(trimmed, na.rm = TRUE)

df1$combo_weights <- normalised <- trimmed / mean(trimmed)

df1$combo_weights <- normalised

hist(df1$combo_weights[df1$t1_lost_following_wave == 0],
     main = "combined weights (observed)", xlab = "weight")

# -----------------------------------------------------------------------------
# 5 analysis set: observed through t2 (not censored at either stage)
# -----------------------------------------------------------------------------

df_analysis <- df1 %>%
  filter(t1_lost_following_wave == 0) %>%
  droplevels()

margot::here_save(df_analysis, "df_analysis_weighted_two_stage")

cli::cli_alert_success(glue("analysis sample: {nrow(df_analysis)} obs"))

# TEST DO NOT UNCOMMENT
# -----------------------------------------------------------------------------
# 6 causal forest (edit outcome var if needed)
# -----------------------------------------------------------------------------
# 
# outcome_var <- "t2_kessler_latent_depression_z"   # ← edit
# 
# Y <- df_analysis[[outcome_var]]
# W <- df_analysis[[exposure_var]]
# X <- as.matrix(df_analysis[, baseline_covars])
# 
# cf <- causal_forest(
#   X, Y, W,
#   sample.weights = df_analysis$combo_weights,
#   num.trees      = 2000
# )
# 
# print(average_treatment_effect(cf))
# margot::here_save(cf,          "cf_ipcw_two_stage")
# -----------------------------------------------------------------------------
# 7 save objects
# -----------------------------------------------------------------------------


cli::cli_h1("two-stage IPCW workflow complete ✔")


# # maintain workflow 
E <- baseline_covars
here_save(E, "E")
length(E)
colnames(df_analysis)


cli::cli_h1("naming convention matcheds `grf` ✔")


# arrange
df_grf <- df_analysis |>
  relocate(ends_with("_weights"), .before = starts_with("t0_")) |>
  relocate(ends_with("_weight"), .before = ends_with("_weights")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
  relocate("t0_not_lost_following_wave", .before = starts_with("t1_")) |>
  relocate(all_of(t1_name_exposure_binary), .before = starts_with("t2_")) |>
  droplevels()

colnames(df_grf)


# +--------------------------+
# |          ALERT           |
# +--------------------------+
# make sure to do this
# save final data
margot::here_save(df_grf, "df_grf")

cli::cli_h1("saved data `df_grf` for models ✔")


# +--------------------------+
# |        END ALERT         |
# +--------------------------+


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

# checks
colnames(df_grf)
str(df_grf)

# check exposures
table(df_grf[[t1_name_exposure_binary]])

# check
hist(df_grf$combo_weights)

df_grf$combo_weights


# calculate summary statistics
t0_weight_summary <- summary(df_wide_encoded)

# check
glimpse(df_grf$combo_weights)

# visualise weight distributions
hist(df_grf$combo_weights, main = "t0_stabalised weights", xlab = "Weight")

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
