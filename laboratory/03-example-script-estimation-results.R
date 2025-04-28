# 03-estimating-models-grf
# joseph.bulbulia@vuw.ac.nz
# april 2025
# devtools::load_all("/Users/joseph/GIT/margot/") # use version >= 1.0.21 
# for information on causal forests see: 
# https://grf-labs.github.io/grf/

# ** NOTE THESE DATA ARE SIMULATED SO RESULTS ARE NOT INTERPRETABLE **

# restart fresh session
#rstudioapi::restartSession()

# set reproducibility
set.seed(123)


# save paths -------------------------------------------------------------------
# create a save path to your on computer
# this is mine
# push_mods <- here::here('/Users/joseph/v-project\ Dropbox/data/courses/25-psych-434')
# yours might be (after creating a data file)

# data path
push_mods <- here::here("data")


# load libraries ---------------------------------------------------------
# install and load 'margot' package
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
}

# load margot library
library(margot)

# load necessary libraries
pacman::p_load(
  clarify,      # sensitivity analysis for causal inference
  cobalt,       # covariate balance tables and plots
  DiagrammeR,   # graph and network visualisation
  doParallel,   # parallel processing with foreach
  fastDummies,  # fast creation of dummy variables
  fs,           # cross-platform file system operations
  ggplot2,      # data visualisation
  glmnet,       # lasso and elastic-net regularised models
  grf,          # generalised random forests
  gt,           # html tables for data frames
  gtsummary,    # summary tables for regression models
  here,         # simple and robust file referencing
  janitor,      # data cleaning and validation
  kableExtra,   # advanced table formatting
  lmtp,         # longitudinal targeted maximum likelihood estimation
  margot,       # functions for casual inference
  MatchIt,      # matching methods for causal inference
  MatchThem,    # matching methods for multiply imputed datasets
  naniar,       # handling and visualisation of missing data
  parameters,   # parameters and performance metrics
  policytree,   # causal inference with policy trees
  progressr,    # progress reporting for R
  ranger,       # fast implementation of random forests
  skimr,        # summary statistics for data frames
  SuperLearner, # ensemble learning
  tidyverse,    # collection of R packages for data science
  WeightIt,     # weighting methods for covariate balancing
  xgboost,      # extreme gradient boosting
  EValue,       # compute Evalues
  data.table,   # fast data wrangling
  maq,          # qini curves
  purrr,        # data wrangling
  patchwork,    # multiple plots
  labelled,     # allows working with labelled data (variable and value labels)
  cli,          # tools for building command line interfaces
  crayon,       # colored terminal output
  rlang,        # tools for programming with R
  kableExtra    # enhance kable output with additional formatting
)


# variable names and labels -----------------------------------------------
# import exposure variable (binary) 
name_exposure <- margot::here_read("name_exposure")
name_exposure

# make exposure names
t1_name_exposure_binary <- paste0("t1_", name_exposure, "_binary")

# check exposure name
t1_name_exposure_binary

# read health outcomes 
raw_outcomes_health <- here_read("raw_outcomes_health")
t2_outcome_health_z <- paste0("t2_", raw_outcomes_health, "_z")
t2_outcome_health_z <- sort(t2_outcome_health_z)

# read raw outcomes 
raw_outcomes_psych <- here_read("raw_outcomes_psych")
t2_outcome_psych_z <- paste0("t2_", raw_outcomes_psych, "_z")
t2_outcome_psych_z <- sort(t2_outcome_psych_z)

# read raw outcomes
raw_outcomes_present <- here_read("raw_outcomes_present")
t2_outcome_present_z <- paste0("t2_", raw_outcomes_present, "_z")
t2_outcome_present_z <- sort(t2_outcome_present_z)

# read raw outcomes
raw_outcomes_life <- here_read("raw_outcomes_life")
t2_outcome_life_z <- paste0("t2_", raw_outcomes_life, "_z")
t2_outcome_life_z <- sort(t2_outcome_life_z)

# read raw outcomes
raw_outcomes_social <- here_read("raw_outcomes_social")
t2_outcome_social_z <- paste0("t2_", raw_outcomes_social, "_z")
t2_outcome_social_z <- sort(t2_outcome_social_z)

# combine all raww outcome names
raw_outcomes_all <- c(raw_outcomes_health, 
                      raw_outcomes_psych,
                      raw_outcomes_present, 
                      raw_outcomes_life, 
                      raw_outcomes_social)

# save all outcomes
here_save(raw_outcomes_all, "raw_outcomes_all")


# label mappings for health outcomes
label_mapping_health <- list(
  "t2_alcohol_frequency" = "Alcohol Frequency",
  "t2_alcohol_intensity" = "Alcohol Intensity",
  "t2_hlth_bmi_z" = "BMI", 
  "t2_hlth_sleep_hours_z" = "Sleep", 
  "t2_log_hours_exercise_z" = "Hours of Exercise (log)",
  "t2_short_form_health_z" = "Short Form Health" 
)

# label mappings for psychological well-being outcomes
label_mapping_psych <- list(
  "t2_hlth_fatigue_z" = "Fatigue", 
  "t2_kessler_latent_anxiety_z" = "Anxiety", 
  "t2_kessler_latent_depression_z" = "Depression",  
  "t2_rumination_z" = "Rumination"
)

# label mappings for present reflective outcomes
label_mapping_present <- list(
  "t2_bodysat_z" = "Body Satisfaction",
  "t2_foregiveness_z" = "Forgiveness",  
  "t2_perfectionism_z" = "Perfectionism",  
  "t2_self_control_z" = "Self Control",  
  "t2_sexual_satisfaction_z" = "Sexual Satisfaction"
)

# label mappings for life reflective outcomes
label_mapping_life <- list(
  "t2_gratitude_z" = "Gratitude", 
  "t2_lifesat_z" = "Life Satisfaction", 
  "t2_meaning_purpose_z" = "Meaning: Purpose",  
  "t2_meaning_sense_z" = "Meaning: Sense", 
  "t2_pwi_z" = "Personal Well-being Index"
)

# label mappings for social outcomes
label_mapping_social <- list(
  "t2_belong_z" = "Social Belonging",
  "t2_neighbourhood_community_z" = "Neighbourhood Community", 
  "t2_support_z" = "Social Support" 
)

# n participants (can be useful for graphs)
n_participants <- here_read("n_participants")

# label mapping all -------------------------------------------------------
label_mapping_all <- c(
  label_mapping_health,
  label_mapping_psych,
  label_mapping_present,
  label_mapping_life,
  label_mapping_social
)


# save label mappings -----------------------------------------------------
here_save(label_mapping_health, "label_mapping_health")
here_save(label_mapping_psych, "label_mapping_psych")
here_save(label_mapping_present, "label_mapping_present")
here_save(label_mapping_life, "label_mapping_life")
here_save(label_mapping_social, "label_mapping_social")
here_save(label_mapping_all, "label_mapping_all")

# start analysis here ----------------------------------------------------
# import data
df_grf <- margot::here_read('df_grf')

# check
colnames(df_grf)

# read original dataframe / used to get measures on data scale
original_df <- margot::here_read("df_wide", push_mods)

# check missing values (baseline missingness is handled by grf)
# takes a long time to render so commented out
# naniar::vis_miss(df_grf, warn_large_data = FALSE)

# check another way
naniar::gg_miss_var(df_grf)

# import names of baseline covariates
E <- margot::here_read("E")
# check
E

# exposure variable (we use the GRF convention where W is the exposure)
# *****SET ***************
# make binary

# *********************************
# check that variables are 0 or 1
df_grf[[t1_name_exposure_binary]]
# *********************************

# needs to be a matrix
W <- as.vector(df_grf[[t1_name_exposure_binary]])

# check that these values are 0 or 1
W

# set weights
weights <- df_grf$t1_adjusted_weights 

# view/ check none too extreme
hist(weights)

# remove attributes of baseline co-variaties
X <-  margot::remove_numeric_attributes(df_grf[E]) 

# set parameters ----------------------------------------------------------
# set model defaults -----------------------------------------------------
grf_defaults <- list(
  seed = 123, # reproduce results
  stabilize.splits = TRUE, # robustness
  # min.node.size = 5,  # default is five/ requires at least 5 observed in control and treated
  # set higher for greater smoothing
  num.trees = 2000 # grf default = 2000 # set lower or higher depending on storage
)

# set defaults for graphs (see bottom of script for options)
decision_tree_defaults <- list(
  span_ratio = .3,
  text_size = 3.8,
  y_padding = 0.25  # use full parameter name
  # edge_label_offset = .002, # options
  # border_size = .05
)

# set defaults for graphs (see bottom of script for options)
# set policy tree defaults
policy_tree_defaults <- list(
  point_alpha = .5,
  title_size = 12,
  subtitle_size = 14,
  axis_title_size = 14,
  legend_title_size = 14,
  split_line_color = "red",
  split_line_alpha = 0.8,
  split_label_color = "red"
)

# test --------------------------------------------------------------------
n <- nrow(X) # n in sample

# define training sample
toy <- sample(1:n, n / 4) # get half sample

# test data
toy_data = df_grf[toy, ]

# check
nrow(toy_data)

# test covariates
X_toy = X[toy, ]

# check
str(X_toy)

# test exposure
W_toy = W[toy]

# test weights
weights_toy = weights[toy]

# # test model
# ignore warnings
cf.test <- margot::margot_causal_forest(
  data          = toy_data,
  outcome_vars  = "t2_log_hours_exercise_z",
  covariates    = X_toy,
  W             = W_toy,
  weights       = weights_toy,
  top_n_vars = 15,
  save_models = TRUE # save the models
)

# test plots
models_binary_batch_test <- margot::margot_policy(
  cf.test,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = "model_t2_log_hours_exercise_z",
  original_df = original_df,
  label_mapping = label_mapping_psych
)

# test plots
models_binary_batch_test$model_t2_log_hours_exercise_z$qini_plot
models_binary_batch_test$model_t2_log_hours_exercise_z$decision_tree
models_binary_batch_test$model_t2_log_hours_exercise_z$policy_tree


# run binary model --------------------------------------------------------
# health models -----------------------------------------------------------
models_binary_health <- margot::margot_causal_forest(
  data = df_grf,
  outcome_vars = t2_outcome_health_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  save_models = TRUE,
  train_proportion = 0.7
)

# check size if needed
margot::margot_size(models_binary_health)

# save model
margot::here_save_qs(models_binary_health, "models_binary_health", push_mods)

# psych models ------------------------------------------------------------
models_binary_psych <- margot::margot_causal_forest(
  data = df_grf,
  outcome_vars = t2_outcome_psych_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  save_models = TRUE,
  train_proportion = 0.7
)

# save model
margot::here_save_qs(models_binary_psych, "models_binary_psych", push_mods)


# present models ----------------------------------------------------------
models_binary_present <- margot::margot_causal_forest(
  data = df_grf,
  outcome_vars = t2_outcome_present_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  save_models = TRUE,
  train_proportion = 0.7
)
# save model
margot::here_save_qs(models_binary_present, "models_binary_present", push_mods)


# life models -------------------------------------------------------------
models_binary_life <- margot::margot_causal_forest(
  data = df_grf,
  outcome_vars = t2_outcome_life_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  save_models = TRUE,
  train_proportion = 0.7
)

# save model
margot::here_save_qs(models_binary_life, "models_binary_life", push_mods)


# social models -----------------------------------------------------------
models_binary_social <- margot::margot_causal_forest(
  data = df_grf,
  outcome_vars = t2_outcome_social_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  save_models = TRUE,
  train_proportion = 0.7
)

# save model
margot::here_save_qs(models_binary_social, "models_binary_social", push_mods)

# graphs ------------------------------------------------------------------
# plot defaults -----------------------------------------------------------
title_binary = "Community Socialising (binary)"
filename_prefix = "grf_extraversion_wb"

# titles
subtitle_health = "Health"
subtitle_psych = "Psychological Well-being"
subtitle_present = "Present-Focussed Well-being"
subtitle_life = "Life-Focussed Well-being"
subtitle_social = "Social Well-being"

# defaults
base_defaults_binary <- list(
  type = "RD",
  title = title_binary,
  #interpret_all_E_gt1 = TRUE,
  e_val_bound_threshold = 1.1,
  colors = c(
    "positive" = "#E69F00",
    "not reliable" = "grey50",
    "negative" = "#56B4E9"
  ),
  x_offset = -.275,
  # will be set based on type
  x_lim_lo = -.275,
  # will be set based on type
  x_lim_hi = .275,
  text_size = 4,
  linewidth = 0.5,
  estimate_scale = 1,
  base_size = 18,
  point_size = 2,
  title_size = 19,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  include_coefficients = FALSE
)

# set options for graphs
# health graph options
outcomes_options_health <- margot_plot_create_options(
  title = subtitle_health,
  base_defaults = base_defaults_binary,
  subtitle = "",
  filename_prefix = filename_prefix)

# psych graph options
outcomes_options_psych <- margot_plot_create_options(
  title = subtitle_psych,
  base_defaults = base_defaults_binary,
  subtitle = "",
  filename_prefix = filename_prefix)


# present graph options ---------------------------------------------------
outcomes_options_present <- margot_plot_create_options(
  title = subtitle_present,
  base_defaults = base_defaults_binary,
  subtitle = "",
  filename_prefix = filename_prefix)


# life graph options ------------------------------------------------------
outcomes_options_life <- margot_plot_create_options(
  title = subtitle_life,
  base_defaults = base_defaults_binary,
  subtitle = "",
  filename_prefix = filename_prefix)


# social graph options ----------------------------------------------------
outcomes_options_social <- margot_plot_create_options(
  title = subtitle_social,
  base_defaults = base_defaults_binary,
  subtitle = "",
  filename_prefix = filename_prefix)

# all graph options ------------------------------------------------------
options_all_models <- margot_plot_create_options(
  title = "Outcomewide Wellbeing",
  base_defaults = base_defaults_binary,
  subtitle = "",
  filename_prefix = filename_prefix)


# make graphs -------------------------------------------------------------
# read results
# warning: reading models will take time
# import
models_binary_health <- margot::here_read_qs("models_binary_health", push_mods)
models_binary_psych <- margot::here_read_qs("models_binary_psych", push_mods)
models_binary_present <- margot::here_read_qs("models_binary_present", push_mods)
models_binary_life <- margot::here_read_qs("models_binary_life", push_mods)
models_binary_social <- margot::here_read_qs("models_binary_social", push_mods)

# check size (example)
margot::margot_size(models_binary_health)

# make ATE plots ----------------------------------------------------------
# health plots ------------------------------------------------------------
binary_results_health <- margot_plot(
  models_binary_health$combined_table,
  options = outcomes_options_health,
  label_mapping = label_mapping_health,
  include_coefficients = FALSE,
  save_output = FALSE, 
  order = "evaluebound_asc",
  original_df = original_df,
  e_val_bound_threshold = 1.1
)

# view
binary_results_health$transformed_table |> rename(
  "E-Value" = "E_Value",
  "E-Value bound" = "E_Val_bound"
) |>
  kbl(format = 'markdown')

# check
cat(binary_results_health$interpretation)

# interpretation
cat(binary_results_health$interpretation)

# plot psych
binary_results_psych_asc <- margot_plot(
  models_binary_psych$combined_table,
  options = outcomes_options_psych,
  label_mapping = label_mapping_psych,
  include_coefficients = FALSE,
  save_output = FALSE,
  original_df = original_df,
  e_val_bound_threshold = 1.1,
  order = "evaluebound_asc")


# order
binary_results_psych_asc$plot

# reorder for descriptions
binary_results_psych <- margot_plot(
  models_binary_psych$combined_table,
  options = outcomes_options_psych,
  label_mapping = label_mapping_psych,
  include_coefficients = FALSE,
  save_output = FALSE,
  e_val_bound_threshold = 1.1,
  original_df = original_df,
  order = "evaluebound_asc"
)

# table
binary_results_psych$transformed_table |> rename(
  "E-Value" = "E_Value",
  "E-Value bound" = "E_Val_bound"
) |>
  kbl(format = 'markdown')

# interpretation
cat(binary_results_psych$interpretation)

# plot present
# order
binary_results_present <- margot_plot(
  models_binary_present$combined_table,
  options = outcomes_options_present,
  label_mapping = label_mapping_present,
  include_coefficients = FALSE,
  save_output = FALSE,
  original_df = original_df,
  order = "evaluebound_asc"
)

# plot
binary_results_present$plot

# interpretation
cat(binary_results_present$interpretation)
(binary_results_present$transformed_table)

# plot life
binary_results_life <- margot_plot(
  models_binary_life$combined_table,
  options = outcomes_options_life,
  label_mapping = label_mapping_life,
  include_coefficients = FALSE,
  save_output = FALSE,
  order = "evaluebound_asc",
  original_df = original_df
)

# table
binary_results_life$transformed_table |> rename(
  "E-Value" = "E_Value",
  "E-Value bound" = "E_Val_bound"
) |>
  kbl(format = 'markdown')

# plot
binary_results_life$transformed_table

# interpretation
cat(binary_results_life$interpretation)

# plot social
binary_results_social <- margot_plot(
  models_binary_social$combined_table,
  options = outcomes_options_social,
  label_mapping = label_mapping_social,
  include_coefficients = FALSE,
  save_output = FALSE,
  original_df = original_df,
  order = "evaluebound_asc"
)

# table
binary_results_social$transformed_table |> rename(
  "E-Value" = "E_Value",
  "E-Value bound" = "E_Val_bound"
) |>
  kbl(format = 'markdown')

# interpretation
cat(binary_results_social$interpretation)


# combine ate plots ------------------------------------------------------
# plot_ate_health <- binary_results_health_asc$plot
# plot_ate_psych <- binary_results_psych_asc$plot
# plot_ate_present <- binary_results_present_asc$plot
# plot_ate_life <- binary_results_life_asc$plot
# plot_ate_social <- binary_results_social_asc$plot
# 
# 
# 
# # create combined plot with annotations
# ate_plots_combined <- plot_ate_health + 
#   plot_ate_psych + 
#   plot_ate_present + 
#   plot_ate_life + 
#   plot_ate_social +
#   plot_annotation(
#     title = title_binary,
#     tag_levels = "A",
#     theme = theme(
#       plot.title = element_text(size = 20),
#       legend.position = "top"
#     )
#   ) +
#   plot_layout(guides = "collect")
# 
# # view combined plot
# ate_plots_combined

# combine all models -----------------------------------------------------
# merge all domain models into single object
all_models <- margot_bind_models(
  models_binary_health,
  models_binary_psych,
  models_binary_present,
  models_binary_life,
  models_binary_social
)


# graph
plot_all_models <- margot_plot(
  all_models$combined_table,
  options = options_all_models,
  save_output = FALSE,
  e_val_bound_threshold = 1.1,
  label_mapping = label_mapping_all,
  save_path = here::here(push_mods),
  original_df = original_df,
  include_coefficients = FALSE,
  order = "evaluebound_asc"
)

# view plot
plot_all_models$plot

# interpretation
cat(plot_all_models$interpretation)

# table
plot_all_models$transformed_table

# nice table
tables_list <- list(
  Health = binary_results_health$transformed_table,
  Psych = binary_results_psych$transformed_table,
  Present = binary_results_present$transformed_table,
  Life = binary_results_life$transformed_table,
  Social = binary_results_social$transformed_table
)

margot_bind_tables_markdown <- margot_bind_tables(
  tables_list = tables_list, #list(all_models$combined_table),
  sort_E_val_bound = "desc",
  e_val_bound_threshold = 1.1,
  highlight_color = NULL,
  bold = TRUE,
  rename_cols = TRUE,
  col_renames = list(
    "E-Value" = "E_Value",
    "E-Value bound" = "E_Val_bound"
  ),
  rename_ate = TRUE,
  threshold_col = "E_Val_bound",
  output_format = "markdown",
  kbl_args = list(
    booktabs = TRUE,
    caption = NULL,
    align = NULL
  )
)

# view markdown table
margot_bind_tables_markdown

# save for publication
here_save(margot_bind_tables_markdown, "margot_bind_tables_markdown")


# count models by category
cat("Number of original models:\n")
cat("Social models:", length(models_binary_social$results), "\n")
cat("Psych models:", length(models_binary_psych$results), "\n")
cat("Health models:", length(models_binary_health$results), "\n")
cat("Present models:", length(models_binary_present$results), "\n")
cat("Life models:", length(models_binary_life$results), "\n")
cat("\nTotal models in combined object:", length(all_models$results), "\n")


# flip negatively oriented outcomes --------------------------------------
# flip outcomes where higher values are worse for interpretation consistency
models_binary_flipped_all <- margot_flip_forests(
  all_models,
  flip_outcomes = c(
    "t2_alcohol_frequency",
    "t2_alcohol_intensity",
    "t2_hlth_bmi_z", 
    "t2_hlth_fatigue_z",
    "t2_kessler_latent_anxiety_z",
    "t2_kessler_latent_depression_z",
    "t2_rumination_z",
    "t2_perfectionism_z"
  )
)

# size
margot_size(models_binary_flipped_all)

# save (only jb - for lecture)
here_save_qs(models_binary_flipped_all, "models_binary_flipped_all", push_mods)

# omnibus heterogeneity tests --------------------------------------------
# test for treatment effect heterogeneity across all outcomes
result_ominbus_hetero_all <- margot::margot_omnibus_hetero_test(models_binary_flipped_all, label_mapping = label_mapping_all)

# view results table
result_ominbus_hetero_all$summary_table |> kbl("markdown")

# view test interpretation
cat(result_ominbus_hetero_all$brief_interpretation)


# rate test analysis -----------------------------------------------------
# define flipped outcome names for interpretation
flipped_names <- c("Alcohol Frequency", "Alcohol Intensity", "BMI", "Fatigue", "Anxiety", "Depression", "Perfectionism", "Rumination")

# save for publication
here_save(flipped_names, "flipped_names")

# create rate analysis table
rate_table_all <- margot_rate(
  models_binary_flipped_all, 
  label_mapping = label_mapping_all, 
  highlight_significant = TRUE
)

# view rate tables
rate_table_all$rate_autoc |> kbl("markdown")
rate_table_all$rate_qini |> kbl("markdown")


# generate interpretation
rate_interpretation_all <- margot_interpret_rate(
  rate_table_all, 
  flipped_outcomes = flipped_names
)

# view interpretations
cat(rate_interpretation_all$autoc_results)
cat(rate_interpretation_all$qini_results)
cat(rate_interpretation_all$comparison)


# check out model names
rate_interpretation_all$either_model_names
rate_interpretation_all$qini_model_names
rate_interpretation_all$both_model_names
rate_interpretation_all$autoc_model_names


# autoc plots ------------------------------------------------------------
# generate batch rate plots for models with significant heterogeneity
batch_rate_autoc_plots <- margot_plot_rate_batch(
  models_binary_flipped_all, 
  save_plots = FALSE,
  # just use rate autoc
  model_names = rate_interpretation_all$autoc_model_names  
)

# view selected autoc plots
batch_rate_autoc_plots$model_t2_log_hours_exercise_z
batch_rate_autoc_plots$model_t2_hlth_fatigue_z
batch_rate_autoc_plots$model_t2_self_control_z
batch_rate_autoc_plots$model_t2_meaning_sense_z

# for instruction
batch_rate_qini_plots <- margot_plot_rate_batch(
  models_binary_flipped_all, 
  save_plots = FALSE,
  model_names = rate_interpretation_all$qini_model_names
)

# for instruction -- note that initial heterogeneity dips below zero
batch_rate_qini_plots$model_t2_bodysat_z
batch_rate_qini_plots$model_t2_self_esteem_z
batch_rate_qini_plots$model_t2_belong_z


# QINI --------------------------------------------------------------------
# batch process heterogeneity results for qini
models_binary_batch_qini <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$qini_model_names,
  original_df = original_df,
  label_mapping = label_mapping_all
)

# check size
margot_size(models_binary_batch_qini)

# save
here_save_qs(models_binary_batch_qini, "models_binary_batch_qini", push_mods)

# view models
# first make graphs
plot_qini_exercise <- models_binary_batch_qini$model_t2_log_hours_exercise_z$qini_plot
plot_qini_fatigue <- models_binary_batch_qini$model_t2_hlth_fatigue_z$qini_plot
plot_qini_bodysat <- models_binary_batch_qini$model_t2_bodysat_z$qini_plot
plot_qini_self_control <- models_binary_batch_qini$model_t2_self_control_z$qini_plot
plot_qini_self_esteem <- models_binary_batch_qini$model_t2_self_esteem_z$qini_plot
plot_qini_belong <- models_binary_batch_qini$model_t2_belong_z$qini_plot

# recall the ate
plot_all_models$plot

# interpret qini curves
interpretation_qini_curves <- margot_interpret_qini(
  models_binary_batch_qini,
  model_names = rate_interpretation_all$qini_model_names,
  label_mapping = label_mapping_all
)

# view qini interpretation
cat(interpretation_qini_curves$qini_explanation)

# view summary table
interpretation_qini_curves$summary_table |> kbl("markdown")

# combine qini plots for visualisation
# patchwork allows us to group graphs together
library(patchwork)
qini_plots_combined <- 
  (plot_qini_exercise + plot_qini_fatigue + plot_qini_bodysat) / 
  (plot_qini_self_control + plot_qini_self_esteem + plot_qini_belong) + 
  plot_annotation(
    title = "Qini Plots: Reliable Priority 'Spending' at Fractional Budgets",
    tag_levels = "A",
    theme = theme(legend.position = "top")
  ) +
  plot_layout(guides = "collect")

# view combined qini plots
qini_plots_combined

# again compare with ate
plot_all_models$plot 


# policy tree analysis ---------------------------------------------------
# model_names_subset <- c(
#   "model_t2_log_hours_exercise_z",
#   "model_t2_self_control_z", 
#   "model_t2_sexual_satisfaction_z",
#   "model_t2_belong_z",
#   "model_t2_support_z"
# )

# make policy trees
plots_policy_trees <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$either_model_names, # defined above
  original_df = original_df,
  label_mapping = label_mapping_all
)

# generate policy tree interpretations
interpretation_policy_trees <- margot_interpret_policy_batch(
  models_binary_flipped_all,
  # use eithre model
  model_names = rate_interpretation_all$either_model_names, # defined above
  train_proportion = 0.8,
  original_df = original_df,
  label_mapping = label_mapping_all
)

# this will give you results
cat(interpretation_policy_trees)

# view plots --------------------------------------------------------------
# log hours exercise
plots_policy_trees$model_t2_log_hours_exercise_z$decision_tree
plots_policy_trees$model_t2_log_hours_exercise_z$policy_tree
plots_policy_trees$model_t2_log_hours_exercise_z$combined_plot

# fatigue
plots_policy_trees$model_t2_hlth_fatigue_z$decision_tree
plots_policy_trees$model_t2_hlth_fatigue_z$policy_tree
plots_policy_trees$model_t2_hlth_fatigue_z$combined_plot

# self control
plots_policy_trees$model_t2_self_control_z$decision_tree
plots_policy_trees$model_t2_self_control_z$policy_tree
plots_policy_trees$model_t2_self_control_z$combined_plot

# meaning sense
plots_policy_trees$model_t2_meaning_sense_z$decision_tree
plots_policy_trees$model_t2_meaning_sense_z$policy_tree
plots_policy_trees$model_t2_meaning_sense_z$combined_plot

# body satisfaction
plots_policy_trees$model_t2_bodysat_z$decision_tree
plots_policy_trees$model_t2_bodysat_z$policy_tree
plots_policy_trees$model_t2_bodysat_z$combined_plot

# self esteem
plots_policy_trees$model_t2_self_esteem_z$decision_tree
plots_policy_trees$model_t2_self_esteem_z$policy_tree
plots_policy_trees$model_t2_self_esteem_z$combined_plot

# belonging
plots_policy_trees$model_t2_belong_z$decision_tree
plots_policy_trees$model_t2_belong_z$policy_tree
plots_policy_trees$model_t2_belong_z$combined_plot


#############################################################################
# theoretical comparisons ---------------------------------------------------
# individual theoretical comparisons (if relevant)
# need to get values for wealth if wealth is compared

# step 1 get information for wealth for conditonal comparisons
head(df_grf$t0_log_household_inc_z)

# get mean on original data scale
log_mean_inc <- mean(original_df$t0_log_household_inc, na.rm = TRUE)

# get sd on original data scale
log_sd_inc <- sd(original_df$t0_log_household_inc, na.rm = TRUE)

# function to get back to data scale
margot_back_transform_log_z(
  log_mean = log_mean_inc, 
  log_sd = log_sd_inc,
  z_scores = c(-1, 0, 1),
  label = "data_scale"
)

# define complex conditions for subsetting
complex_condition_political <- X[, "t0_political_conservative_z"] > -1 &
  X[, "t0_political_conservative_z"] < 1

complex_condition_wealth <- X[, "t0_log_household_inc_z"] > -1 & 
  X[, "t0_log_household_inc_z"] < 1

# wealth subsets
subsets_standard_wealth <- list(
  Poor = list(
    var = "t0_log_household_inc_z",
    value = -1,
    operator = "<",
    description = "HShold income < -1 SD (NZD ~41k)"
    # label = "Conservative"  # label remains as is, but could be changed if desired
  ),
  MiddleIncome = list(
    subset_condition = complex_condition_wealth,
    description = "HShold income within +/-1SD (> NZD 41k < NZD 191k)"
  ),
  Rich = list(
    var = "t0_log_household_inc_z",
    value = 1,
    operator = ">",
    description = "HShold income > +1 SD (NZD 191k)",
    label = "Rich"
  )
)

# political subsets
subsets_standard_political <- list(
  Liberal = list(
    var = "t0_political_conservative_z",
    value = -1,
    operator = "<",
    description = "< -1 SD in political conservativism"
  ),
  Moderates = list(
    var = "t0_political_conservative_z",
    # operator = "<",
    subset_condition = complex_condition_political,
    description = "Effects among those > -1 SD and < +1 in political conservativism",
    label = "Centrist"
  ),
  Conservative = list(
    var = "t0_political_conservative_z",
    value = 1,
    operator = ">",
    description = "> +1 SD in political conservativism",
    label = "Conservative"
  )
)

# gender subsets
subsets_standard_gender <- list(
  Female = list(
    var = "t0_male_binary",
    value = 0,
    description = "Females"
  ),
  Male = list(
    var = "t0_male_binary",
    value = 1,
    description = "Males"
  )
) 

# ethnicity subsets
subsets_standard_ethnicity <- list(
  Asian = list(
    var = "t0_eth_cat_asian_binary",
    value = 1,
    description = "Asians"
  ),
  Euro = list(
    var = "t0_eth_cat_euro_binary",
    value = 1,
    description = "Europeans (Pakeha)"
  ),
  Pacific = list(
    var = "t0_eth_cat_pacific_binary",
    value = 1,
    description = "Pacific Peoples"
  ),
  Maori = list(
    var = "t0_eth_cat_maori_binary",
    value = 1,
    description = "Māori"
  )
)

# subsets_standard_cohort <- list(
#   boomers = list(
#     var = "t0_gen_cohort_gen_Boomers_binary",
#     value = 1,
#     description = "Baby Boomers",
#     label = "Boomers"  # label remains as is, but could be changed if desired
#   ),
#   gen_X = list(
#     var = "t0_gen_cohort_gen_X_binary",
#     value = 1,
#     description = "Generation X",
#     label = "Generation_X"  # label remains as is, but could be changed if desired
#   ),
#   gen_Y = list(
#     var = "t0_gen_cohort_gen_Y_binary",
#     value = 1,
#     description = "Generation Y",
#     label = "Generation_Y"  # label remains as is, but could be changed if desired
#   ),
#   gen_Z = list(
#     var = "t0_gen_cohort_gen_Z_binary",
#     value = 1,
#     description = "Generation Z",
#     label = "Generation_Z"  # label remains as is, but could be changed if desired
#   )
# )

# check
mean(original_df$t0_age) - sd(original_df$t0_age) #

# if we have specific groups to compare
complex_condition_age_under_neg_1_sd  <- X[, "t0_age_z"] < -1 
complex_condition_age_gr_eq_neg_1_sd  <- X[, "t0_age_z"] >= -1 

# age subsets
subsets_standard_cohort <- list(
  under_37 = list(
    value = complex_condition_age_under_neg_1_sd,
    description = "under_37"
  ),
  over_37 = list(
    value = complex_condition_age_gr_eq_neg_1_sd,
    description = "37 and over"
  )
)

# batch planned subgroup analysis -----------------------------------------
# set up lists of models, names, and subtitles
domain_models <- list(
  models_binary_health,
  models_binary_psych,
  models_binary_present,
  models_binary_life,
  models_binary_social
)

# set up domain names
domain_names <- c("health", "psych", "present", "life", "social")

# set up subtitles
subtitles <- c(
  subtitle_health,
  subtitle_psych,
  subtitle_present,
  subtitle_life,
  subtitle_social
)

# set up subset types in a list
subset_types <- list(
  wealth = subsets_standard_wealth,
  ethnicity = subsets_standard_ethnicity,
  political = subsets_standard_political,
  gender = subsets_standard_gender#,
  # cohort = subsets_standard_cohort
)

# run the batch processing for all domains and subsets
planned_subset_results <- margot_planned_subgroups_batch(
  domain_models = domain_models,
  X = X,
  base_defaults = base_defaults_binary,
  subset_types = subset_types,
  original_df = original_df,
  domain_names = domain_names,
  subtitles = subtitles,
  push_mods = push_mods
)

# results
# health subgroup
cat(planned_subset_results$health$wealth$explanation)
cat(planned_subset_results$health$ethnicity$explanation) 
cat(planned_subset_results$health$political$explanation) 
cat(planned_subset_results$health$gender$explanation) 
cat(planned_subset_results$health$cohort$explanation) 


cat(planned_subset_results$psych$wealth$explanation) 
cat(planned_subset_results$psych$ethnicity$explanation) 
cat(planned_subset_results$psych$political$explanation) 
cat(planned_subset_results$psych$gender$explanation) 
cat(planned_subset_results$psych$cohort$explanation) 


cat(planned_subset_results$present$wealth$explanation) 
cat(planned_subset_results$present$ethnicity$explanation) 
cat(planned_subset_results$present$political$explanation) 
cat(planned_subset_results$present$gender$explanation) 
cat(planned_subset_results$present$cohort$explanation) 


cat(planned_subset_results$life$wealth$explanation) 
cat(planned_subset_results$life$ethnicity$explanation) 
cat(planned_subset_results$life$political$explanation) 
cat(planned_subset_results$life$gender$explanation) 
cat(planned_subset_results$life$cohort$explanation) 

cat(planned_subset_results$social$wealth$explanation) 
cat(planned_subset_results$social$ethnicity$explanation) 
cat(planned_subset_results$social$political$explanation) 
cat(planned_subset_results$social$gender$explanation) 
cat(planned_subset_results$social$cohort$explanation) 


# combine tables ----------------------------------------------------------
# wrap each domain's table in a list:
# wealth subgroups --------------------------------------------------------
tables_list_poor <- list(
  Health = planned_subset_results$health$wealth$results$Poor$transformed_table,
  Psych  = planned_subset_results$psych$wealth$results$Poor$transformed_table,
  Life   = planned_subset_results$life$wealth$results$Poor$transformed_table,
  Social = planned_subset_results$social$wealth$results$Poor$transformed_table
)

# new function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_poor,
  bold = TRUE,
  kbl_args = list(booktabs = TRUE, caption = "Wealth Subgroup Analysis: Poor"),
  highlight_color = NULL, 
  output_format = "html"
)

# create table list for middle income subgroup
tables_list_middleincome <- list(
  Health = planned_subset_results$health$wealth$results$MiddleIncome$transformed_table,
  Psych  = planned_subset_results$psych$wealth$results$MiddleIncome$transformed_table,
  Life   = planned_subset_results$life$wealth$results$MiddleIncome$transformed_table,
  Social = planned_subset_results$social$wealth$results$MiddleIncome$transformed_table
)

# bind tables and display results
margot::margot_bind_tables(
  tables_list = tables_list_middleincome,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE, 
    caption = "Wealth Subgroup Analysis: Middle Income"
  ),
  highlight_color = NULL, 
  output_format = "html"
)

planned_subset_results$health$wealth$results$Rich$transformed_table

tables_list_rich <- list(
  Health = planned_subset_results$health$wealth$results$Rich$transformed_table,
  Psych  = planned_subset_results$psych$wealth$results$Rich$transformed_table,
  Life   = planned_subset_results$life$wealth$results$Rich$transformed_table,
  Social = planned_subset_results$social$wealth$results$Rich$transformed_table
)


# new function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_rich,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE, 
    caption = "Wealth Subgroup Analysis: Rich"
  ),
  highlight_color = NULL, 
  output_format = "html"
)

# cohort subgroups --------------------------------------------------------

# wealth subgroups --------------------------------------------------------
planned_subset_results$health$cohort$results$Generation_X$transformed_table

planned_subset_results$health$cohort$results$Boomers$transformed_table
tables_list_boomers <- list(
  Health = planned_subset_results$health$cohort$results$Boomers$transformed_table,
  Psych  = planned_subset_results$psych$cohort$results$Boomers$transformed_table,
  Life   = planned_subset_results$psych$cohort$results$Boomers$transformed_table,
  Social = planned_subset_results$psych$cohort$results$Boomers$transformed_table
)


# new function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_boomers,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE, 
    caption = "Cohort Subgroup Analysis: Boomers"
  ),
  highlight_color = NULL,
  output_format = "html"
)


# margot_bind_tables(tables_list = tables_lisblue()# margot_bind_tables(tables_list = tables_list,
#                    bold = TRUE,
#                    highlight_color = NULL, output_format = "latex")
tables_list_genZ <- list(
  Health = planned_subset_results$health$cohort$results$Generation_Z$transformed_table,
  Psych  = planned_subset_results$psych$cohort$results$Generation_Z$transformed_table,
  Life   = planned_subset_results$psych$cohort$results$Generation_Z$transformed_table,
  Social = planned_subset_results$psych$cohort$results$Generation_Z$transformed_table
)


# new function bind tables
# new function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_genZ,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE, 
    caption = "Cohort Subgroup Analysis: Generation Z"
  ),
  highlight_color = NULL,
  output_format = "html"
)

# new function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_rich,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE, 
    caption = "Wealth Subgroup Analysis: Rich"
  ),
  highlight_color = NULL,
  output_format = "html"
)


# plots -------------------------------------------------------------------
# Results Plots
# health
plots_subgroup_wealth_health <- wrap_plots(
  list(
    planned_subset_results$health$wealth$results$Poor$plot,
    planned_subset_results$health$wealth$results$MiddleIncome$plot,
    planned_subset_results$health$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(
    title = subtitle_health,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_wealth_health)

# plots
plots_subgroup_ethnicity_health<- wrap_plots(
  list(
    planned_subset_results$health$ethnicity$results$Asian$plot,
    planned_subset_results$health$ethnicity$results$Euro$plot,
    planned_subset_results$health$ethnicity$results$Maori$plot,
    planned_subset_results$health$ethnicity$results$Pacific$plot
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_health,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_ethnicity_health)

# plots
plots_subgroup_political_health <- wrap_plots(
  list(
    planned_subset_results$health$political$results$Conservative$plot,
    planned_subset_results$health$political$results$Centrist$plot,
    planned_subset_results$health$political$results$Conservative$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_health,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_political_health)

# plots
plots_subgroup_gender_health<- wrap_plots(
  list(
    planned_subset_results$health$gender$results$Female$plot,
    planned_subset_results$health$gender$results$Male$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_health,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_gender_health)

# plots
plots_subgroup_cohort_health<- wrap_plots(
  list(
    planned_subset_results$health$cohort$results$Boomers$plot,
    planned_subset_results$health$cohort$results$Generation_X$plot,
    planned_subset_results$health$cohort$results$Generation_Y$plot,
    planned_subset_results$health$cohort$results$Generation_Z$plot
    
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_health,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_cohort_health)

# psychological well-being
plots_subgroup_wealth_psych <- wrap_plots(
  list(
    planned_subset_results$psych$wealth$results$Poor$plot,
    planned_subset_results$psych$wealth$results$MiddleIncome$plot,
    planned_subset_results$psych$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(
    title = subtitle_psych,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_wealth_psych)

# plots
plots_subgroup_ethnicity_psych<- wrap_plots(
  list(
    planned_subset_results$psych$ethnicity$results$Asian$plot,
    planned_subset_results$psych$ethnicity$results$Euro$plot,
    planned_subset_results$psych$ethnicity$results$Maori$plot,
    planned_subset_results$psych$ethnicity$results$Pacific$plot
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_psych,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_ethnicity_psych)

# plots
plots_subgroup_political_psych <- wrap_plots(
  list(
    planned_subset_results$psych$political$results$Conservative$plot,
    planned_subset_results$psych$political$results$Centrist$plot,
    planned_subset_results$psych$political$results$Conservative$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_psych,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_political_psych)

# plots
plots_subgroup_gender_psych<- wrap_plots(
  list(
    planned_subset_results$psych$gender$results$Female$plot,
    planned_subset_results$psych$gender$results$Male$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_psych,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_gender_psych)

# plots
plots_subgroup_cohort_psych<- wrap_plots(
  list(
    planned_subset_results$psych$cohort$results$Boomers$plot,
    planned_subset_results$psych$cohort$results$Generation_X$plot,
    planned_subset_results$psych$cohort$results$Generation_Y$plot,
    planned_subset_results$psych$cohort$results$Generation_Z$plot
    
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_psych,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_cohort_psych)

# present focussed well-being
plots_subgroup_wealth_present <- wrap_plots(
  list(
    planned_subset_results$present$wealth$results$Poor$plot,
    planned_subset_results$present$wealth$results$MiddleIncome$plot,
    planned_subset_results$present$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(
    title = subtitle_present,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_wealth_present)

# plots
plots_subgroup_ethnicity_present<- wrap_plots(
  list(
    planned_subset_results$present$ethnicity$results$Asian$plot,
    planned_subset_results$present$ethnicity$results$Euro$plot,
    planned_subset_results$present$ethnicity$results$Maori$plot,
    planned_subset_results$present$ethnicity$results$Pacific$plot
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_present,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_ethnicity_present)

# plots
plots_subgroup_political_present <- wrap_plots(
  list(
    planned_subset_results$present$political$results$Conservative$plot,
    planned_subset_results$present$political$results$Centrist$plot,
    planned_subset_results$present$political$results$Conservative$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_present,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_political_present)

# plots
plots_subgroup_gender_present<- wrap_plots(
  list(
    planned_subset_results$present$gender$results$Female$plot,
    planned_subset_results$present$gender$results$Male$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_present,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view  
print(plots_subgroup_gender_present)

# plots
plots_subgroup_cohort_present<- wrap_plots(
  list(
    planned_subset_results$present$cohort$results$Boomers$plot,
    planned_subset_results$present$cohort$results$Generation_X$plot,
    planned_subset_results$present$cohort$results$Generation_Y$plot,
    planned_subset_results$present$cohort$results$Generation_Z$plot
    
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_present,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_cohort_present)


## life focussed well-being
plots_subgroup_wealth_life <- wrap_plots(
  list(
    planned_subset_results$life$wealth$results$Poor$plot,
    planned_subset_results$life$wealth$results$MiddleIncome$plot,
    planned_subset_results$life$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(
    title = subtitle_life,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_wealth_life)

# plots
plots_subgroup_ethnicity_life<- wrap_plots(
  list(
    planned_subset_results$life$ethnicity$results$Asian$plot,
    planned_subset_results$life$ethnicity$results$Euro$plot,
    planned_subset_results$life$ethnicity$results$Maori$plot,
    planned_subset_results$life$ethnicity$results$Pacific$plot
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_life,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_ethnicity_life)

# plots
plots_subgroup_political_life <- wrap_plots(
  list(
    planned_subset_results$life$political$results$Conservative$plot,
    planned_subset_results$life$political$results$Centrist$plot,
    planned_subset_results$life$political$results$Conservative$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_life,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_political_life)

# plots
plots_subgroup_gender_life<- wrap_plots(
  list(
    planned_subset_results$life$gender$results$Female$plot,
    planned_subset_results$life$gender$results$Male$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_life,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_gender_life)

# plots
plots_subgroup_cohort_life<- wrap_plots(
  list(
    planned_subset_results$life$cohort$results$Boomers$plot,
    planned_subset_results$life$cohort$results$Generation_X$plot,
    planned_subset_results$life$cohort$results$Generation_Y$plot,
    planned_subset_results$life$cohort$results$Generation_Z$plot
    
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_life,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

print(plots_subgroup_cohort_life)

# social well-being
plots_subgroup_wealth_social <- wrap_plots(
  list(
    planned_subset_results$social$wealth$results$Poor$plot,
    planned_subset_results$social$wealth$results$MiddleIncome$plot,
    planned_subset_results$social$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(
    title = subtitle_social,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_wealth_social)

plots_subgroup_ethnicity_social<- wrap_plots(
  list(
    planned_subset_results$social$ethnicity$results$Asian$plot,
    planned_subset_results$social$ethnicity$results$Euro$plot,
    planned_subset_results$social$ethnicity$results$Maori$plot,
    planned_subset_results$social$ethnicity$results$Pacific$plot
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_social,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_ethnicity_social)


plots_subgroup_political_social <- wrap_plots(
  list(
    planned_subset_results$social$political$results$Conservative$plot,
    planned_subset_results$social$political$results$Centrist$plot,
    planned_subset_results$social$political$results$Conservative$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_social,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_political_social)

# plots
plots_subgroup_gender_social<- wrap_plots(
  list(
    planned_subset_results$social$gender$results$Female$plot,
    planned_subset_results$social$gender$results$Male$plot
  ),
  ncol = 1
)+
  patchwork::plot_annotation(
    title = subtitle_social,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

# view
print(plots_subgroup_gender_social)

# plots
plots_subgroup_cohort_social<- wrap_plots(
  list(
    planned_subset_results$social$cohort$results$Boomers$plot,
    planned_subset_results$social$cohort$results$Generation_X$plot,
    planned_subset_results$social$cohort$results$Generation_Y$plot,
    planned_subset_results$social$cohort$results$Generation_Z$plot
    
  ),
  ncol = 2
)+
  patchwork::plot_annotation(
    title = subtitle_social,
    theme = theme(plot.title = element_text(size = 18,  face = "bold"))
  )

print(plots_subgroup_cohort_social)



# plot options: showcased ---------------------------------------------
# default
margot_plot_decision_tree(
  models_binary_social,
  "model_t2_support_z",
)
# tighten branches for easier viewing in single graphs
margot::margot_plot_decision_tree(
  models_binary_social,
  "model_t2_support_z",
  span_ratio = .30,
  text_size = 3.8,
  border_size = .1,
  #  title = "none",
  original_df = original_df
)
# colour decision node
margot::margot_plot_decision_tree(
  models_binary_social,
  "model_t2_support_z",
  span_ratio = .3,
  text_size = 4, 
  title = "New Title",
  non_leaf_fill =  "violet",
  original_df = original_df
)
# make new title
margot::margot_plot_decision_tree(
  models_binary_social,
  "model_t2_support_z",
  span_ratio = .2,
  text_size = 3, 
  title = "New Title",
  non_leaf_fill =  "white",
  original_df = original_df
)

# remove title
margot::margot_plot_decision_tree(
  models_binary_social,
  "model_t2_support_z",
  text_size = 5, 
  title = 'none', # set title to none
  original_df = original_df
)

# policy tree options 
# select only plot 1 change alpha
margot::margot_plot_policy_tree(
  models_binary_social,
  "model_t2_support_z",
  point_alpha = .25, 
  plot_selection = "p1"
)
# select only plot 2 change size of axis_text
# change colours, modify etc... 
margot::margot_plot_policy_tree(
  models_binary,
  "model_t2_agreeableness_z",
  plot_selection = "p2",
  axis_title_size = 30,
  split_label_size = 20,
  split_label_color = "red",
  split_line_color = "red",
)

# adjust only the alpha
margot::margot_plot_policy_tree(
  models_binary,
  "model_t2_agreeableness_z",
  point_alpha = .1
)