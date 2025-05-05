# # example 2 02-data-wrangling-grf-model
# get data into wide format and ready for modelling using grf
# joseph.bulbulia@gmail.com
# may 2025

# restart fresh session
# rstudioapi::restartSession()
# set reproducibility
set.seed(123)

# essential libraries ---------------------------------------------------------

if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}

# check if pacman is installed; if not, install it
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}


# check package version
packageVersion(pkg = "margot")


# set seed ----------------------------------------------------------------


# reproducibility
set.seed(123)


# directory path configuration -----------------------------------------------
# save path (customise for your own computer) ----------------------------
push_mods <- here::here("models_example_2")

# read original data (for plots) ------------------------------------------
original_df <- margot::here_read("df_wide", push_mods)

# plot title --------------------------------------------------------------

title_binary = "Extraversion (binary)"
filename_prefix = "grf_extraversion_wb"


# import names ------------------------------------------------------------
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

# bind all outcomes
raw_outcomes_all <- c(raw_outcomes_health, 
                      raw_outcomes_psych,
                      raw_outcomes_present, 
                      raw_outcomes_life, 
                      raw_outcomes_social)

# all outcomes
t2_outcomes_all <- c(t2_outcome_health_z, t2_outcome_psych_z, t2_outcome_present_z, t2_outcome_life_z, t2_outcome_social_z)

# save for pub
here_save(t2_outcomes_all, "t2_outcomes_all")

# save all outcomes
here_save(raw_outcomes_all, "raw_outcomes_all")

# label mappings for health outcomes
label_mapping_health <- list(
  "t2_alcohol_frequency_weekly_z" = "Alcohol Frequency",
  "t2_alcohol_intensity_weekly_z" = "Alcohol Intensity",
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
  # "t2_perfectionism_z" = "Perfectionism",  ** --- EXPOSURE ---**
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

# label mapping all -------------------------------------------------------
label_mapping_all <- c(
  label_mapping_health,
  label_mapping_psych,
  label_mapping_present,
  label_mapping_life,
  label_mapping_social
)

# save
here_save(label_mapping_all, "label_mapping_all")

# load libraries ----------------------------------------------------------
# load necessary libraries
pacman::p_load(
  clarify,      # sensitivity analysis for causal inference
  cobalt,       # covariate balance tables and plots
  DiagrammeR,   # graph and network visualization
  doParallel,   # parallel processing with foreach
  fastDummies,  # fast creation of dummy variables
  fs,           # cross-platform file system operations
  ggplot2,      # data visualisation
  glmnet,       # lasso and elastic-net regularized models
  grf,          # generalized random forests
  gtsummary,    # summary tables for regression models
  here,         # simple and robust file referencing
  janitor,      # data cleaning and validation
  kableExtra,   # advanced table formatting
  lmtp,         # longitudinal targeted maximum likelihood estimation
  # margot,       # functions for casual inference
  naniar,       # handling and visualization of missing data
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
  patchwork,     # multiple plots
  labelled,
  cli,
  rlang
)


# start analysis here ----------------------------------------------------
# import data
df_grf <- margot::here_read('df_grf')

# check
colnames(df_grf)

# check missing values (baseline missingness is handled by grf)
# takes a long time to render so commented out
# naniar::vis_miss(df_grf, warn_large_data = FALSE)

# check another way
naniar::gg_miss_var(df_grf)

# import names of baseline covariates
E <- margot::here_read("E")

# check
print(E)


# get exposure variable, call it W ----------------------------------------
# check that variables are 0 or 1
df_grf[[t1_name_exposure_binary]]

# check: ensure both binaries only take values 0 or 1 (ignore NA)
stopifnot(
  all(
    df_grf[[t1_name_exposure_binary]][
      !is.na(df_grf[[t1_name_exposure_binary]])
    ] %in% 0:1
  )
)

# needs to be a matrix
W <- as.vector(df_grf[[t1_name_exposure_binary]])

# set weights
weights <- df_grf$t1_adjusted_weights 

# view/ check none too extreme
hist(weights)

# remove attributes of baseline co-variaties
X <-  margot::remove_numeric_attributes(df_grf[E]) 

# set model defaults -----------------------------------------------------
grf_defaults <- list(
  seed = 123, # reproduce results
  stabilize.splits = TRUE, # robustness
  # min.node.size = 5,  # default is five/ requires at least 5 observed in control and treated
  # set higher for greater smoothing
  num.trees = 2000 # grf default = 2000 # set lower or higher depending on storage
)

# set defaults for graphs (see bottom of script for options)
# see margot package for options
decision_tree_defaults <- list(
  span_ratio = .3,
  text_size = 3.8,
  y_padding = 0.25  # use full parameter name
  # edge_label_offset = .002, # options
  # border_size = .05 # options
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


# Uncomment and Run Tests Once ----------------------------------------------------------


# 
# # test --------------------------------------------------------------------
# n <- nrow(X) # n in sample
# 
# # define training sample
# toy <- sample(1:n, n / 4) # get half sample
# 
# # test set data
# toy_data = df_grf[toy, ]
# 
# # check size
# nrow(toy_data)
# 
# # covariates
# X_toy = X[toy, ]
# 
# # test set covariates check
# str(X_toy)
# 
# # test set exposure
# W_toy = W[toy]
# 
# # test weights
# weights_toy = weights[toy]
# 
# # 1. fit model and save everything
# cf_out <- margot_causal_forest(
#   data        = toy_data,
#   outcome_vars=  c("t2_kessler_latent_depression_z"),
#   covariates  = X_toy,
#   W           = W_toy,
#   weights     = weights_toy,
#   save_data   = TRUE,   #  ← needed for flipping models
#   save_models = TRUE, # ←  save models
# )
# 
# 
# # inspect qini data
# # where there are very low or high propensity scores (prob of exposure) we might consider trimming
# table_inspect_qini <- margot::margot_inspect_qini(cf_out, propensity_bounds = c(0.01, 0.97))
# table_inspect_qini
# 
# # test model: 1L tree
# policy_tree_test_1L <- margot_plot_policy_tree(
#   mc_test    = cf_out,
#   model_name = "model_t2_kessler_latent_depression_z",
#   max_depth  = 1L, # ← new argument
#   original_df = original_df,
#   label_mapping = label_mapping_all
# )
# 
# # view - notice this plot is **not persuasive**
# policy_tree_test_1L
# 
# 
# # test level 2 - we will default to a policy tree of two levels
# policy_tree_test <- margot_plot_policy_tree(
#   mc_test    = cf_out,
#   model_name = "model_t2_kessler_latent_depression_z",
#   max_depth  = 2L, # ← new argument
#   original_df = original_df,
#   label_mapping = label_mapping_all
# )
# 
# # view - a little better but as we'll see there's not much evidence for HTE
# plot(policy_tree_test)
# 
# 
# # decision tree 1L test level 1
# decision_tree_test_1L <- margot_plot_decision_tree(
#   cf_out,
#   model_name = "model_t2_kessler_latent_depression_z",
#   max_depth  = 1L, # ← new argument
#   original_df = original_df,
#   label_mapping = label_mapping_all
# )
# 
# decision_tree_test_1L
# 
# # result was
# cf_out$results$model_t2_kessler_latent_depression_z$policy_tree_depth_1
# 
# # plotting using policy tree gives correct split
# plot(cf_out$results$model_t2_kessler_latent_depression_z$policy_tree_depth_1)
# 
# # plotting using margot function does not give correct label colour
# decision_tree_test_1L
# 
# 
# # decision tree 2L
# decision_tree_test_2L <- margot_plot_decision_tree(
#   cf_out,
#   model_name = "model_t2_kessler_latent_depression_z",
#   max_depth  = 2L, # ← new argument
#   original_df = original_df,
#   label_mapping = label_mapping_all
# )
# 
# # view
# decision_tree_test_2L
# cf_out$results$model_t2_kessler_latent_depression_z$policy_tree_depth_2
# 
# # check again policy tree graph
# plot(cf_out$results$model_t2_kessler_latent_depression_z$policy_tree_depth_2)
# 
# # more tests
# combo1 <- margot_plot_policy_combo(
#   result_object       = cf_out,
#   model_name          = "model_t2_kessler_latent_depression_z",
#   max_depth           = 1L,       # <— depth‐1 decision tree
#   policy_tree_args    = list(point_alpha = 0.7),
#   decision_tree_args  = list(text_size   = 4)
# )
# 
# # L1 combo tree ** (we'll generally plot combo trees) ** 
# combo1$combined_plot
# 
# # test L2 combo tree
# combo2 <- margot_plot_policy_combo(
#   result_object       = cf_out,
#   model_name          = "model_t2_kessler_latent_depression_z",
#   max_depth           = 2L,       # <— depth‐2 decision tree
#   policy_tree_args    = list(point_alpha = 0.7),
#   decision_tree_args  = list(text_size   = 4)
# )
# # view
# combo2$combined_plot
# 
# # test batch function 1 L (ignore warnings)
# models_binary_batch_test <-margot_policy(
#   cf_out,
#   save_plots = FALSE,
#   output_dir = here::here(push_mods),
#   decision_tree_args = decision_tree_defaults,
#   policy_tree_args = policy_tree_defaults,
#   model_names = c("model_t2_kessler_latent_depression_z"),
#   original_df = original_df,
#   label_mapping = label_mapping_psych,
#   max_depth     = 1L # test with depth 1
# )
# 
# 
# models_binary_batch_test$model_t2_kessler_latent_depression_z$qini_plot
# models_binary_batch_test$model_t2_kessler_latent_depression_z$combined_plot
# 
# 
# # interpretation of qini curves
# interpretation_qini_curves_test_1L <- margot_interpret_policy_tree(
#   model       = cf_out,
#   model_name  = "model_t2_kessler_latent_depression_z",
#   max_depth   = 1L,
#   label_mapping = label_mapping_all,
#   original_df   = original_df
# )
# 
# # read interpretation 1 L
# cat(interpretation_qini_curves_test_1L)
# 
# # interpretation for 2L policy tree
# interpretation_qini_curves_test <- margot_interpret_policy_batch(
#   models       = cf_out,
#   model_name  = "model_t2_kessler_latent_depression_z",
#   max_depth   = 2L,
#   label_mapping = label_mapping_all,
#   original_df   = original_df
# )
# 
# cat(interpretation_qini_curves_test)
# 
# 
# # 2. flip the selected outcomes (and regen trees)
# # often we will reverse outcomes that we want to minimise
# cf_out_f <- margot_flip_forests(
#   model_results = cf_out,
#   flip_outcomes = c("t2_kessler_latent_depression_z"),
#   recalc_policy = TRUE
# )
# 
# # check
# cf_out_f$flip_outcomes_postprocessed
# 
# policy_tree_test_1 <- margot_interpret_policy_tree(
#   model    = cf_out_f,
#   model_name = "model_t2_kessler_latent_depression_z",
#   max_depth  = 1L, # ← new argument
#   original_df = original_df,
#   label_mapping = label_mapping_all
# )
# 
# cat(policy_tree_test_1)
# 
# # check  we should find that treatments are rare
# policy_tree_test_2 <- margot_plot_policy_tree(
#   cf_out_f,
#   model_name = "model_t2_kessler_latent_depression_z",
#   max_depth  = 2L, # ← new argument
#   original_df = original_df,
#   label_mapping = label_mapping_all
# )
# 
# # note
# policy_tree_test_2
# 
# # depth-1 (ignore warnings)
# tree1 <- margot_plot_decision_tree(
#   result_object = cf_out_f,
#   model_name    = "model_t2_kessler_latent_depression_z",
#   max_depth     = 1L,
#   original_df   = original_df,
#   label_mapping = label_mapping_all
# )
# 
# # depth-2 scatter panels
# tree2 <- margot_plot_decision_tree(
#   result_object = cf_out_f,
#   model_name    = "model_t2_kessler_latent_depression_z",
#   max_depth     = 2L,
#   original_df   = original_df,
#   label_mapping = label_mapping_all
# )
# 
# # 3. rescue any empty‐gain Qini curves via propensity trimming
# cf_out_flipped <- margot_rescue_qini(
#   model_results      = cf_out_f,
#   propensity_bounds  = c(0.05, 0.95)
# )
# 
# # test plots
# models_binary_batch_test <-margot_policy(
#   cf_out_f,
#   save_plots = FALSE,
#   output_dir = here::here(push_mods),
#   decision_tree_args = decision_tree_defaults,
#   policy_tree_args = policy_tree_defaults,
#   model_names = c("model_t2_kessler_latent_depression_z"),
#   original_df = original_df,
#   label_mapping = label_mapping_psych,
#   max_depth     = 2L
# )
# 
# # plots
# models_binary_batch_test[[1]][[1]]
# models_binary_batch_test[[1]][[2]]
# models_binary_batch_test[[1]][[3]]
# 


# full models -------------------------------------------------------------

# ** uncomment to run full models** 

# # health models -----------------------------------------------------------
# models_binary_health <- margot::margot_causal_forest(
#   data = df_grf,
#   outcome_vars = t2_outcome_health_z,
#   covariates = X,
#   W = W,
#   weights = weights,
#   grf_defaults = grf_defaults,
#   top_n_vars = 15,
#   save_models = TRUE,
#   save_data = TRUE,
#   train_proportion = 0.7
# )
# 
# # check size if needed
# margot::margot_size(models_binary_health)
# 
# # save model
# margot::here_save_qs(models_binary_health, "models_binary_health", push_mods)
# 
# 
# # psych models ------------------------------------------------------------
# models_binary_psych <- margot::margot_causal_forest(
#   data = df_grf,
#   outcome_vars = t2_outcome_psych_z,
#   covariates = X,
#   W = W,
#   weights = weights,
#   grf_defaults = grf_defaults,
#   top_n_vars = 15,
#   save_models = TRUE,
#   save_data = TRUE,
#   train_proportion = 0.7
# )
# 
# # save model
# margot::here_save_qs(models_binary_psych, "models_binary_psych", push_mods)
# 
# 
# # present models ----------------------------------------------------------
# models_binary_present <- margot::margot_causal_forest(
#   data = df_grf,
#   outcome_vars = t2_outcome_present_z,
#   covariates = X,
#   W = W,
#   weights = weights,
#   grf_defaults = grf_defaults,
#   top_n_vars = 15,
#   save_models = TRUE,
#   save_data = TRUE,
#   train_proportion = 0.7
# )
# 
# # save model
# margot::here_save_qs(models_binary_present, "models_binary_present", push_mods)
# 
# 
# # life models -------------------------------------------------------------
# models_binary_life <- margot::margot_causal_forest(
#   data = df_grf,
#   outcome_vars = t2_outcome_life_z,
#   covariates = X,
#   W = W,
#   weights = weights,
#   grf_defaults = grf_defaults,
#   top_n_vars = 15,
#   save_models = TRUE,
#   save_data = TRUE,
#   train_proportion = 0.7
# )
# 
# # save model
# margot::here_save_qs(models_binary_life, "models_binary_life", push_mods)
# 
# 
# # social models -----------------------------------------------------------
# models_binary_social <- margot::margot_causal_forest(
#   data = df_grf,
#   outcome_vars = t2_outcome_social_z,
#   covariates = X,
#   W = W,
#   weights = weights,
#   grf_defaults = grf_defaults,
#   top_n_vars = 15,
#   save_models = TRUE,
#   save_data = TRUE,
#   train_proportion = 0.7
# )
# 
# 
# # save model
# margot::here_save_qs(models_binary_social, "models_binary_social", push_mods)
# 


# read results ------------------------------------------------------------

# if you save models you do not need to re-run them
models_binary_health <- margot::here_read_qs("models_binary_health", push_mods)
models_binary_psych <- margot::here_read_qs("models_binary_psych", push_mods)
models_binary_present <- margot::here_read_qs("models_binary_present", push_mods)
models_binary_life <- margot::here_read_qs("models_binary_life", push_mods)
models_binary_social <- margot::here_read_qs("models_binary_social", push_mods)

# check size (example)
margot::margot_size(models_binary_health)


# make graphs -------------------------------------------------------------
# titles
subtitle_health = "Health"
subtitle_psych = "Psychological Well-being"
subtitle_present = "Present-Focussed Well-being"
subtitle_life = "Life-Focussed Well-being"
subtitle_social = "Social Well-being"


# settings
x_offset = -.5
x_lim_lo = -.5
x_lim_hi = .5


# defaults for ATE plots
base_defaults_binary <- list(
  type = "RD",
  title = title_binary,
  e_val_bound_threshold = 1.2,
  colors = c(
    "positive" = "#E69F00",
    "not reliable" = "grey50",
    "negative" = "#56B4E9"
  ),
  x_offset = x_offset,
  # will be set based on type
  x_lim_lo = x_lim_lo,
  # will be set based on type
  x_lim_hi = x_lim_hi,
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
  e_val_bound_threshold = 1.2
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
  e_val_bound_threshold = 1.2,
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
  e_val_bound_threshold = 1.2,
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
  e_val_bound_threshold = 1.2,
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

# make markdown tables (to be imported into the manuscript)
margot_bind_tables_markdown <- margot_bind_tables(
  tables_list = tables_list, #list(all_models$combined_table),
  sort_E_val_bound = "desc",
  e_val_bound_threshold = 1.2, # ← choose threshold
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


# evaluate models ---------------------------------------------------------
# trim models if extreme propensity scores dominate
# diag_tbl_health_trim_98 <- margot_inspect_qini(models_binary_health,
#                                        propensity_bounds = c(0.01, 0.99))
# diag_tbl_health_trim_98
# 
# diag_tbl_health_trim_95 <- margot_inspect_qini(models_binary_health,
#                                       propensity_bounds = c(0.03, 0.97))
# diag_tbl_health_trim_95

# rescue qini if needed ---------------------------------------------------
# test
# diag_tbl_trim_all <- margot_inspect_qini(all_models,
#                                        propensity_bounds = c(0.03, 0.97))
# diag_tbl_trim_all




# flipping models: outcomes we want to minimise given the exposure --------
# standard negative outcomes/  not used in this study
flip_outcomes_standard = c(
  "t2_alcohol_frequency_weekly_z", 
  "t2_alcohol_intensity_z",
  "t2_hlth_bmi_z",
  "t2_hlth_fatigue_z",
  "t2_kessler_latent_anxiety_z",
  "t2_kessler_latent_depression_z",
  "t2_rumination_z"#,
  # "t2_perfectionism_z" # the exposure variable was not investigated
)

# we will investigate losses to these outcomes
# usual flipped names for positive interventions
# commented out for this study
# flipped_names <- c(
#   "Alcohol Frequency",
#   "Alcohol Intensity",
#   "BMI",
#   "Fatigue",
#   "Anxiety",
#   "Depression",
#   "Perfectionism",
#   "Rumination"
# )

# set diff for all outcomes to obtain vector of postive outcomes to reverse
flip_outcomes <- c( setdiff(t2_outcomes_all, flip_outcomes_standard) ) 

# check
flip_outcomes

# checks
neg_check <- vapply(all_models$results[ paste0("model_", flip_outcomes) ],
                    \(x) mean(x$tau_hat, na.rm = TRUE) < 0, logical(1))
stopifnot(all(neg_check))   # every chosen outcome has a negative mean CATE

# get labels
flipped_names <- margot_get_labels(flip_outcomes, label_mapping_all)

# check
flipped_names

# save for publication
here_save(flipped_names, "flipped_names")



# flip negatively oriented outcomes --------------------------------------

# flip models using margot's function
# this will take some time 
models_binary_flipped_all <- margot_flip_forests(
  all_models,
  flip_outcomes = flip_outcomes, #  ← select 
  recalc_policy = TRUE
)

# size
margot_size(models_binary_flipped_all)

# test
models_binary_flipped_all$results$model_t2_kessler_latent_depression_z$policy_tree_depth_1
models_binary_flipped_all$results$model_t2_kessler_latent_depression_z$policy_tree_depth_2



# omnibus heterogeneity tests --------------------------------------------
# test for treatment effect heterogeneity across all outcomes
result_ominbus_hetero_all <- margot::margot_omnibus_hetero_test(models_binary_flipped_all, label_mapping = label_mapping_all)

# view results table
result_ominbus_hetero_all$summary_table |> kbl("markdown")

# view test interpretation
cat(result_ominbus_hetero_all$brief_interpretation)

# rate test analysis -----------------------------------------------------
# define flipped outcome names for interpretation

# create rate analysis table
rate_table_all <- margot_rate(
  models = models_binary_flipped_all,  # <- the big results list
  policy        = "treat_best",            # or "withold_best"/ although do not attempt fitting curves or policytrees
  label_mapping = label_mapping_all
)
# view rate tables
rate_table_all$rate_autoc |> kbl("markdown")
rate_table_all$rate_qini |> kbl("markdown")

# rate_table_all_previous <- margot_rate(
#   models = models_binary_flipped_all,  
#   label_mapping = label_mapping_all
# )
# # view rate tables
# rate_table_all_previous$rate_autoc |> kbl("markdown")
# rate_table_all_previous$rate_qini |> kbl("markdown")


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
# autoc plots ------------------------------------------------------------
# generate batch rate plots for models with significant heterogeneity
batch_rate_autoc_plots <- margot_plot_rate_batch(
  models_binary_flipped_all, 
  save_plots = FALSE,
  # just use rate autoc
  model_names = rate_interpretation_all$autoc_model_names
)

# extract individual plots from the batch result
autoc_plots <- batch_rate_autoc_plots
# determine number of columns based on number of plots
num_cols <- ifelse(length(autoc_plots) > 3, 2, 1) # ← choose 

# combine plots using patchwork
library(patchwork)

# only proceed if there are plots to combine
if (length(autoc_plots) > 0) {
  # initialize with first plot
  combined_autoc_plot <- autoc_plots[[1]]
  
  # add remaining plots if any
  if (length(autoc_plots) > 1) {
    for (i in 2:length(autoc_plots)) {
      combined_autoc_plot <- combined_autoc_plot + autoc_plots[[i]]
    }
  }
  
  # apply the dynamic layout
  combined_autoc_plot <- combined_autoc_plot + 
    plot_layout(ncol = num_cols) &
    plot_annotation(
      title = "AUTOC Model Plots",
      subtitle = paste0(length(autoc_plots), " models with significant heterogeneity"),
      tag_levels = "A"
    )
  
  # view the combined plot
  print(combined_autoc_plot)
  
  # save the combined plot if needed
  width <- ifelse(num_cols == 1, 8, 12)
  height <- 6 * ceiling(length(autoc_plots)/num_cols)
  
  ggsave(here::here(push_mods, "combined_autoc_plots.pdf"), 
         combined_autoc_plot, 
         width = width, height = height)
} else {
  # handle case with no plots
  message("No AUTOC plots available")
}



# QINI --------------------------------------------------------------------
decision_tree_test_1L <- margot_plot_decision_tree(
  models_binary_flipped_all,
  model_name = "model_t2_kessler_latent_depression_z",
  max_depth  = 1L, # ← new argument
  original_df = original_df,
  label_mapping = label_mapping_all
)
decision_tree_test_1L

decision_tree_test_1L

# ignore workings
policy_tree_plots_1L <- models_binary_batch_qini <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$qini_model_names,
  max_depth  = 1L, # ← new argument
  original_df = original_df,
  label_mapping = label_mapping_all
)

# check size
margot_size(policy_tree_plots_1L)

# view
policy_tree_plots_1L[[1]][[3]]
policy_tree_plots_1L[[2]][[3]]



# 2 L workflow
policy_tree_plots_1L <- models_binary_batch_qini <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$qini_model_names,
  max_depth  = 1L, # ← new argument
  original_df = original_df,
  label_mapping = label_mapping_all
)

# view
policy_tree_plots_2L[[1]][[3]]

policy_tree_plots_2L[[2]][[3]]
margot_interpret_policy_batch

margot_interpret_policy_tree(models_binary_flipped_all,   
                             model_name = rate_interpretation_all$qini_model_names[[1]], 
                             original_df = original_df)

# interpretation ----------------------------------------------------------
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



# policy tree analysis ---------------------------------------------------
# make policy trees
plots_policy_trees <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$either_model_names, # defined above
  original_df = original_df,
  label_mapping = label_mapping_all, 
  max_depth = 2
)


# extract the policy tree plots for all models
# get model names
model_names <- rate_interpretation_all$either_model_names
model_names

# create a more organized structure for the plots
plot_collection <- list()

# for each model, collect all three plot types
for (i in seq_along(model_names)) {
  model_name <- model_names[[i]]
  
  # Create a list of the three plots for this model
  model_plots <- list(
    plot1 = plots_policy_trees[[i]][[1]],
    plot2 = plots_policy_trees[[i]][[2]],
    plot3 = plots_policy_trees[[i]][[3]]
  )
  
  # Add to the collection with model name as key
  plot_collection[[model_name]] <- model_plots
}

# example
plot_collection[[1]][[3]]


# view plots --------------------------------------------------------------
# model 1
plots_policy_trees[[1]][[3]]

# model 2
plots_policy_trees[[2]][[3]]

# yola

devtools::load_all("/Users/joseph/GIT/margot/")


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

complex_condition_age <- X[, "t0_age_z"] > -1 &
  X[, "t0_age_z"] < 1

# # if we have specific groups to compare
# complex_condition_age_under_neg_1_sd  <- X[, "t0_age_z"] < -1 
# complex_condition_age_gr_eq_neg_1_sd  <- X[, "t0_age_z"] > -1 

# check ages to get number
mean(original_df$t0_age) - sd(original_df$t0_age) 
mean(original_df$t0_age) + sd(original_df$t0_age) 


# wealth subsets
subsets_standard_wealth <- list(
  Poor = list(
    var = "t0_log_household_inc_z",
    value = -1,
    operator = "<",
    description = "Effects among those HShold income < -1 SD (NZD ~41k)",
    label = "Poor"  # label remains as is, but could be changed if desired
  ),
  MiddleIncome = list(
    subset_condition = complex_condition_wealth,
    description = "Effects among those HS_hold income within +/-1SD (> NZD 41k < NZD 191k)"
  ),
  Rich = list(
    var = "t0_log_household_inc_z",
    value = 1,
    operator = ">",
    description = "Effects among those HS_hold income > +1 SD (NZD 191k)",
    label = "Rich"
  )
)

# political subsets
subsets_standard_political <- list(
  Liberal = list(
    var = "t0_political_conservative_z",
    value = -1,
    operator = "<",
    description = "Effects among those < -1 SD in political conservativism",
    label = "Liberal"
  ),
  Centrist = list(
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
    description = "Effects among those > +1 SD in political conservativism",
    label = "Conservative"
  )
)


# political subsets
subsets_standard_age <- list(
  Younger = list(
    var = "t0_age_z",
    value = -1,
    operator = "<",
    description = "Effects among those < under 35 years old",
    label = "Age < 35"
  ),
  Middle = list(
    var = "t0_age_z",
    # operator = "<",
    subset_condition = complex_condition_age,
    description = "Effects among those 35-62",
    label = "Age 35-62"
  ),
  Older = list(
    var = "t0_political_conservative_z",
    value = 1,
    operator = ">",
    description = "Effects among those > 62",
    label = "Age > 62"
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


# batch planned subgroup analysis -----------------------------------------
# set up lists of models, names, and subtitles
domain_models <- list(
  models_binary_health,
  models_binary_psych,
  models_binary_present,
  models_binary_life,
  models_binary_social
)

str(domain_models, max.level=2)
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
subsets_standard_cohort
# set up subset types in a list
subset_types <- list(
  wealth = subsets_standard_wealth,
  ethnicity = subsets_standard_ethnicity,
  political = subsets_standard_political,
  gender = subsets_standard_gender,
  cohort = subsets_standard_age
)



planned_subset_results <- margot_planned_subgroups_batch(
  domain_models = domain_models,
  X = X,
  base_defaults = base_defaults_binary,
  subset_types = subset_types,
  original_df = original_df,
  domain_names = domain_names,
  subtitles = subtitles
)



planned_subset_results

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

planned_subset_results$health$wealth$results$Poor$transformed_table
# combine tables ----------------------------------------------------------
# wrap each domain's table in a list:
# wealth subgroups --------------------------------------------------------
tables_list_poor <- list(
  Health = planned_subset_results$health$wealth$results$Poor$transformed_table,
  Psych  = planned_subset_results$psych$wealth$results$Poor$transformed_table,
  Life   = planned_subset_results$life$wealth$results$Poor$transformed_table,
  Social = planned_subset_results$social$wealth$results$Poor$transformed_table
)

# function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_poor,
  bold = TRUE,
  kbl_args = list(booktabs = TRUE, caption = "Wealth Subgroup Analysis: Poor"),
  highlight_color = NULL, 
  output_format = "html" # could be "markdown"
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
planned_subset_results$health$
  planned_subset_results$health$cohort$results$Boomers$transformed_table
tables_list_younger <- list(
  Health = planned_subset_results$health$cohort$results$`Age < 35`$transformed_table,
  Psych  = planned_subset_results$psych$cohort$results$`Age < 35`$transformed_table,
  Life   = planned_subset_results$psych$cohort$results$`Age < 35`$transformed_table,
  Social = planned_subset_results$psych$cohort$results$`Age < 35`$transformed_table
)


# new function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_younger,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE,
    caption = "Cohort Subgroup Analysis: Age under 35"
  ),
  highlight_color = NULL,
  output_format = "html"
)


tables_list_middle <- list(
  Health = planned_subset_results$health$cohort$results$`Age 35-62`$transformed_table,
  Psych  = planned_subset_results$psych$cohort$results$`Age 35-62`$transformed_table,
  Life   = planned_subset_results$psych$cohort$results$`Age 35-62`$transformed_table,
  Social = planned_subset_results$psych$cohort$results$`Age 35-62`$transformed_table
)


#  function bind tables
#  function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_middle,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE, 
    caption = "Cohort Subgroup Analysis: Ages 35-62"
  ),
  highlight_color = NULL,
  output_format = "html"
)

tables_list_older <- list(
  Health = planned_subset_results$health$cohort$results$`Age > 62`$transformed_table,
  Psych  = planned_subset_results$psych$cohort$results$`Age > 62`$transformed_table,
  Life   = planned_subset_results$psych$cohort$results$`Age > 62`$transformed_table,
  Social = planned_subset_results$psych$cohort$results$`Age > 62`$transformed_table
)

# new function bind tables
margot::margot_bind_tables(
  tables_list = tables_list_older,
  bold = TRUE,
  kbl_args = list(
    booktabs = TRUE, 
    caption = "Cohort Subgroup Analysis: Age > 62"
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
    planned_subset_results$health$cohort$results$`Age < 35`$plot,
    planned_subset_results$health$cohort$results$`Age 35-62`$plot,
    planned_subset_results$health$cohort$results$`Age > 62`$plot
  ),
  ncol = 1
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
    planned_subset_results$health$psych$results$`Age < 35`$plot,
    planned_subset_results$health$psych$results$`Age 35-62`$plot,
    planned_subset_results$health$psych$results$`Age > 62`$plot
  ),
  ncol = 1
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
    planned_subset_results$present$cohort$results$`Age < 35`$plot,
    planned_subset_results$present$cohort$results$`Age 35-62`$plot,
    planned_subset_results$present$cohort$results$`Age > 62`$plot
  ),
  ncol = 1
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
    planned_subset_results$life$cohort$results$`Age < 35`$plot,
    planned_subset_results$life$cohort$results$`Age 35-62`$plot,
    planned_subset_results$life$cohort$results$`Age > 62`$plot
    
  ),
  ncol = 1
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