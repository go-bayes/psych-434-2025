# script 3: causal workflow for estimating average treatment effects using margot
# may 2025
# questions: joseph.bulbulia@vuw.ac.nz

# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+

# restart fresh session

rstudioapi::restartSession()


# reproducibility ---------------------------------------------------------

# choose number
set.seed(123)
seed = 123

# essential library ---------------------------------------------------------
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}

# min version of markgot
min_version <- "1.0.43"
if (packageVersion("margot") < min_version) {
  stop("please install margot >= min_version for this workflow\n
       run: devtools::install_github(\"go-bayes/margot\")
")
}

# call library
library("margot")

# check package version
packageVersion(pkg = "margot")



# load libraries ----------------------------------------------------------
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
  grf, ranger,     # machine learning forests
  doParallel,      # parallel processing,
  kableExtra,
  ggplot2 ,        # graphs
  rlang ,          # functions for base types/Core R/ 'Tidyverse'
  purrr ,          # functional programming tools.
  patchwork,      # nice graph placement
  janitor,         # nice labels
  glue,            # format/ interpolate a string
  cli,
  future,
  crayon,
  glue,
  stringr, 
  furrr
)



# directory path configuration -----------------------------------------------
# save path (customise for your own computer) ----------------------------
push_mods <- here::here("save_directory") 

# read original data (for plots) ------------------------------------------
original_df <- margot::here_read("df_wide", push_mods)

# plot title --------------------------------------------------------------
title_binary = "Effects of {{name_exposure}} on {{name_outcomes}}"
filename_prefix = "grf_extraversion_wb"

# for manuscript later
margot::here_save(title_binary,"title_binary")

# import names ------------------------------------------------------------
name_exposure <- margot::here_read("name_exposure")
name_exposure

# make exposure names
t1_name_exposure_binary <- paste0("t1_", name_exposure, "_binary")

# check exposure name
t1_name_exposure_binary

# read outcome vars
outcome_vars <- margot::here_read("outcome_vars")

# read and sort outcome variables -----------------------------------------
# we do this by domain: health, psych, present, life, social
read_and_sort <- function(key) {
  raw  <- margot::here_read(key, push_mods)
  vars <- paste0("t2_", raw, "_z")
  sort(vars)
}
t2_outcome_z  <- read_and_sort("outcome_vars")

# view
t2_outcome_z


# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+


# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+


# define names for titles -------------------------------------------------

nice_exposure_name =  stringr::str_to_sentence(name_exposure)
nice_outcome_name = "Wellbeing"
title = glue::glue("Effect of {nice_exposure_name} on {nice_outcome_name}")
title
# save for final rport
here_save(title, "title")

# combine outcomes ---------------------------------------------------------
# check outcome vars and make labels for graphs/tables
outcome_vars

label_mapping_all <- list(
  #"t2_alcohol_frequency_weekly_z" = "Alcohol Frequency",
  #"t2_alcohol_intensity_weekly_z" = "Alcohol Intensity",
  #"t2_hlth_bmi_z" = "BMI",
  #"t2_hlth_sleep_hours_z" = "Sleep",
  "t2_log_hours_exercise_z" = "Hours of Exercise (log)",
  #"t2_short_form_health_z" = "Short Form Health"
  "t2_hlth_fatigue_z" = "Fatigue",
  "t2_kessler_latent_anxiety_z" = "Anxiety",
  "t2_kessler_latent_depression_z" = "Depression",
  "t2_rumination_z" = "Rumination",
  # "t2_bodysat_z" = "Body Satisfaction",
  "t2_foregiveness_z" = "Forgiveness",
  "t2_perfectionism_z" = "Perfectionism", 
  "t2_self_esteem_z" = "Self Esteem",
  # "t2_self_control_z" = "Self Control",
  # "t2_sexual_satisfaction_z" = "Sexual Satisfaction".
  "t2_gratitude_z" = "Gratitude",
  "t2_lifesat_z" = "Life Satisfaction",
  "t2_meaning_purpose_z" = "Meaning: Purpose",
  "t2_meaning_sense_z" = "Meaning: Sense",
  "t2_pwi_z" = "Personal Well-being Index",
  "t2_belong_z" = "Social Belonging",
  "t2_neighbourhood_community_z" = "Neighbourhood Community",
  "t2_support_z" = "Social Support"
)


# save
here_save(label_mapping_all, "label_mapping_all")

# check
label_mapping_all

cli::cli_h1("created and saved label_mapping for use in graphs/tables ✔")


# make options -------------------------------------------------------------
# titles
ate_title = "ATE Effects of {{nice_name_exposure}} on {{nice_name_outcome}}"
subtitle = ""
filename_prefix = "final_report"
#
here_save(ate_title, "ate_title")
here_save(filename_prefix, "filename_prefix")

# settings
x_offset = -.5
x_lim_lo = -.5
x_lim_hi = .5


# defaults for ate plots
base_defaults_binary <- list(
  type = "RD",
  title = ate_title,
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

# save

# health graph options
outcomes_options_all <- margot_plot_create_options(
  title = subtitle,
  base_defaults = base_defaults_binary,
  subtitle = subtitle,
  filename_prefix = filename_prefix
)


# policy tree graph settings ----------------------------------------------
decision_tree_defaults <- list(
  span_ratio       = .3,
  text_size        = 3.8,
  y_padding        = 0.25,
  edge_label_offset = .002,
  border_size      = .05
)
policy_tree_defaults <- list(
  point_alpha       = .5,
  title_size        = 12,
  subtitle_size     = 12,
  axis_title_size   = 12,
  legend_title_size = 12,
  split_line_color  = "red",
  split_line_alpha  = .8,
  split_label_color = "red",
  list(split_label_nudge_factor = 0.007)
)

# +--------------------------+
# |   END MODIFY SECTION     |
# +--------------------------+

# +----------------------------------------------+
# |       DO NOT ALTER  (except where noted)     |
# +----------------------------------------------+

# load GRF data and prepare inputs ----------------------------------------
df_grf <- margot::here_read('df_grf', push_mods)
E      <- margot::here_read('E',      push_mods)
# check exposure binary
stopifnot(all(df_grf[[t1_name_exposure_binary]][!is.na(df_grf[[t1_name_exposure_binary]])] %in% 0:1))
# set exposure and weights

W       <- as.vector(df_grf[[t1_name_exposure_binary]]) # note it is the processed weights for attrition "t1"

# old workflow
# weights <- df_grf$t1_adjusted_weights

# new weights workflow, use "combo_weights" -- see revised script 2
weights<- df_grf$combo_weights

hist(weights) # quick check for extreme weights
# select covariates and drop numeric attributes
X <- margot::remove_numeric_attributes(df_grf[E])

# set model defaults -----------------------------------------------------
grf_defaults <- list(seed = 123, stabilize.splits = TRUE, num.trees = 2000)

# example: fit causal forest on a toy subset ------------------------------
# first, create a smaller test sample
n   <- nrow(X)
toy <- sample(seq_len(n), floor(n / 4))
# define toy data
toy_data     <- df_grf[toy, ]
X_toy        <- X[toy, ]
W_toy        <- W[toy]
weights_toy  <- weights[toy]


# **** IF THIS FAILS CHECK THAT YOU ARE USING VARIABLES FROM YOUR OUTCOME VARS **
# fit the model
cf_out <- margot_causal_forest_parallel( #<- new function ***
  data         = toy_data,
  # +--------------------------+
  # |    MODIFY THIS           | 
  # +--------------------------+
  outcome_vars = c(t2_outcome_z[[1]]), # select variable in your outcome_variable set
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  covariates   = X_toy,
  W            = W_toy,
  weights      = weights_toy,
  save_data    = TRUE,
  save_models  = TRUE
)


# inspect propensities ------------------------------------------------------
qini_tbl <- margot::margot_inspect_qini(cf_out, propensity_bounds = c(0.01, 0.97))

# plot policy-combo trees --------------------------------------------------
combo1 <- margot_plot_policy_combo(
  result_object    = cf_out,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_name       = paste0("model_", t2_outcome_z[[1]]),
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  max_depth        = 1L,          # depth-1 tree
  decision_tree_args = list(text_size = 4),
  policy_tree_args   = list(point_alpha = 0.7),
  original_df        = original_df,
  label_mapping      = label_mapping_all
)

# show
combo1$combined_plot

# you can repeat for depth-2 ----------------------------------------------
combo2 <- margot_plot_policy_combo(
  result_object    = cf_out,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_name       = paste0("model_", t2_outcome_z[[1]]),
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  max_depth        = 2L,
  decision_tree_args = decision_tree_defaults,
  policy_tree_args   = policy_tree_defaults,
  original_df        = original_df,
  label_mapping      = label_mapping_all
)

# show
combo2$combined_plot

# batch plotting ----------------------------------------------------------
models_batch_1L <- margot_policy(
  cf_out,
  save_plots         = FALSE,
  output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args   = policy_tree_defaults,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_names        = paste0("model_", t2_outcome_z[[1]]),
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  original_df        = original_df,
  label_mapping      = label_mapping_all,
  max_depth          = 1L
)

# view first model's plots
models_batch_1L[[1]][[3]]  # combo plot
models_batch_1L[[1]][[4]]  # qini plot

# sub plots
models_batch_1L[[1]][[1]]  # predictions of policy tree
models_batch_1L[[1]][[2]]  # policy tree

# qini interpretations at different spends
# negative is bad
models_batch_1L[[1]][[5]]  

# 2L tree
models_batch_2L <- margot_policy(
  cf_out,
  save_plots         = FALSE,
  output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args   = policy_tree_defaults,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_names        = paste0("model_", t2_outcome_z[[1]]),
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  original_df        = original_df,
  label_mapping      = label_mapping_all,
  max_depth          = 2L)
# view first model's plots
models_batch_2L[[1]][[3]]  # combo plot
models_batch_2L[[1]][[4]]  # qini plot - not convincing

# 2. flip the selected outcomes (and regen trees)
# use -- when the outcome is undesirable and we want to minimise it 
# (assuming the exposure is something we'd prescribe)

# select models whose outcomes are undesirable  when the intervention is meant to be 'good'
# such variables will be specific to your study 

# +--------------------------+
# |    MODIFY THIS           |
# +--------------------------+

flip_outcomes_test = paste0("model_", t2_outcome_z[[1]])

# function to get the labels from the models (labels were defined above)
flipped_names_test <- margot_get_labels(flip_outcomes_test, label_mapping_all)

# +--------------------------+
# |   END MODIFY             |
# +--------------------------+

# check size of objects
lobstr::obj_size(cf_out)                         # overall
purrr::map_dbl(cf_out, lobstr::obj_size)         # cf_out$results, cf_out$full_models, …

mdl <- cf_out$results[[1]]
purrr::map_dbl(mdl, lobstr::obj_size)            # tau_hat, dr_scores, forest, …

# run flip forests
cf_out_f <- margot_flip_forests_parallel( #<- NEW FUNCTION 
  model_results = cf_out,
  flip_outcomes = flip_outcomes_test,
  recalc_policy = TRUE,
  max_size_GB    = 15) # SET FOR YOUR COMPUTER


# where there are very low or high propensity scores (prob of exposure) 
# we might consider trimming
margot::margot_inspect_qini(cf_out_f, propensity_bounds = c(0.01, 0.97))


# if we had extreme scores (not used here)
# cf_out_flipped_trimmed <- margot_rescue_qini(model_results      = cf_out_f,
#                                              propensity_bounds  = c(0.05, 0.95)) 

# flipped batch model
models_batch_flipped_2L <- margot_policy(
  cf_out_f,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_names = paste0("model_", t2_outcome_z[[1]]),
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  original_df = original_df,
  label_mapping = label_mapping_all,
  max_depth     = 2L,
  output_objects = "combined_plot" # new parameter margot v1.0.39
)

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# flipped
# interpretation: exposure minimising depression
models_batch_flipped_2L[[1]]


# *** NOTE DIFFERENCES IN INTERPRETATION

# not flipped: exposure as maximizing depression
models_batch_2L[[1]][[3]]

# +--------------------------+
# |        END ALERT         |
# +--------------------------+
# interpretation example 


cli::cli_h1("created and saved label_mapping for use in graphs/tables ✔")


# +--------------------------+
# |          ALERT           |
# +--------------------------+
# select options that make sense fo your study/results
# might need to be tweaked after the analysis

# make options -------------------------------------------------------------

cli::cli_h1("testing on smaller dataset completed ✔")


# ** uncomment to run full model**

# causal forest model -----------------------------------------------------------

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# !!!! THIS WILL TAKE TIME  !!!!!
models_binary <- margot_causal_forest_parallel(
  data = df_grf,
  outcome_vars = t2_outcome_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  save_models = TRUE,
  save_data = TRUE,
  train_proportion = 0.7
)

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# !!!! THIS WILL TAKE TIME  !!!!!
# save model
margot::here_save_qs(models_binary, "models_binary", push_mods)
# +--------------------------+
# |        END ALERT         |
# +--------------------------+


cli::cli_h1("causal forest model completed and saved ✔")


# read results ------------------------------------------------------------
# if you save models you do not need to re-run them

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# reading models takes time
# if you want to check the size of an object use
# margot::margot_size(object)


# +--------------------------+
# |          ALERT           |
# +--------------------------+
# !!!! THIS WILL TAKE TIME  !!!!!
models_binary <- margot::here_read_qs("models_binary", push_mods)

# +--------------------------+
# |        END ALERT         |
# +--------------------------+

# count models by category
# just a check
cat("Number of original models:\n", length(models_binary$results), "\n")



# test function -----------------------------------------------------------



# make ate plots ----------------------------------------------------------

# uncorrected results
models_binary$combined_table

#   ************* NEW - CORRECTION FOR FAMILY-WISE ERROR **********

# then pass to the results
ate_results <- margot_plot(
  models_binary$combined_table, # <- now pass the corrected results.
  options = outcomes_options_all,
  label_mapping = label_mapping_all,
  include_coefficients = FALSE,
  save_output = FALSE,
  order = "evaluebound_asc",
  original_df = original_df,
  e_val_bound_threshold = 1.1,
  rename_ate = TRUE,
  adjust = "bonferroni", #<- new 
  alpha = 0.05 # <- new 
)




# view
cat(ate_results$interpretation)

# check
ate_results$plot

# interpretation
cat(ate_results$interpretation)

# save
here_save(ate_results, "ate_results")


# make markdown tables (to be imported into the manuscript)
margot_bind_tables_markdown <- margot_bind_tables(
  adj_tbl,
  #list(all_models$combined_table),
  sort_E_val_bound = "desc",
  e_val_bound_threshold = 1.1,
  # ← choose threshold
  highlight_color = NULL,
  bold = TRUE,
  rename_cols = TRUE,
  col_renames = list("E-Value" = "E_Value", "E-Value bound" = "E_Val_bound"),
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


# evaluate models ---------------------------------------------------------
# trim models if extreme propensity scores dominate
# diag_tbl_98 <- margot_inspect_qini(models_binary,
#                                        propensity_bounds = c(0.01, 0.99))




# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+



# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+


# FLIPPING OUTCOMES  ------------------------------------------------------

# note that the meaning of a heterogeneity will vary depending on our interests.
# typically we are interested in whether an exposure improves life, and whether there is variability (aka HTE) in degrees of improvement.
# in this case we must take negative outcomes and "flip" them -- recalculating the policy trees and qini curves for each
# for example if the outcome is depression, then by flipping depression we better understand how the exposure *reduces* depression. 
# what if the exposure is harmful? say what if we are interested in the effect of depression on wellbeing? In that case, we might
# want to "flip" the positive outcomes. That is, we might want to understand for whom a negative exposure is extra harmful. 
# here we imagine that extroversion is generally positive in its effects, and so we "flip" the negative outcomes. 
# if you were interested in a negative exposure, say "neuroticism" then you would probably want to flip the positive outcomes. 
# note there are further questions we might ask. We might consider who responds more 'weakly" to a negative exposure (or perhaps to a positive exposure).
# Such a question could make sense if we had an exposure that was generally very strong. 
# however, let's stay focussed on evaluating evaluating strong responders. We will flip the negative outcomes if we expect the exposure is positive,
# and flip the positive outcomes if we expect the exposure to be generally negative. 
# if there is no natural "positive" or negative, then just make sure the valence of the outcomes aligns, so that all are oriented in the same 
# direction if they have a valence.  if unsure, just ask for help!

# flipping models: outcomes we want to minimise given the exposure --------
# standard negative outcomes/  not used in this example
# flipping models: outcomes we want to minimise given the exposure --------
# standard negative outcomes/  not used in this example
# +--------------------------+
# |    MODIFY THIS           |
# +--------------------------+

# WHICH OUTCOMES -- if any ARE UNDESIREABLE? 
flip_outcomes_standard = c(
  #"t2_alcohol_frequency_weekly_z",
  #"t2_alcohol_intensity_z",
  #"t2_hlth_bmi_z",
  #"t2_hlth_fatigue_z",
  "t2_kessler_latent_anxiety_z", #  ← select
  "t2_kessler_latent_depression_z",#  ← select
  "t2_rumination_z" #  ← select
  #"t2_perfectionism_z" # the exposure variable was not investigated
)

# when exposure is negative and you want to focus on how much worse off
# some people are use this: 

# NOT IF THE EXPOSURE IS NEGATIVE, FOCUS ON WHICH OUTCOMES, if any, ARE POSITIVE AND FLIP THESE?
# flip_outcomes<- c( setdiff(t2_outcomes_all, flip_outcomes_standard) )

# our example has the exposure as positive
flip_outcomes <- flip_outcomes_standard

# check
flip_outcomes


# +--------------------------+
# |   END MODIFY             |
# +--------------------------+

# checks for when exposure is *damaging** 
# neg_check <- vapply(all_models$results[ paste0("model_", flip_outcomes) ],
#                     \(x) mean(x$tau_hat, na.rm = TRUE) < 0, logical(1))
# stopifnot(all(neg_check))   # every chosen outcome has a negative mean cate

# get labels
flipped_names <- margot_get_labels(flip_outcomes, label_mapping_all)

# check
flipped_names

# save for publication
here_save(flipped_names, "flipped_names")

cli::cli_h1("flipped outcomes identified and names saved ✔")


# flip negatively oriented outcomes --------------------------------------

# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+


# flip models using margot's function

#  *** this will take some time ***

# ** give it time **
# ** once run/ comment out **

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# !!!! THIS WILL TAKE TIME  !!!!!
models_binary_flipped_all <- margot_flip_forests_parallel(models_binary,
                                                 flip_outcomes = flip_outcomes_standard,
                                                 recalc_policy = TRUE,
                                                 max_size_GB = 32)

cli::cli_h1("flipped forest models completed ✔")

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# !!!! THIS WILL TAKE TIME  !!!!!
# save
here_save_qs(models_binary_flipped_all, "models_binary_flipped_all", push_mods)

# +--------------------------+
# |        END ALERT         |
# +--------------------------+


# +--------------------------+
# |          ALERT           |
# +--------------------------+
# !!!! THIS WILL TAKE TIME  !!!!!
# read back if needed
models_binary_flipped_all <- here_read_qs("models_binary_flipped_all", push_mods)
# +--------------------------+
# |        END ALERT         |
# +--------------------------+

# where there are very low or high propensity scores (prob of exposure) we might consider trimming
# margot::margot_inspect_qini(models_binary_flipped_all, propensity_bounds = c(0.05, 0.95))
# 
# 
# # if we had extreme scores (not used here)
# models_binary_flipped_all_t <- margot_rescue_qini(model_results  = models_binary_flipped_all,
#                                              propensity_bounds  = c(0.05, 0.95))

# heterogeneity and policy analysis workflow -----------------------------
# step 1: omnibus heterogeneity test ---------------------------------------
# test for treatment effect heterogeneity across all flipped outcomes
#	omnibus calibration test asks: 'Can the forest rank units by τ(x) at all?'  a null result means the ordering of CATEs is 
# indistinguishable from noise, not that all HTE is zero.
omnibus_results <-
  margot::margot_omnibus_hetero_test(
    models_binary_flipped_all,
    label_mapping = label_mapping_all
  )

# display summary and interpretation
omnibus_results$summary_table %>% kbl("markdown")
cat(omnibus_results$brief_interpretation, "\n")

# -------------------------------------------------------------------
# 1. screen outcomes for evidence of heterogeneity 
# ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––-
keep <- margot_screen_models(
  models_binary_flipped_all,  # full margot object with $results
  rule   = "rate",           # heterogeneity evidence from rate tests
  target = "either",         # either RATE‑AUTOC or RATE‑Qini may pass
  alpha  = 0.10,             # raw p < 0.10 is enough to keep
  adjust = "BH"              # <‑‑correction
)

# view
print(keep)

# save
here_save(keep, "keep")
models_binary_flipped_all$results$model_t2_kessler_latent_anxiety_z$rate_qini

keep_autoc <- margot_screen_models(
  models_binary_flipped_all,  # full margot object with $results
  rule   = "rate",           # heterogeneity evidence from rate tests
  target = "either",         # either RATE‑AUTOC or RATE‑Qini may pass
  alpha  = 0.10,             # raw p < 0.10 is enough to keep
  adjust = "BH"              # <‑‑correction
)

# view
print(keep_autoc)

# save for manuscript
here_save(keep_autoc, "keep_autoc")

keep_qini <- margot_screen_models(
  models_binary_flipped_all,  # full margot object with $results
  rule   = "rate",           # heterogeneity evidence from rate tests
  target = "QINI",         # either RATE‑AUTOC or RATE‑Qini may pass
  alpha  = 0.10,             # raw p < 0.10 is enough to keep
  adjust = "BH"              # <‑‑correction
)

# view
print(keep_qini)


# save for manuscript
here_save(keep_qini, "keep_qini")

model_keep <- paste0("model_", keep)

devtools::load_all("/Users/joseph/GIT/margot/")

# step 2: compute rate heterogeneity metrics ------------------------------
# calculate rate-autoc and rate-qini tables
# note: RATE-AUTOC asks: 'If I only treat the top k\% by τ(x), do I maximise average gain?' 
# it rewards extreme uplift but can be volatile.
# RATE-Qini asks: 'if i treat more broadly, do I still improve aggregate outcome?' it trades intensity for coverage.
rate_results <-
  margot_rate(
    models   = models_binary_flipped_all,
  #  model_names        = model_keep,     # only models that survived bh
    policy   = "treat_best",
    alpha  = 0.20,             # raw p < 0.10 is enough to keep
    adjust = "fdr",              # <‑‑correction
    label_mapping = label_mapping_all
  )

# show rate tables
rate_results$rate_autoc %>% kbl("markdown")
rate_results$rate_qini %>% kbl("markdown")

# save rate results
here_save(rate_results, "rate_results")

# generate textual interpretations for rate metrics
rate_interp <-
  margot_interpret_rate(
    rate_results,
    flipped_outcomes = flipped_names
  )
cat(rate_interp$autoc_results, "\n")
cat(rate_interp$qini_results, "\n")
cat(rate_interp$comparison, "\n")

here_save(rate_interp, "rate_interp")

# organise model groups by heterogeneity evidence
model_groups <- list(
  autoc  = rate_interp$autoc_model_names,
  qini   = rate_interp$qini_model_names,
  either = rate_interp$either_model_names
)

cli::cli_h1("rate metrics and interpretations complete ✔")

# helper: combine and save ggplot objects ---------------------------------
combine_and_save <- function(plots, prefix) {
  if (length(plots) == 0) {
    message("no ", prefix, " plots to combine")
    return(invisible(NULL))
  }
  cols     <- ifelse(length(plots) > 3, 2, 1)
  combined <- purrr::reduce(plots, `+`) +
    patchwork::plot_layout(ncol = cols) &
    patchwork::plot_annotation(
      title    = toupper(prefix),
      subtitle = glue::glue("{length(plots)} models"),
      tag_levels = "A"
    )
  print(combined)
  ggsave(
    here::here(push_mods, paste0("combined_", prefix, ".pdf")),
    combined,
    width  = ifelse(cols == 1, 8, 12),
    height = 6 * ceiling(length(plots) / cols)
  )
  combined
}

# step 3: plot rate curves -----------------------------------------------
autoc_plots <-
  margot_plot_rate_batch(
    models      = models_binary_flipped_all,
    save_plots  = FALSE,
    label_mapping      = label_mapping_all,
    model_names = model_groups$autoc
  )
autoc_plots$model_t2_log_hours_exercise_z

combined_autoc <- combine_and_save(autoc_plots, "rate_autoc")

qini_rate_plots <-
  margot_plot_rate_batch(
    models      = models_binary_flipped_all,
    save_plots  = FALSE,
    model_names = model_groups$qini,
    label_mapping      = label_mapping_all
  )

qini_rate_plots$model_t2_meaning_sense_z

combined_qini_rate <- combine_and_save(qini_rate_plots, "rate_qini")
cli::cli_h1("rate curves plotted ✔")

# view 
autoc_plots$model_t2_log_hours_exercise_z

# step 4: Qini model curves --------------------------------------------
qini_policy_results <-
  margot_policy(
    models_binary_flipped_all,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = list(),
    policy_tree_args   = list(),
    model_names        = model_groups$qini,
    original_df        = original_df,
    label_mapping      = label_mapping_all,
    max_depth          = 2L,
    output_objects     = c("qini_plot", "diff_gain_summaries")
  )

# view qini plots
qini_plots <- purrr::map(qini_policy_results, ~ .x$qini_plot)
qini_plots

cli::cli_h1("Qini model curves plotted ✔")

# step 5: main focus – 2-level policy trees -------------------------------
# policy value (our 2-level tree on the 30 % test fold) asks: 'under a low-complexity rule, do treated units in the selected leaves really outperform?'
policy_2L <-
  margot_policy(
    models_binary_flipped_all,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = decision_tree_defaults,
    policy_tree_args   = policy_tree_defaults,
    model_names        = model_groups$either,
    original_df        = original_df,
    label_mapping      = label_mapping_all,
    max_depth          = 2L,
    output_objects     = "combined_plot"
  )

# extract and display 2L policy tree plots
plots_2L <- purrr::map(policy_2L, ~ .x[[1]])
purrr::walk(plots_2L, print)

# interpret 2L policy trees
interp_2L <-
  margot_interpret_policy_batch(
    models_binary_flipped_all,
    model_names = model_groups$either
  )
cat(interp_2L, "\n")

cli::cli_h1("main 2L policy trees analysed ✔")

# step 6: secondary focus – qualitative assessment of policy trees ----
policy_1L <-
  margot_policy(
    models_binary_flipped_all,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = decision_tree_defaults,
    policy_tree_args   = policy_tree_defaults,
    model_names        = model_groups$either,
    original_df        = original_df,
    label_mapping      = label_mapping_all,
    max_depth          = 1L,
    output_objects     = "combined_plot"
  )
plots_1L <- purrr::map(policy_1L, ~ .x[[1]])

cli::cli_h1("secondary 1L policy trees reviewed ✔")

# view
plots_1L$model_t2_log_hours_exercise_z
plots_1L$model_t2_meaning_sense_z

# step 7: secondary focus – 2-level policy trees -------------------------
# policies fror all outcomes
policy_all_2L <-
  margot_policy(
    models_binary_flipped_all,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = decision_tree_defaults,
    policy_tree_args   = policy_tree_defaults,
    # model_names        = model_groups$either,
    original_df        = original_df,
    label_mapping      = label_mapping_all,
    max_depth          = 2L,
    output_objects     = "combined_plot"
  )
# extract and display 2L policy tree plots
plots_all_2L <- purrr::map(policy_all_2L, ~ .x[[1]])
purrr::walk(plots_all_2L, print).

# view
plots_all_2L$model_t2_log_hours_exercise_z
plots_all_2L$model_t2_meaning_sense_z

# others -- not reliable, only exploratory
plots_all_2L$model_t2_belong_z # etc


# interpret 2L policy trees
interp_all_2L <-
  margot_interpret_policy_batch(
    models_binary_flipped_all,
    model_names = model_groups$either
  )
cat(interp_all_2L, "\n")

cli::cli_h1("main 2L policy trees analysed ✔")
plots_all_2L$model_t2_belong_z



# step 8 BONFERRONI CORRECTION TO WORKFLOW ---------------------------------------
#–– 1·a  (already produced by margot_flip_forests) ––––––––––––––––––––––
flipped <- models_binary_flipped_all          # <- our object

#–– 1·b  outcome-level screening ––––––––––––––––––––––––––––––––––––––––
#  ➜ keep outcomes whose ATE 95 % CI excludes 0   **OR**
#    whose AUTOC RATE CI excludes 0  (choose your favourite rule)

keep <- margot_screen_models(
  flipped,
  rule = "rate",        # <– implementation-specific
  target = "either",            # <– or "QINI"
  alpha = 0.05,
  adjust =  "bonferroni"
)


# get models
model_keep  <- paste0("model_", keep)

# view
model_keep

# corrected policies
policy_2L_corrected <-
  margot_policy(
    models_binary_flipped_all,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = decision_tree_defaults,
    policy_tree_args   = policy_tree_defaults,
    model_names        = model_keep,
    original_df        = original_df,
    label_mapping      = label_mapping_all,
    max_depth          = 2L,
    output_objects     = "combined_plot"
  )
# extract and display 2L policy tree plots
plots_2L_corrected <- purrr::map(policy_2L_corrected, ~ .x[[1]])
purrr::walk(plots_2L_corrected, print)

# view
plots_2L_corrected$model_t2_neighbourhood_community_z


# others -- not reliable, only exploratory
plots_all_2L$model_t2_belong_z # etc


# interpret 2L policy trees
interp_all_2L <-
  margot_interpret_policy_batch(models_binary_flipped_all, model_names = model_groups$either)
cat(interp_all_2L, "\n")

cli::cli_h1("main 2L policy trees analysed ✔")
plots_all_2L$model_t2_belong_z

# view
models_binary_flipped_all$results$model_t2_kessler_latent_anxiety_z$policy_tree_depth_2



# IGNORE - DEVELOPING
# #–– 2·b  loop over the *kept* outcomes only ––––––––––––––––––––––––––––
# developing
# flipped$results[paste0("model_", keep)] <-
#   purrr::map(flipped$results[paste0("model_", keep)],
#              add_policy_p,
#              depth = 2,
#              R     = 999,
#              seed  = 2025)
# 
# 
# # view
# flipped$results$model_t2_belong_z$policy_value
# 
# 
# #–– 3·a  assemble every metric + Bonferroni adjustment ––––––––––––––––––
# res_tbl  <- margot_summarise_all(flipped, target = "both", adjust = "none")
# dplyr::glimpse(res_tbl)
# 
# # focus on the screened models only
# res_tbl <- dplyr::filter(res_tbl, outcome %in% keep) |>
#   dplyr::mutate(
#     pv_est = purrr::map_dbl(outcome,
#                             ~ flipped$results[[paste0("model_", .x)]]$policy_value$estimate),
#     pv_se  = purrr::map_dbl(outcome,
#                             ~ flipped$results[[paste0("model_", .x)]]$policy_value$std.err)
#   )
# 
# #–– 3·b  prettify & print –––––––––––––––––––––––––––––––––––––––––––––––
# report <- res_tbl %>%
#   transmute(
#     Outcome        = tools::toTitleCase(gsub("_z$", "", sub("^t2_", "", outcome))),
#     `ATE  (±SE)`   = sprintf("%.3f ± %.3f", ate_est,  ate_se),
#     `RATE AUTOC`   = sprintf("%.3f [%.3f, %.3f]",
#                              rate_autoc, rate_autoc_lo, rate_autoc_hi),
#     `RATE QINI`    = sprintf("%.3f [%.3f, %.3f]",
#                              rate_qini,  rate_qini_lo,  rate_qini_hi),
#     `Policy Δ (±SE)` = sprintf("%.3f ± %.3f", pv_est, pv_se),
#     `Policy p`     = sprintf("%.3f", policy_p),
#     `p-adj`        = sprintf("%.3f", p_adj),
#     `Sig FW(.05)`  = ifelse(pass, "✓", "✗")
#   )
# knitr::kable(report, caption = "Treatment-effect & policy results (Bonferroni corrected)")




# PLANNED COMPARISONS -----------------------------------------------------



# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+



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
  MiddleIncome = list(subset_condition = complex_condition_wealth, description = "Effects among those HS_hold income within +/-1SD (> NZD 41k < NZD 191k)"),
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
    var = "t0_age_z",
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
  models_binary # HERE WE USE THE ORIGINAL MODELS
)


# set up domain names
domain_names <- c("wellbeing")

# set up subtitles
subtitles <- ""

# set up subset types in a list
subset_types <- list(
  wealth = subsets_standard_wealth,
  ethnicity = subsets_standard_ethnicity,
  political = subsets_standard_political,
  gender = subsets_standard_gender,
  cohort = subsets_standard_age
)


# run model
planned_subset_results <- margot_planned_subgroups_batch(
  domain_models = domain_models,
  X = X,
  base_defaults = base_defaults_binary,
  subset_types = subset_types,
  original_df = original_df,
  domain_names = domain_names,
  subtitles = subtitles
)


# results
cat(planned_subset_results$wellbeing$wealth$explanation)
cat(planned_subset_results$wellbeing$ethnicity$explanation)
cat(planned_subset_results$wellbeing$political$explanation)
cat(planned_subset_results$wellbeing$gender$explanation)
cat(planned_subset_results$wellbeing$cohort$explanation)



# cohort subgroups --------------------------------------------------------

# plots -------------------------------------------------------------------
# results plots
# health
plots_subgroup_wealth<- wrap_plots(
  list(
    planned_subset_results$wellbeing$wealth$results$Poor$plot,
    planned_subset_results$wellbeing$wealth$results$MiddleIncome$plot,
    planned_subset_results$wellbeing$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(title = "Wealth",
                             theme = theme(plot.title = element_text(size = 18, face = "bold")))

# view
plots_subgroup_wealth

# plots
plots_subgroup_ethnicity <- wrap_plots(
  list(
    planned_subset_results$wellbeing$ethnicity$results$Asian$plot,
    planned_subset_results$wellbeing$ethnicity$results$Euro$plot,
    planned_subset_results$wellbeing$ethnicity$results$Pacific$plot,
    planned_subset_results$wellbeing$ethnicity$results$Maori$plot
    
  ),
  ncol = 2
) +
  patchwork::plot_annotation(title = "Ethnicity",
                             theme = theme(plot.title = element_text(size = 18, face = "bold")))

# view
print(plots_subgroup_ethnicity)

# plots
plots_subgroup_political <- wrap_plots(
  list(
    planned_subset_results$wellbeing$political$results$Liberal$plot,
    planned_subset_results$wellbeing$political$results$Centrist$plot,
    planned_subset_results$wellbeing$political$results$Conservative$plot  
  ),
  ncol = 1
) +
  patchwork::plot_annotation(title = "Political Orientation",
                             theme = theme(plot.title = element_text(size = 18, face = "bold")))

# view
print(plots_subgroup_political)

# plots
plots_subgroup_gender <- wrap_plots(
  list(
    planned_subset_results$wellbeing$gender$results$Female$plot,
    planned_subset_results$wellbeing$gender$results$Male$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(title = "Gender",
                             theme = theme(plot.title = element_text(size = 18, face = "bold")))

# view
print(plots_subgroup_gender)

# plots
plots_subgroup_cohort <- wrap_plots(
  list(
    planned_subset_results$wellbeing$cohort$results$`Age < 35`$plot,
    planned_subset_results$wellbeing$cohort$results$`Age 35-62`$plot,
    planned_subset_results$wellbeing$cohort$results$`Age > 62`$plot
  ),
  ncol = 1
) +
  patchwork::plot_annotation(title = "Age Cohorts",
                             theme = theme(plot.title = element_text(size = 18, face = "bold")))

# view
print(plots_subgroup_cohort)




# COMPARE GROUPS ----------------------------------------------------------
devtools::load_all("/Users/joseph/GIT/margot/")

planned_subset_results$wellbeing$cohort$results$`Age < 35`$transformed_table
planned_subset_results$wellbeing$cohort$results$`Age > 62`$transformed_table

group_comparison_age_young_old <- margot_compare_groups(
  planned_subset_results$wellbeing$cohort$results$`Age < 35`$transformed_table, # reference
  planned_subset_results$wellbeing$cohort$results$`Age > 62`$transformed_table, # comparison
  type = "RD",
  label_mapping = NULL,
  decimal_places = 4
)
group_comparison_age_young_old$results |> kbl("markdown", digits = 2)
group_comparison_age_young_old$interpretation



# results
# compare another group ---------------------------------------------------

planned_subset_results$wellbeing$cohort$results$`Age < 35`$transformed_table

group_comparison_ethn_euro_maori <- margot_compare_groups(
  planned_subset_results$wellbeing$ethnicity$results$Euro$transformed_table, # reference
  planned_subset_results$wellbeing$ethnicity$results$Maori$transformed_table, # comparison
  type = "RD",
  label_mapping = NULL,
  decimal_places = 4
)

# results
group_comparison_ethn_euro_maori$results # no differences
group_comparison_ethn_euro_maori$interpretation # no differences


# compare another group ---------------------------------------------------
# gender
group_comparison_gender<- margot_compare_groups(
  planned_subset_results$wellbeing$gender$results$Female$transformed_table, # reference
  planned_subset_results$wellbeing$gender$results$Male$transformed_table, # comparison
  type = "RD",
  label_mapping = NULL,
  decimal_places = 4
)

# results
group_comparison_gender$results # no reliable differences
group_comparison_gender$interpretation # no differences


# TAKE HOME
# first these are simulated data, so true effects are attenuated
# second, when we allow our models to be flexible, we find that pre-defined group comparisons often come up short


# plot options: showcased ---------------------------------------------
# default
margot_plot_decision_tree(models_binary, "model_t2_support_z", )
# tighten branches for easier viewing in single graphs
margot::margot_plot_decision_tree(
  models_binary,
  "model_t2_support_z",
  span_ratio = .30,
  text_size = 3.8,
  border_size = .1,
  #  title = "none",
  original_df = original_df
)
# colour decision node
margot::margot_plot_decision_tree(
  models_binary,
  "model_t2_support_z",
  span_ratio = .3,
  text_size = 4,
  title = "New Title",
  non_leaf_fill =  "violet",
  original_df = original_df
)
# make new title
margot::margot_plot_decision_tree(
  models_binary,
  "model_t2_support_z",
  span_ratio = .2,
  text_size = 3,
  title = "New Title",
  non_leaf_fill =  "white",
  original_df = original_df
)

# remove title
margot::margot_plot_decision_tree(
  models_binary,
  "model_t2_support_z",
  text_size = 5,
  title = 'none',
  # set title to none
  original_df = original_df
)


# adjust only the alpha
margot::margot_plot_policy_tree(models_binary, "model_t2_support_z", point_alpha = .1)
margot::margot_plot_policy_tree(models_binary, "model_t2_support_z", point_alpha = .9)
