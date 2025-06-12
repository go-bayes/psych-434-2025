# script 3: causal workflow for estimating average treatment effects using margot
# may 2025
# questions: joseph.bulbulia@vuw.ac.nz

# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+

# restart fresh session

# rstudioapi::restartSession()


# reproducibility ---------------------------------------------------------

# choose number
set.seed(123)
seed = 123

# essential library ---------------------------------------------------------
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}

# min version of margot
if (packageVersion("margot") < "1.0.52") {
  stop(
    "please install margot >= 1.0.52 for this workflow\n
       run: devtools::install_github(\"go-bayes/margot\")
"
  )
}

# call library
library("margot")

# check package version
packageVersion(pkg = "margot")


# load libraries ----------------------------------------------------------
# pacman will install missing packages automatically
pacman::p_load(
  tidyverse,
  # data wrangling + plotting
  qs,
  here,# project-relative file paths
  data.table,# fast data manipulation
  fastDummies,# dummy variable creation
  naniar,# missing data handling
  skimr,# summary statistics
  grf,
  ranger,
  doParallel,
  kableExtra,
  ggplot2 ,
  rlang ,
  purrr ,
  patchwork,
  janitor,  # nice labels
  glue,
  cli,
  future,
  crayon,
  glue,
  stringr,
  future,
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
margot::here_save(title_binary, "title_binary")

# import names ------------------------------------------------------------
name_exposure <- margot::here_read("name_exposure")
name_exposure

# make exposure names
t1_name_exposure_binary <- paste0("t1_", name_exposure)

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
x_offset = -.25
x_lim_lo = -.25
x_lim_hi = .25


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
  text_size = 8,
  linewidth = 0.75,
  estimate_scale = 1,
  base_size = 18,
  point_size = 4,
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
E      <- margot::here_read('E', push_mods)

# check exposure binary
stopifnot(all(df_grf[[t1_name_exposure_binary]][!is.na(df_grf[[t1_name_exposure_binary]])] %in% 0:1))
# set exposure and weights

W       <- as.vector(df_grf[[t1_name_exposure_binary]]) # note it is the processed weights for attrition "t1"

# old workflow
# weights <- df_grf$t1_adjusted_weights

# new weights workflow, use "combo_weights" -- see revised script 2
weights <- df_grf$combo_weights

hist(weights) # quick check for extreme weights
# select covariates and drop numeric attributes
X <- margot::remove_numeric_attributes(df_grf[E])

# set model defaults -----------------------------------------------------
grf_defaults <- list(seed = 123,
                     stabilize.splits = TRUE,
                     num.trees = 2000)


# causal forest model -----------------------------------------------------------

# +--------------------------+
# |          ALERT           |
# +--------------------------+






# !!!! THIS WILL TAKE TIME  !!!!!
# **----- COMMENT OUT AFTER YOU RUN TO AVOID RUNNING MORE THAN ONCE -----**

t2_outcome_z
X
models_binary <- margot_causal_forest(
  # <- could be 'margot_causal_forest_parrallel()' if you have a powerful computer
  data = df_grf,
  outcome_vars = t2_outcome_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  #<- can be modified but will affect run times
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
# !!!! THIS WILL TAKE TIME  !!!!!
models_binary <- margot::here_read_qs("models_binary", push_mods)

# +--------------------------+
# |        END ALERT         |
# +--------------------------+

# count models by category
# just a check
cat("Number of original models:\n",
    length(models_binary$results),
    "\n")

# make ate plots ----------------------------------------------------------
#   ************* NEW - CORRECTION FOR FAMILY-WISE ERROR **********
# then pass to the results
ate_results <- margot_plot(
  models_binary$combined_table,
  # <- now pass the corrected results.
  options = outcomes_options_all,
  label_mapping = label_mapping_all,
  include_coefficients = FALSE,
  save_output = FALSE,
  order = "evaluebound_asc",
  original_df = original_df,
  e_val_bound_threshold = 1.2,
  rename_ate = TRUE,
  adjust = "bonferroni",  #<- new
  alpha = 0.05 # <- new
)


# view
cat(ate_results$interpretation)

# check
ate_results$plot

# interpretation
cat(ate_results$interpretation)

# save
here_save_qs(ate_results, "ate_results", push_mods)


# make markdown tables (to be imported into the manuscript)
margot_bind_tables_markdown <- margot_bind_tables(
  ate_results$transformed_table,
  #list(all_models$combined_table),
  sort_E_val_bound = "desc",
  e_val_bound_threshold = 1.2,
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
t2_outcome_z
# WHICH OUTCOMES -- if any ARE UNDESIREABLE?
flip_outcomes_standard = c(
  t2_outcome_z
)

# when exposure is negative and you want to focus on how much worse off

# NOTE IF THE EXPOSURE IS NEGATIVE, FOCUS ON WHICH OUTCOMES, if any, ARE POSITIVE AND FLIP THESE?
# flip_outcomes<- c( setdiff(t2_outcomes_all, flip_outcomes_standard) )

# our example has the exposure as positive
flip_outcomes <- flip_outcomes_standard

# save
here_save(flip_outcomes, "flip_outcomes")

# check
flip_outcomes
label_mapping_all


# +--------------------------+
# |   END MODIFY             |
# +--------------------------+

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
# can be margot_flip_forests_parrallel() if you have sufficient compute, set GB = something less than your system RAM
models_binary_flipped_all <- margot_flip_forests(models_binary,
                                                 flip_outcomes = flip_outcomes_standard,
                                                 recalc_policy = TRUE)

cli::cli_h1("flipped forest models completed ✔")
# !!!! THIS WILL TAKE TIME  !!!!!
# save
here_save_qs(models_binary_flipped_all,
             "models_binary_flipped_all",
             push_mods)


# +--------------------------+
# |          ALERT           |
# +--------------------------+
# !!!! THIS WILL TAKE TIME  !!!!!
# read back if needed
models_binary_flipped_all <- here_read_qs("models_binary_flipped_all", push_mods)

flip_outcomes

# this is a new function requires margot 1.0.48 or higher
label_mapping_all_flipped <- margot_reversed_labels(label_mapping_all, flip_outcomes)

# view
label_mapping_all_flipped

# save flipped labels
here_save(label_mapping_all_flipped, "label_mapping_all_flipped")

# +--------------------------+
# |        END ALERT         |
# +--------------------------+


# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+


# ──────────────────────────────────────────────────────────────────────────────
# SCRIPT:  HETEROGENEITY WORKFLOW
# PURPOSE: screen outcomes for heterogeneity, plot RATE & Qini curves,
#          fit shallow policy trees, and produce plain-language summaries.
# REQUIREMENTS:
#   • margot ≥ 1.0.52
#   • models_binary_flipped_all        – list returned by margot_causal_forest()
#   • original_df                      – raw data frame used in the forest
#   • label_mapping_all_flipped        – named vector of pretty labels
#   • flipped_names                    – vector of outcomes that were flipped
#   • decision_tree_defaults           – list of control parameters
#   • policy_tree_defaults             – list of control parameters
#   • push_mods                        – sub-folder for caches/outputs
#   • use 'models_binary', `label_mapping_all`, and set `flipped_names = ""` if no outcome flipped
# ──────────────────────────────────────────────────────────────────────────────

# check package version early
stopifnot(utils::packageVersion("margot") >= "1.0.52")


policy_tree_defaults <- list(
  point_alpha               = 0.5,
  title_size               = 12,
  subtitle_size            = 12,
  axis_title_size          = 12,
  legend_title_size        = 12,
  split_line_color         = "red",
  split_line_alpha         = 0.8,
  split_label_color        = "red",
  split_label_nudge_factor = 0.007  # ← moved out of nested list
)


# helper: quick kable printer --------------------------------------------------
print_rate <- function(tbl) {
  tbl |>
    mutate(across(where(is.numeric), \(x) round(x, 2))) |>
    kbl(format = "markdown")
}

# 1  SCREEN FOR HETEROGENEITY (RATE AUTOC + RATE Qini)  ----------------------
devtools::load_all("/Users/joseph/GIT/margot/")

rate_results <- margot_rate(
  models        = models_binary_flipped_all,
  policy        = "treat_best",
  alpha         = 0.20,        # keep raw p < .20
  adjust        = "fdr",       # false-discovery-rate correction
  label_mapping = label_mapping_all_flipped
)

print_rate(rate_results$rate_autoc)
print_rate(rate_results$rate_qini)
# convert RATE numbers into plain-language text
rate_interp <- margot_interpret_rate(
  rate_results,
  flipped_outcomes      = flipped_names,
  adjust_positives_only = TRUE
)

cat(rate_interp$comparison, "\n")
cli_h2("Analysis ready for Appendix ✔")

# organise model names by evidence strength
model_groups <- list(
  autoc       = rate_interp$autoc_model_names,
  qini        = rate_interp$qini_model_names,
  either      = rate_interp$either_model_names,
  exploratory = rate_interp$not_excluded_either
)

# 2  PLOT RATE AUTOC CURVES ---------------------------------------------------

autoc_plots <- margot_plot_rate_batch(
  models        = models_binary_flipped_all,
  save_plots    = FALSE,  # set TRUE to store .png files
  label_mapping = label_mapping_all_flipped,
  model_names   = model_groups$autoc
)

# inspect the first curve - note there may be more/none.
# if none, comment out
autoc_plots[[1]]
autoc_name_1 <- rate_results$rate_autoc$outcome[[1]]

# 3  QINI CURVES + GAIN INTERPRETATION ---------------------------------------
qini_results <- margot_policy(
  models_binary_flipped_all,
  save_plots         = FALSE,
  output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_args,
  policy_tree_args   = policy_tree_args,
  model_names        = names(models_binary_flipped_all$results),
  original_df        = original_df,
  label_mapping      = label_mapping_all_flipped,
  max_depth          = 2L,
  output_objects     = c("qini_plot", "diff_gain_summaries")
)

qini_gain <- margot_interpret_qini(
  qini_results,
  label_mapping = label_mapping_all_flipped
)

print_rate(qini_gain$summary_table)
cat(qini_gain$qini_explanation, "\n")

reliable_ids <- qini_gain$reliable_model_ids
reliable_ids
# (re-)compute plots only for models that passed Qini reliability
qini_results_valid <- margot_policy(
  models_binary_flipped_all,
  save_plots         = FALSE,
  output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_args,
  policy_tree_args   = policy_tree_args,
  model_names        = names(reliable_ids),
  original_df        = original_df,
  label_mapping      = label_mapping_all_flipped,
  max_depth          = 2L,
  output_objects     = c("qini_plot", "diff_gain_summaries")
)

qini_plots <- map(qini_results_valid, ~ .x$qini_plot)
qini_results_valid
# grab pretty outcome names
qini_names <- margot_get_labels(reliable_ids, label_mapping_all_flipped)
qini_names
label_mapping_all_flipped
cli_h1("Qini curves generated ✔")
reliable_ids
str(policy_tree_defaults)
str(decision_tree_defaults)
str(models_binary_flipped_all, max.level = 2)
reliable_ids
str(label_mapping_all_flipped)
# minimal test
test_result <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  model_names = reliable_ids[1],  # just first model
  max_depth = 2L,
  output_objects = c("policy_tree")  # just policy tree
)
# 4  POLICY TREES (max depth = 2) -------------------------------------------
policy_results_2L <- margot_policy(
  models_binary_flipped_all,
  save_plots         = FALSE,
  output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args   = policy_tree_defaults,
  model_names        = reliable_ids,
  max_depth          = 2L,
  original_df        = original_df,
  label_mapping      = label_mapping_all_flipped
)

policy_results_2L <- margot_plot_policy_combo(
  models_binary_flipped_all,
  # save_plots         = FALSE,
  # output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args   = policy_tree_defaults,
  model_name        = "model_t2_support_z",
  max_depth          = 2L,
  original_df        = original_df,
  label_mapping      = label_mapping_all_flipped
)

policy_results_2L
label_mapping_all_flipped
policy_plots <- map(policy_results_2L, ~ .x$combined_plot)

# ️5  PLAIN-LANGUAGE INTERPRETATION OF TREES ----------------------------------

policy_text <- margot_interpret_policy_batch(
  models            = models_binary_flipped_all,
  original_df       = original_df,
  model_names       = reliable_ids,
  label_mapping     = label_mapping_all_flipped,
  max_depth         = 2L
)

cat(policy_text, "\n")

cli::cli_h1("Finished: depth-2 policy trees analysed ✔")

# ───────────────────────────── EOF ────────────────────────────────────────────

# names of valid models
glued_policy_names_1 <- qini_names[[1]]
glued_policy_names_2 <- qini_names[[2]]
glued_policy_names_3 <- qini_names[[3]]
glued_policy_names_4 <- qini_names[[4]]
glued_policy_names_5 <- qini_names[[5]]
glued_policy_names_6 <- qini_names[[6]]
glued_policy_names_7 <- qini_names[[7]]


# view qini plots --------------------------------------------------------------

library(patchwork)
# combine first column of plots (4,6,7,8) and second column (9,11,12)
# these showed reliable qini results

combined_qini <- (
  qini_plots[[1]] /
    qini_plots[[2]] /
    qini_plots[[3]] /
    qini_plots[[4]]
) | (
  # remove this block if you don't have plots 9,11,12
  qini_plots[[5]] /
    qini_plots[[6]] /
    qini_plots[[7]]
) +
  # collect all legends into one shared guide
  plot_layout(guides = "collect") +
  # add title (and optionally subtitle)
  plot_annotation(
    title    = "Combined Qini Plots",
    subtitle = "Panels arranged with shared legend"
  ) &
  # apply theme modifications to all subplots
  theme(
    legend.position   = "bottom",           # place legend below
    plot.title        = element_text(hjust = 0.5),  # centre title
    plot.subtitle     = element_text(hjust = 0.5)   # centre subtitle
  )

# draw it
print(combined_qini)

# PLANNED COMPARISONS -----------------------------------------------------

# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+



# compact example ---------------------------------------------------------

# ------------------------------------------------------------------------------
# purpose: explores effect heterogeneity across researcher-defined strata (appendix only)
# ------------------------------------------------------------------------------
# workflow philosophy -----------------------------------------------------------
# • descriptive, not prescriptive. we *report* how τ̂ varies over groups – we do
#   NOT optimise a policy rule.
# • strata = ±1 sd splits by default; change to suit theory.
# • code chunks are short and labelled so students can run/debug in order.
# ------------------------------------------------------------------------------

library(margot)      # ≥ 1.0.52
library(tidyverse)   # pipes + helpers
library(knitr)       # kable tables
library(patchwork)   # plot stacks

# check package version early ---------------------------------------------------
stopifnot(utils::packageVersion("margot") >= "1.0.52")

# ------------------------- 1. define strata -----------------------------------
# 0. back‑transform helper -------------------------------------------
# margot stores income as z‑scored log dollars.  to write interpretable
# subtitles we convert ±1 sd back to the raw scale.  the helper simply
# inverts the: log → z transformation.
log_mean_inc <- mean(original_df$t0_log_household_inc, na.rm = TRUE)
log_sd_inc   <- sd  (original_df$t0_log_household_inc, na.rm = TRUE)

margot_back_transform_log_z(
  log_mean = log_mean_inc,
  log_sd   = log_sd_inc,
  z_scores = c(-1, 0, 1),
  label    = "data_scale"   # prints nz$ values ≈ 41k,…
)

# 1. define strata via logical vectors -------------------------------
# we treat ±1sd as the default cut for “low / mid / high”.  students
# can change the thresholds or supply any logical `subset_condition`.
complex_condition_political <- between(X[, "t0_political_conservative_z"], -1, 1)
complex_condition_wealth    <- between(X[, "t0_log_household_inc_z"], -1, 1)
complex_condition_age       <- between(X[, "t0_age_z"], -1, 1)

# sanity‑check age bounds on the raw scale
mean(original_df$t0_age) + c(-1, 1) * sd(original_df$t0_age)

# wealth -----------------------------------------------------------------------
subsets_standard_wealth <- list(
  Poor   = list(
    var = "t0_log_household_inc_z",
    value = -1,
    operator = "<",
    description = "income < −1sd (≈ NZ$41k)",
    label = "Poor"
  ),
  MiddleIncome = list(subset_condition = complex_condition_wealth, description = "income within ±1sd (≈ NZ$41‑191k)"),
  Rich   = list(
    var = "t0_log_household_inc_z",
    value = 1,
    operator = ">",
    description = "income > +1sd (≈ NZ$191k)",
    label = "Rich"
  )
)
# political orientation --------------------------------------------------------
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

# age --------------------------------------------------------------------------
subsets_standard_age <- list(
  Younger = list(
    var = "t0_age_z",
    value = -1,
    operator = "<",
    label = "Age < 35"
  ),
  Middle = list(
    var = "t0_age_z",
    # operator = "<",
    subset_condition = complex_condition_age,
    label = "Age 35-62"
  ),
  Older = list(
    var = "t0_age_z",
    value = 1,
    operator = ">",
    label = "Age > 62"
  )
)

# gender (binary) --------------------------------------------------------------
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

# ethnicity (binary dummies) --------------------------------------------------
subsets_standard_ethnicity <- list(
  Asian = list(
    var = "t0_eth_cat_asian_binary",
    value = 1,
    label = "Asians",
    description = "Asians"
    
  ),
  Euro = list(
    var = "t0_eth_cat_euro_binary",
    value = 1,
    label = "NZ Europeans ",
    description = "NZ Europeans"
    
  ),
  Pacific = list(
    var = "t0_eth_cat_pacific_binary",
    value = 1,
    label = "Pacific Peoples",
    description =  "Pacific Peoples"
  ),
  Maori = list(
    var = "t0_eth_cat_maori_binary",
    value = 1,
    label = "Māori",
    description = 'Māori'
  )
)
# religion ---------------------------------------------------------------------
subsets_standard_secular_vs_religious <- list(
  Not_Religious = list(var = "t0_religion_bigger_denominations_not_rel_binary", value = 1, label = "Not Religious"),
  Religious = list(var = "t0_religion_bigger_denominations_not_rel_binary", value = 0, label = "Religious")
)

# ------------------------- 2. defaults for ATE plots --------------------------
# set up domain names
domain_names <- c("wellbeing")

# set up subtitles
subtitles <- ""

# play around with these values
x_offset_comp <- 1.0
x_lim_lo_comp <- -1.0
x_lim_hi_comp <- 1.0

base_defaults_comparisons <- list(
  type = "RD",
  title = ate_title,
  e_val_bound_threshold = 1.2,
  label_mapping = label_mapping_all,
  adjust = "bonferroni",
  #<- new
  alpha = 0.05,
  # <- new
  colors = c(
    "positive" = "#E69F00",
    "not reliable" = "grey50",
    "negative" = "#56B4E9"
  ),
  x_offset = x_offset_comp,
  # will be set based on type
  x_lim_lo = x_lim_lo_comp,
  # will be set based on type
  x_lim_hi = x_lim_hi_comp,
  text_size = 8,
  linewidth = 0.75,
  estimate_scale = 1,
  base_size = 18,
  point_size = 2.5,
  title_size = 19,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  include_coefficients = FALSE
)

# ------------------------- 3. batch subgroup analysis -------------------------
planned_subset_results <- margot_planned_subgroups_batch(
  domain_models = list(models_binary),
  X             = X,
  base_defaults = base_defaults,
  subset_types  = list(
    wealth              = subsets_wealth,
    political_orientation = subsets_political,
    age                 = subsets_age,
    gender              = subsets_gender,
    ethnicity           = subsets_ethnicity,
    religion            = subsets_religion
  ),
  original_df   = original_df,
  label_mapping = label_mapping_all,
  domain_names  = "wellbeing",
  adjust        = "bonferroni",
  alpha         = 0.05,
  subtitles  = subtitles 
)

# ------------------------- 4. quick summaries ---------------------------------
cat(planned_subset_results$wellbeing$wealth$explanation)
cat(planned_subset_results$wellbeing$political_orientation$explanation)
cat(planned_subset_results$wellbeing$age$explanation)

# ------------------------- 5. example plots -----------------------------------
# wealth – vertical stack
wrap_plots(
  list(
    planned_subset_results$wellbeing$wealth$results$Poor$plot,
    planned_subset_results$wellbeing$wealth$results$`Middle Income`$plot,
    planned_subset_results$wellbeing$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  plot_annotation(
    title = "Wealth",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )

# ethnicity – 2×2 grid
wrap_plots(
  list(
    planned_subset_results$wellbeing$ethnicity$results$Asian$plot,
    planned_subset_results$wellbeing$ethnicity$results$`NZ European`$plot,
    planned_subset_results$wellbeing$ethnicity$results$Pacific$plot,
    planned_subset_results$wellbeing$ethnicity$results$Māori$plot
  ),
  ncol = 2
) +
  plot_annotation(
    title = "Ethnicity",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )

# ------------------------- 6. group-vs-group example --------------------------
comp_age <- margot_compare_groups(
  group1_name    = "Under 35",
  group2_name    = "Over 62",
  planned_subset_results$wellbeing$age$results$`Age < 35`$transformed_table,
  planned_subset_results$wellbeing$age$results$`Age > 62`$transformed_table,
  type           = "RD",
  decimal_places = 3
)
print(comp_age$results |> kbl("markdown", digits = 2))
cat(comp_age$interpretation)

cli::cli_h1("subgroup analysis complete ✔")


# example: secular vs religious
group_comparison_secular_religious <- margot_compare_groups(
  group1_name = "Secular People",
  group2_name = "People Who Identify With Religion",
  planned_subset_results$wellbeing$religion$results$`Not Religious`$transformed_table,
  # reference
  planned_subset_results$wellbeing$religion$results$Religious$transformed_table,
  # reference
  type            = "RD",
  # risk‑difference scale
  decimal_places  = 3
)
print(group_comparison_secular_religious$results |> kbl("markdown", digits = 2))
cat(group_comparison_secular_religious$interpretation)



# End of Script -----------------------------------------------------------



# EXTRA MATERIAL ----------------------------------------------------------



# FOR APPENDIX IF DESIRED -------------------------------------------------
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
    label_mapping = label_mapping_all,
    model_names = model_groups$autoc
  )
autoc_plots[[1]]

combined_autoc <- combine_and_save(autoc_plots, "rate_autoc")
model_groups$exploratory


# ILLLUSTRATIONS OF SETTINGS
# OPTIONS FOR DECISION TREES ----------------------------------------------
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