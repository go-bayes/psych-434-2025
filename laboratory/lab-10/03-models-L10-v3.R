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

# min version of margot
if (packageVersion("margot") < "1.0.50") {
  stop(
    "please install margot >= 1.0.50 for this workflow\n
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
str(df_grf)
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

# WHICH OUTCOMES -- if any ARE UNDESIREABLE?
flip_outcomes_standard = c(
  #"t2_alcohol_frequency_weekly_z",
  #"t2_alcohol_intensity_z",
  #"t2_hlth_bmi_z",
  #"t2_hlth_fatigue_z",
  "t2_kessler_latent_anxiety_z",
  #  ← select
  "t2_kessler_latent_depression_z",
  #  ← select
  "t2_rumination_z" #  ← select
  #"t2_perfectionism_z" # the exposure variable was not investigated
)

# when exposure is negative and you want to focus on how much worse off

# NOTE IF THE EXPOSURE IS NEGATIVE, FOCUS ON WHICH OUTCOMES, if any, ARE POSITIVE AND FLIP THESE?
# flip_outcomes<- c( setdiff(t2_outcomes_all, flip_outcomes_standard) )

# our example has the exposure as positive
flip_outcomes <- flip_outcomes_standard

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

# +--------------------------+
# |        END ALERT         |
# +--------------------------+


# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+
# HTE analysis ------------------------------------------------------------
# -------------------------------------------------------------------
# heterogeneity‐driven policy analysis – annotated workflow (depth = 2)
# -------------------------------------------------------------------
# this script shows the **minimal** end‑to‑end path from a fitted set of
# causal‑forest models to depth‑2 policy trees, *when our main aim is to
# explore treatment heterogeneity* (not necessarily to beat the ate).
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# 1. screen outcomes for evidence of heterogeneity
# ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––-
# calculate rate-autoc and rate-qini tables
# note: RATE-AUTOC asks: 'If I only treat the top k\% by τ(x), do I maximise average gain?'
# it rewards extreme uplift but can be volatile.
# RATE-Qini asks: 'if i treat more broadly, do I still improve aggregate outcome?' it trades intensity for coverage.

# this is a new function requires margot 1.0.48 or higher
label_mapping_all_flipped <- margot_reversed_labels(label_mapping_all, flipped_names)

# view
label_mapping_all_flipped

# save flipped labels
here_save(label_mapping_all_flipped, "label_mapping_all_flipped")

rate_results <-
  margot_rate(
    models   = models_binary_flipped_all,
    policy   = "treat_best",
    alpha  = 0.20,# raw p < 0.20 is enough to keep
    adjust = "fdr", # <‑‑correction
    label_mapping = label_mapping_all
  )

# show rate tables
rate_results$rate_autoc %>% kbl("markdown")
rate_results$rate_qini %>% kbl("markdown")


# save rate results
here_save(rate_results, "rate_results")

# generate textual interpretations for rate metrics
# 2. then run the interpreter, passing through adjust_positives_only = TRUE
rate_interp <- margot::margot_interpret_rate(rate_results,
                                     flipped_outcomes       = flipped_names,
                                     adjust_positives_only  = TRUE)

cat(rate_interp$comparison)


cat(rate_interp$autoc_results, "\n")
cat(rate_interp$qini_results, "\n")
cat(rate_interp$comparison, "\n")
cat(rate_interp$not_excluded_either, "\n")
here_save(rate_interp, "rate_interpretation")

# organise model groups by heterogeneity evidence
model_groups <- list(
  autoc  = rate_interp$autoc_model_names,
  qini   = rate_interp$qini_model_names,
  either = rate_interp$either_model_names,
  exploratory = rate_interp$not_excluded_either # not excluded, for exploratory research
)

# save
here_save(model_groups, "model_groups")

cli::cli_h1("rate metrics and interpretations complete ✔")


# -------------------------------------------------------------------
# 2. get model names after correction
# -------------------------------------------------------------------
model_keep <- model_groups$either
model_keep_autoc <-  model_groups$autoc
model_keep_qini <- model_groups$qini
model_exploratory <- model_groups$exploratory

# -------------------------------------------------------------------
# 3a. fit + plot depth‑2 policy trees for kept models ------------------
# -------------------------------------------------------------------
policy_2L_corrected <- margot_policy(
  models_binary_flipped_all,
  save_plots         = FALSE,
  # plots kept in memory
  output_dir         = here(push_mods),
  decision_tree_args = decision_tree_defaults,
  # layout tweaks
  policy_tree_args   = policy_tree_defaults,
  # tuning for policytree
  # model_names        = model_exploratory, # use all!
  # if you just want to explore, otherwise, pic "models_keep" for theoretically motivated work.
  original_df        = original_df,
  # raw test‑fold data for labels
  label_mapping      = label_mapping_all_flipped,
  max_depth          = 2L,
  output_objects     = "combined_plot" # returns ggplot object
)


# quick display  ----------------------------------------------------
plots_2L_corrected <- purrr::map(policy_2L_corrected, ~ .x[[1]])
purrr::walk(plots_2L_corrected, print)   # print each to the plot panel

length(plots_2L_corrected)
plots_2L_corrected[[1]] 
plots_2L_corrected[[2]]
plots_2L_corrected[[3]] 
plots_2L_corrected[[4]]
plots_2L_corrected[[5]]
plots_2L_corrected[[6]]
plots_2L_corrected[[7]]
plots_2L_corrected[[8]]
plots_2L_corrected[[9]]
plots_2L_corrected[[10]]
plots_2L_corrected[[11]]
plots_2L_corrected[[12]]


interpret_policy_trees <- margot_interpret_policy_batch(
  models_binary_flipped_all,
  original_df        = original_df,
  # raw test‑fold data for labels
  label_mapping      = label_mapping_all_flipped,
  max_depth          = 2L)





# -------------------------------------------------------------------
# 4. interpretation ----------------------------------
# -------------------------------------------------------------------
# generates a short paragraph per outcome explaining the splits and
# their implied treatment rule.
interp_all_2L <- margot_interpret_policy_batch(
  models_binary_flipped_all,
  # use the *flipped* master object for labels
  # model_names =  model_groups$exploratory, # use all
  label_mapping      = label_mapping_all_flipped,
  original_df = original_df # include original data for better interpretation
)

cat(interp_all_2L, "\n")

# end‑of‑workflow -----------------------------
cli::cli_h1("main 2l policy trees analysed ✔")


# PLANNED COMPARISONS -----------------------------------------------------

# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+

# -------------------------------------------------------------------
# planned subgroup analysis + theoretical comparisons ---------------
# -------------------------------------------------------------------
# this block asks: “do the causal‑forest estimates differ meaningfully
# across researcher‑defined demographic strata such as wealth, age,
# gender, ethnicity, and political orientation?”  the analysis is
# descriptive: we are *exploring* effect heterogeneity, not optimising a
# policy rule.
# -------------------------------------------------------------------

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

# 2. wrap strata in named lists --------------------------------------
# each entry is consumed by `margot_planned_subgroups_batch()`.  fields:
#   *  var / value / operator – simple threshold OR
#   * subset_condition       – pre‑computed logical
#   * description            – shown in plots
#   * label                  – used as facet name
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


# define complex political orientation
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
    description = "Effects among those HS_hold income within +/-1SD (> NZD 41k < NZD 191k)",
    label = "Middle Income"
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


# age subsets
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

# religious denominations
subsets_standard_secular_vs_religious <- list(
  Not_Religious = list(var = "t0_religion_bigger_denominations_not_rel_binary", value = 1, label = "Not Religious"),
  Religious = list(var = "t0_religion_bigger_denominations_not_rel_binary", value = 0, label = "Religious")
)

# batch planned subgroup analysis -----------------------------------------
# set up domain names
domain_names <- c("wellbeing")

# set up subtitles
subtitles <- ""

# new base defaults that work for your comparisons
# defaults for ate plots

# play around with these values
x_offset_comp <- 1.0
x_lim_lo_comp <- -1.0
x_lim_hi_comp <- 1.0

label_mapping_all
base_defaults_comparisons <- list(
  type = "RD",
  title = ate_title,
  e_val_bound_threshold = 1.2,
  label_mapping = "label_mapping_all",
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

# 3. batch subgroup analysis -----------------------------------------
planned_subset_results <- margot_planned_subgroups_batch(
  domain_models  = list(models_binary),
  X              = X,
  base_defaults  = base_defaults_comparisons,
  subset_types   = list(
    religions_secular = subsets_standard_secular_vs_religious,
    # only compare one group with others
    ethnicity = subsets_standard_ethnicity,
    wealth = subsets_standard_wealth,
    gender = subsets_standard_gender,
    cohort = subsets_standard_age
  ),
  original_df    = original_df,
  label_mapping  = label_mapping_all,
  # ← supply it here
  domain_names   = "wellbeing",
  subtitles      = "",
  adjust         = "bonferroni",
  # ← here
  alpha          = 0.05           # ← and here
)



# the function:
#   1. filters the test fold to each stratum,
#   2. re‑computes the doubly‑robust ate for every outcome,
#   3. wraps results in a tidy list → $domain → $subset → $plot/table.

# 4. text summaries ---------------------------------------------------
cat(planned_subset_results$wellbeing$wealth$explanation)
cat(planned_subset_results$wellbeing$religions$explanation)
cat(planned_subset_results$wellbeing$ethnicity$explanation)
cat(planned_subset_results$wellbeing$religions_secular$explanation)


# (political, gender, cohort idem)

# 5. assemble quick facet plots --------------------------------------
# patchwork stacks the ggplots vertically (ncol = 1) or in a grid.  the
# helper `wrap_plots()` is from patchwork.  here we show wealth strata.
# -------------------------------------------------------------------
# example 1 – stack three wealth strata vertically -------------------
plots_subgroup_wealth <- wrap_plots(
  list(
    planned_subset_results$wellbeing$wealth$results$Poor$plot,
    planned_subset_results$wellbeing$wealth$results$`Middle Income`$plot,
    planned_subset_results$wellbeing$wealth$results$Rich$plot
  ),
  ncol = 1
) +
  plot_annotation(title = "Wealth",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))
print(plots_subgroup_wealth)

# -------------------------------------------------------------------
# example 2 – 2×2 grid for ethnicity (space-saving) ------------------
plots_subgroup_ethnicity <- wrap_plots(
  list(
    planned_subset_results$wellbeing$ethnicity$results$Asians$plot,
    planned_subset_results$wellbeing$ethnicity$result$`NZ Europeans `$plot,
    planned_subset_results$wellbeing$ethnicity$results$`Pacific Peoples`$plot,
    planned_subset_results$wellbeing$ethnicity$results$Māori$plot
  ),
  ncol = 2
) +
  plot_annotation(title = "Ethnicity",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))
print(plots_subgroup_ethnicity)
planned_subset_results$wellbeing$ethnicity$results$Asians$plot
# -------------------------------------------------------------------
# example 3 – horizontal strip for gender ----------------------------
plots_subgroup_gender <- wrap_plots(
  list(
    planned_subset_results$wellbeing$gender$results$Female$plot,
    planned_subset_results$wellbeing$gender$results$Male$plot
  ),
  ncol = 2
) +
  plot_annotation(title = "Gender",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))
print(plots_subgroup_gender)


# example 4 – Religious vs Secular -------------------
plots_subgroup_secular <- wrap_plots(
  list(
    planned_subset_results$wellbeing$religions_secular$results$`Not Religious`$plot,
    planned_subset_results$wellbeing$religions_secular$results$`Religious`$plot
  ),
  ncol = 1
) +
  plot_annotation(title = "Secular vs Religious",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))
print(plots_subgroup_secular)



# 6. group‑vs‑group comparisons --------------------------------------
#   • `margot_compare_groups()` takes two transformed tables (risk‑diff)
#   • returns a *difference in differences* tibble + plain‑text interp.

# example: young (<35) vs older (>62)
group_comparison_age_young_old <- margot_compare_groups(
  group1_name = "People Under 35 Years Old",
  group2_name = "People Over 62 Years Old",
  planned_subset_results$wellbeing$cohort$results$`Age < 35`$transformed_table,
  # reference
  planned_subset_results$wellbeing$cohort$results$`Age > 62`$transformed_table,
  # comparison
  type            = "RD",
  # risk‑difference scale
  decimal_places  = 4
)
print(group_comparison_age_young_old$results |> kbl("markdown", digits = 2))
cat(group_comparison_age_young_old$interpretation)


# 7. group‑vs‑group comparisons --------------------------------------
#   • `margot_compare_groups()` takes two transformed tables (risk‑diff)
#   • returns a *difference in differences* tibble + plain‑text interp.

# example: young (<35) vs older (>62)
group_comparison_secular_religious <- margot_compare_groups(
  group1_name = "Secular People",
  group2_name = "People Who Identify With Religion",
  planned_subset_results$wellbeing$religions_secular$results$`Not Religious`$transformed_table,
  # reference
  planned_subset_results$wellbeing$religions_secular$results$Religious$transformed_table,
  # reference
  type            = "RD",
  # risk‑difference scale
  decimal_places  = 3
)
print(group_comparison_secular_religious$results |> kbl("markdown", digits = 2))
cat(group_comparison_secular_religious$interpretation)



# note comparisons should be specified a priori, and you can save results as you
# for your manuscript


# end of theoretical comparisons block ---------------------------------



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
    label_mapping      = label_mapping_all,
    model_names = model_groups$autoc
  )
autoc_plots[[1]]

combined_autoc <- combine_and_save(autoc_plots, "rate_autoc")
model_groups$exploratory
# step 4: Qini model curves --------------------------------------------
qini_policy_results <-
  margot_policy(
    models_binary_flipped_all,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = list(),
    policy_tree_args   = list(),
    # model_names        = "model_t2_neighbourhood_community_z",
    original_df        = original_df,
    label_mapping      = label_mapping_all,
    max_depth          = 2L,
    output_objects     = c("qini_plot", "diff_gain_summaries")
  )
qini_policy_results$model_t2_neighbourhood_community_z$diff_gain_summaries
# view qini plots
qini_plots <- purrr::map(qini_policy_results, ~ .x$qini_plot)


qini_policy_results <-
  margot_interpret_policy_batch(
    models_binary_flipped_all,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = list(),
    policy_tree_args   = list(),
    # model_names        = "model_t2_neighbourhood_community_z",
    original_df        = original_df,
    label_mapping      = label_mapping_all,
    max_depth          = 2L,
    output_objects     = c("qini_plot", "diff_gain_summaries")
  )
# how many?
length(qini_plots)
# explore
qini_plots[[1]] 
qini_plots[[2]] #* 
qini_plots[[3]] #
qini_plots[[4]] #
qini_plots[[5]] #
qini_plots[[6]] #
qini_plots[[7]] #
qini_plots[[8]] #
qini_plots[[9]] # bad
qini_plots[[10]] # bad
qini_plots[[11]] #
qini_plots[[12]] #

# suggests qini result 
qini_policy_results[[2]]$diff_gain_summaries
qini_policy_results[[5]]$diff_gain_summaries
qini_policy_results[[6]]$diff_gain_summaries
qini_policy_results[[7]]$diff_gain_summaries
qini_policy_results[[8]]$diff_gain_summaries
qini_policy_results[[9]]$diff_gain_summaries # bad
qini_policy_results[[10]]$diff_gain_summaries
qini_policy_results[[11]]$diff_gain_summaries

qini_policy_results


cli::cli_h1("Qini model curves plotted ✔")


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
