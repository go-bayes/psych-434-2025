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
min_version <- "1.0.44"
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
# !!!! THIS WILL TAKE TIME  !!!!!
models_binary <- margot::here_read_qs("models_binary", push_mods)

# +--------------------------+
# |        END ALERT         |
# +--------------------------+

# count models by category
# just a check
cat("Number of original models:\n", length(models_binary$results), "\n")

# make ate plots ----------------------------------------------------------
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
  e_val_bound_threshold = 1.2,
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
  "t2_kessler_latent_anxiety_z", #  ← select
  "t2_kessler_latent_depression_z",#  ← select
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

keep_autoc <- margot_screen_models(
  models_binary_flipped_all,  # full margot object with $results
  rule   = "rate",           # heterogeneity evidence from rate tests
  target = "AUTOC",         # either RATE‑AUTOC or RATE‑Qini may pass
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

# -------------------------------------------------------------------
# 2. get model names after correction
# -------------------------------------------------------------------

# pull the matching model keys (prefixed) for downstream plotting ------
model_keep <- paste0("model_", keep)

# -------------------------------------------------------------------
# 3. fit + plot depth‑2 policy trees for kept models ------------------
# -------------------------------------------------------------------
policy_2L_corrected <- margot_policy(
  models_binary_flipped_all,
  save_plots         = FALSE,          # plots kept in memory
  output_dir         = here(push_mods),
  decision_tree_args = decision_tree_defaults, # layout tweaks
  policy_tree_args   = policy_tree_defaults,   # tuning for policytree
  model_names        = model_keep,     # only models that survived bh
  original_df        = original_df,    # raw test‑fold data for labels
  label_mapping      = label_mapping_all,
  max_depth          = 2L,
  output_objects     = "combined_plot" # returns ggplot object
)

# quick display  ----------------------------------------------------
plots_2L_corrected <- purrr::map(policy_2L_corrected, ~ .x[[1]])
purrr::walk(plots_2L_corrected, print)   # print each to the plot panel

# -------------------------------------------------------------------
# 4. interpretation ----------------------------------
# -------------------------------------------------------------------
# generates a short paragraph per outcome explaining the splits and     
# their implied treatment rule.                                         
interp_all_2L <- margot_interpret_policy_batch(
  models_binary_flipped_all,  # use the *flipped* master object for labels
  model_names = model_keep
)

cat(interp_all_2L, "\n")

#  saved -- to be imported into manuscript
here_save_qs(policy_2L_corrected, "policy_2L_corrected", push_mods)
here_save(interp_all_2L, "interp_all_2L")

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
  label    = "data_scale"   # prints nz$ values ≈ 41k, …
)

# 1. define strata via logical vectors -------------------------------
# we treat ±1 sd as the default cut for “low / mid / high”.  students
# can change the thresholds or supply any logical `subset_condition`.
complex_condition_political <- between(X[,"t0_political_conservative_z"], -1, 1)
complex_condition_wealth    <- between(X[,"t0_log_household_inc_z"],    -1, 1)
complex_condition_age       <- between(X[,"t0_age_z"],                   -1, 1)

# sanity‑check age bounds on the raw scale
mean(original_df$t0_age) + c(-1, 1) * sd(original_df$t0_age)

# 2. wrap strata in named lists --------------------------------------
# each entry is consumed by `margot_planned_subgroups_batch()`.  fields:
#   *  var / value / operator – simple threshold OR
#   * subset_condition       – pre‑computed logical
#   * description            – shown in plots
#   * label                  – used as facet name
subsets_standard_wealth <- list(
  Poor   = list(var="t0_log_household_inc_z", value=-1, operator="<",
                description="income < −1 sd (≈ NZ$41k)", label="Poor"),
  MiddleIncome = list(subset_condition = complex_condition_wealth,
                      description = "income within ±1 sd (≈ NZ$41‑191k)"),
  Rich   = list(var="t0_log_household_inc_z", value= 1, operator=">",
                description="income > +1 sd (≈ NZ$191k)", label="Rich")
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
# set up domain names
domain_names <- c("wellbeing")

# set up subtitles
subtitles <- ""

# 3. batch subgroup analysis -----------------------------------------
planned_subset_results <- margot_planned_subgroups_batch(
  domain_models = list(models_binary),  # use *original* (unflipped) forests
  X             = X,                    # covariate matrix
  base_defaults = base_defaults_binary, # labeller + theme options
  subset_types  = list(
    wealth     = subsets_standard_wealth,
    ethnicity  = subsets_standard_ethnicity,
    political  = subsets_standard_political,
    gender     = subsets_standard_gender,
    cohort     = subsets_standard_age
  ),
  original_df   = original_df,          # raw data for back‑transforms
  domain_names  = "wellbeing",
  subtitles     = ""                   # optional ggplot subtitle
)

# the function:                                                        
#   1. filters the test fold to each stratum,                           
#   2. re‑computes the doubly‑robust ate for every outcome,             
#   3. wraps results in a tidy list → $domain → $subset → $plot/table.

# 4. text summaries ---------------------------------------------------
cat(planned_subset_results$wellbeing$wealth$explanation)
cat(planned_subset_results$wellbeing$ethnicity$explanation)
# (political, gender, cohort idem)

# 5. assemble quick facet plots --------------------------------------
# patchwork stacks the ggplots vertically (ncol = 1) or in a grid.  the
# helper `wrap_plots()` is from patchwork.  here we show wealth strata.
# -------------------------------------------------------------------
# example 1 – stack three wealth strata vertically -------------------
plots_subgroup_wealth <- wrap_plots(
  list(
    planned_subset_results$wellbeing$wealth$results$Poor$plot,
    planned_subset_results$wellbeing$wealth$results$MiddleIncome$plot,
    planned_subset_results$wellbeing$wealth$results$Rich$plot
  ), ncol = 1) +
  plot_annotation(
    title = "Wealth",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )
print(plots_subgroup_wealth)

# -------------------------------------------------------------------
# example 2 – 2×2 grid for ethnicity (space-saving) ------------------
plots_subgroup_ethnicity <- wrap_plots(
  list(
    planned_subset_results$wellbeing$ethnicity$results$Asian$plot,
    planned_subset_results$wellbeing$ethnicity$results$Euro$plot,
    planned_subset_results$wellbeing$ethnicity$results$Pacific$plot,
    planned_subset_results$wellbeing$ethnicity$results$Maori$plot
  ), ncol = 2) +
  plot_annotation(
    title = "Ethnicity",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )
print(plots_subgroup_ethnicity)

# -------------------------------------------------------------------
# example 3 – horizontal strip for gender ----------------------------
plots_subgroup_gender <- wrap_plots(
  list(
    planned_subset_results$wellbeing$gender$results$Female$plot,
    planned_subset_results$wellbeing$gender$results$Male$plot
  ), ncol = 2) +
  plot_annotation(
    title = "Gender",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )
print(plots_subgroup_gender)


# 6. group‑vs‑group comparisons --------------------------------------
#   • `margot_compare_groups()` takes two transformed tables (risk‑diff)
#   • returns a *difference in differences* tibble + plain‑text interp.

# example: young (<35) vs older (>62)
group_comparison_age_young_old <- margot_compare_groups(
  planned_subset_results$wellbeing$cohort$results$`Age < 35`$transformed_table, # reference
  planned_subset_results$wellbeing$cohort$results$`Age > 62`$transformed_table, # comparison
  type            = "RD",          # risk‑difference scale
  decimal_places  = 4
)
print(group_comparison_age_young_old$results |> kbl("markdown", digits = 2))
cat(group_comparison_age_young_old$interpretation)

# note comparisons should be specified a priori, and you can save results as you
# for your manuscript


# end of theoretical comparisons block ---------------------------------



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




