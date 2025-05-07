# script 3: causal workflow for estimating average treatment effects using margot
# may 2025
# questions: joseph.bulbulia@vuw.ac.nz
devtools::load_all("/Users/joseph/GIT/margot/")


# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+

# restart fresh session

rstudioapi::restartSession()



# reproducibility ---------------------------------------------------------


set.seed(123)


# essential library ---------------------------------------------------------



if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}



if (packageVersion("margot") < "1.0.35") {
  stop("please install margot >= 1.0.35 for this workflow\n
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
  margot,          # off-cran causal workflow tools
  tidyverse,       # data wrangling + plotting
  qs,              # fast data i/o
  here,            # project-relative file paths
  data.table,      # fast data manipulation
  fastDummies,     # dummy variable creation
  naniar,          # missing data handling
  skimr,           # summary statistics
  grf, ranger,     # machine learning forests
  doParallel,      # parallel processing,
  ggplot2 ,        # graphs
  rlang ,          # functions for base types/Core R/ 'Tidyverse'
  purrr ,          # functional programming tools.
  patchwork,      # nice graph placement
  janitor,         # nice labels
  glue            # format/ interpolate a string
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

nice_exposure_name = "Extraversion"
nice_outcome_name = "Wellbeing"
title = "Effect of {{nice_exposure_name}} on {{nice_outcome_name}}"

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
  "t2_bodysat_z" = "Body Satisfaction",
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


# make options -------------------------------------------------------------
# titles
title = "ATE Effects of {{nice_name_exposure}} on {{nice_name_outcome}}"
subtitle = ""
filename_prefix = "final_report"


# settings
x_offset = -.5
x_lim_lo = -.5
x_lim_hi = .5


# defaults for ate plots
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
weights <- df_grf$t1_adjusted_weights
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

# fit the model
cf_out <- margot_causal_forest(
  data         = toy_data,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  outcome_vars = "t2_kessler_latent_depression_z", # select variable in your outcome_variable set
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  covariates   = X_toy,
  W            = W_toy,
  weights      = weights_toy,
  save_data    = TRUE,
  save_models  = TRUE
  )

# inspect Qini curve ------------------------------------------------------
qini_tbl <- margot::margot_inspect_qini(cf_out, propensity_bounds = c(0.01, 0.97))
# show
print(qini_tbl)

# plot policy-combo trees --------------------------------------------------
combo1 <- margot_plot_policy_combo(
  result_object    = cf_out,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_name       = "model_t2_kessler_latent_depression_z",
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  max_depth        = 1L,          # depth-1 tree
  decision_tree_args = list(text_size = 4),
  policy_tree_args   = list(point_alpha = 0.7)
)

# show
combo1$combined_plot

# you can repeat for depth-2 ----------------------------------------------
combo2 <- margot_plot_policy_combo(
  result_object    = cf_out,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_name       = "model_t2_kessler_latent_depression_z",
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
  model_names        = "model_t2_kessler_latent_depression_z",
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

models_batch_2L <- margot_policy(
  cf_out,
  save_plots         = FALSE,
  output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args   = policy_tree_defaults,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  model_names        = "model_t2_kessler_latent_depression_z",
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  original_df        = original_df,
  label_mapping      = label_mapping_all,
  max_depth          = 2L)
# view first model's plots
models_batch_2L[[1]][[3]]  # combo plot
models_batch_2L[[1]][[4]]  # qini plot

# 2. flip the selected outcomes (and regen trees)
# use -- when the outcome is undesirable and we want to minimise it 
# (assuming the exposure is something we'd prescribe)
cf_out_f <- margot_flip_forests(
  model_results = cf_out,
  # +--------------------------+
  # |    MODIFY THIS           |
  # +--------------------------+
  flip_outcomes = c("t2_kessler_latent_depression_z"),
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  recalc_policy = TRUE
)

# where there are very low or high propensity scores (prob of exposure) we might consider trimming
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
  model_names = c("model_t2_kessler_latent_depression_z"),
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
  original_df = original_df,
  label_mapping = label_mapping_all,
  max_depth     = 2L
)

# flipped
# interpretation: exposure minimising depression
models_batch_flipped_2L[[1]][[3]]

# not flipped: exposure as maximizing depression
models_batch_2L[[1]][[3]]


# full model -------------------------------------------------------------

# ** uncomment to run full model**

# # health models -----------------------------------------------------------
models_binary <- margot::margot_causal_forest(
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

# save model
margot::here_save_qs(models_binary, "models_binary", push_mods)


# read results ------------------------------------------------------------

# if you save models you do not need to re-run them
models_binary <- margot::here_read_qs("models_binary", push_mods)




# make graphs -------------------------------------------------------------
# make ate plots ----------------------------------------------------------
# health plots ------------------------------------------------------------
binary_results <- margot_plot(
  models_binary$combined_table,
  options = outcomes_options_all,
  label_mapping = label_mapping_all,
  include_coefficients = FALSE,
  save_output = FALSE,
  order = "evaluebound_asc",
  original_df = original_df,
  e_val_bound_threshold = 1.2
)

# view
binary_results$transformed_table |> rename("E-Value" = "E_Value", "E-Value bound" = "E_Val_bound") |>
  kbl(format = 'markdown')

# check
cat(binary_results_health$interpretation)

# interpretation
cat(binary_results$interpretation)

# nice table
tables_list <- list(
  Wellbeing = binary_results$transformed_table
)

# make markdown tables (to be imported into the manuscript)
margot_bind_tables_markdown <- margot_bind_tables(
  tables_list = tables_list,
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


# count models by category
cat("Number of original models:\n", length(models_binary$results), "\n")



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

# flipping models: outcomes we want to minimise given the exposure --------
# standard negative outcomes/  not used in this study

# +--------------------------+
# |    MODIFY THIS           |
# +--------------------------+
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
# +--------------------------+
# |   END MODIFY             |
# +--------------------------+


# we will investigate losses to these outcomes
# usual flipped names for positive interventions
# commented out for this study

# WHICH OUTCOMES -- if any ARE UNDESIREABLE? 

# +--------------------------+
# |    MODIFY THIS           |
# +--------------------------+
flipped_names <- c(
  # v"Alcohol Frequency",
  #  "Alcohol Intensity",
  #  "BMI",
  #  "Fatigue",
  "Anxiety", "Depression", "Rumination"  #  ← select
  # "Perfectionism")
  # +--------------------------+
  # |   END MODIFY             |
  # +--------------------------+
)
  

# NOT IF THE EXPOSURE IS NEGATIVE, FOCUS ON WHICH OUTCOMES, if any, ARE POSITIVE AND FLIP THESE?
flip_outcomes <- flip_outcomes_standard #c( setdiff(t2_outcomes_all, flip_outcomes_standard) )

# check
flip_outcomes

# checks
# neg_check <- vapply(all_models$results[ paste0("model_", flip_outcomes) ],
#                     \(x) mean(x$tau_hat, na.rm = TRUE) < 0, logical(1))
# stopifnot(all(neg_check))   # every chosen outcome has a negative mean cate

# get labels
flipped_names <- margot_get_labels(flip_outcomes, label_mapping_all)

# check
flipped_names

# save for publication
here_save(flipped_names, "flipped_names")



# flip negatively oriented outcomes --------------------------------------

# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+


# flip models using margot's function

#  *** this will take some time ***

# ** give it time **
# ** once run/ comment out **
models_binary_flipped_all <- margot_flip_forests(models_binary,
                                                 flip_outcomes = flip_outcomes,
                                                 recalc_policy = TRUE)

# save
here_save_qs(models_binary_flipped_all, "models_binary_flipped_all", push_mods)

# read back if needed
models_binary_flipped_all <- here_read_qs("models_binary_flipped_all", push_mods)

# omnibus heterogeneity tests --------------------------------------------
# test for treatment effect heterogeneity across all outcomes
# result_ominbus_hetero_all <- margot::margot_omnibus_hetero_test(models_binary_flipped_all, 
#                                                                 label_mapping = label_mapping_all)
# 
# # view results table
# result_ominbus_hetero_all$summary_table |> kbl("markdown")
# 
# # view test interpretation
# cat(result_ominbus_hetero_all$brief_interpretation)

# rate test analysis -----------------------------------------------------

# create rate analysis table
rate_table_all <- margot_rate(
  models = models_binary_flipped_all,
  policy = "treat_best",  # or "withold_best" but don't attempt fitting curves or policytrees
  label_mapping = label_mapping_all
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

# compare rate and qini -- see grf documentation
cat(rate_interpretation_all$comparison)

# check out model names for different ways of thinking about heterogeneity
rate_interpretation_all$either_model_names
rate_interpretation_all$qini_model_names
rate_interpretation_all$both_model_names
rate_interpretation_all$autoc_model_names

# autoc plots ------------------------------------------------------------
# generate batch rate plots for models with significant heterogeneity
batch_rate_autoc_plots <- margot_plot_rate_batch(
  models_binary_flipped_all,
  save_plots = FALSE,
  # just use rate autoc for rate plots
  model_names = rate_interpretation_all$autoc_model_names
)

# extract individual plots from the batch result
autoc_plots <- batch_rate_autoc_plots

# determine number of columns based on number of plots
num_cols <- ifelse(length(autoc_plots) > 3, 2, 1)

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
  height <- 6 * ceiling(length(autoc_plots) / num_cols)
  
  ggsave(
    here::here(push_mods, "combined_autoc_plots.pdf"),
    combined_autoc_plot,
    width = width,
    height = height
  )
} else {
  # handle case with no plots
  message("No AUTOC plots available")
}


# qini --------------------------------------------------------------------
models_batch_qini_2L <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$qini_model_names,
  max_depth  = 2L,
  # ← new argument
  original_df = original_df,
  label_mapping = label_mapping_all
)

plots <- lapply(seq_along(models_batch_qini_2L), function(i) {
  models_batch_qini_2L[[i]][[4]]  # extract the 4th element (plot) from each model
})


rate_interpretation_all$qini_model_names

# give the plots meaningful names
names(plots) <- rate_interpretation_all$qini_model_names

# determine number of columns based on number of plots# determine number of columns based on number of plots
num_cols <- ifelse(length(plots) > 3, 2, 1)

# combine plots using patchwork
library(patchwork)


# create combined plot
combined_plot <- plots[[1]]
for (i in 2:length(plots)) {
  combined_plot <- combined_plot + plots[[i]]
}

# apply the dynamic layout
combined_plot <- combined_plot + plot_layout(ncol = num_cols)

# add titles and annotations
combined_plot <- combined_plot &
  plot_annotation(
    title = "Qini Model Plots",
    subtitle = paste0(length(plots), " models arranged in ", num_cols, " column(s)"),
    tag_levels = "A"  # adds a, b, c, etc. to the plots
  )

# view
combined_plot

# save (optional)
width <- ifelse(num_cols == 1, 8, 12)
height <- 6 * ceiling(length(plots)/num_cols)  # height per row * number of rows

# save
ggsave(here::here(push_mods, "combined_qini_plots.pdf"),
       combined_plot,
       width = width, height = height)


# interpretation ----------------------------------------------------------
# interpret qini curves
interpretation_qini_curves_2L <- margot_interpret_qini(
  models_batch_qini_2L,
  model_names = rate_interpretation_all$qini_model_names,
  label_mapping = label_mapping_all
)
interpretation_qini_curves_2L

# view qini interpretation
cat(interpretation_qini_curves_2L$qini_explanation)

# view summary table
interpretation_qini_curves_2L$summary_table |> kbl("markdown")



# policy tree analysis ---------------------------------------------------
# make policy trees
# 1 l decision trees are generally very bad
plots_policy_trees_1L <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$either_model_names,
  # defined above
  original_df = original_df,
  label_mapping = label_mapping_all,
  max_depth = 1L
)

# get number of models
n_models_1L <- length(rate_interpretation_all$either_model_names)

# # use purrr to map through and print each model
# purrr::map(1:n_models_1L, function(i) {
#   # print model name as a header
#   cat("# model", i, "\n")
#   # print the corresponding model plot
#   print(plots_policy_trees_1L[[i]][[3]])
#   # add spacing between models
#   cat("\n\n")
# })

model_outputs <- purrr::map(1:n_models_1L, ~plots_policy_trees_1L[[.x]][[3]])

# name the list elements by model number
names(model_outputs) <- paste0("model_", 1:n_models)


model_outputs$model_1
model_outputs$model_2

# policy tree analysis ---------------------------------------------------
# make policy trees
# *** 2l is much more persuasive ***
plots_policy_trees_2L <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  model_names = rate_interpretation_all$either_model_names,
  # defined above
  original_df = original_df,
  label_mapping = label_mapping_all,
  max_depth = 2L
)

n_models_2L <- length(rate_interpretation_all$either_model_names)

model_outputs_2L <- purrr::map(1:n_models_1L, ~plots_policy_trees_2L[[.x]][[3]])

model_outputs_2L[[1]]
model_outputs_2L[[2]]

interpret_plots_policy_trees_2L <- margot_interpret_policy_batch(
  models_binary_flipped_all, model_names = rate_interpretation_all$either_model_names)


# view interpretation
cat(interpret_plots_policy_trees_2L)

# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+


# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+
# you can investigate policy trees for all outcomes, mindful that the rate and qini are not reliable. 
# still, with appropriate caution, this may help to clarify psychologically interesting questions

all_plots_policy_trees_1L <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  # model_names = rate_interpretation_all$either_model_names, # use all
  # defined above
  original_df = original_df,
  label_mapping = label_mapping_all,
  max_depth = 1L
)
n_models_1L_all <- length(models_binary_flipped_all$results)
n_models_1L_all


model_outputs_1L_all <- purrr::map(1:n_models_1L_all, ~all_plots_policy_trees_2L[[.x]][[3]])

# view
model_outputs_1L_all[[1]] # interesting
model_outputs_1L_all[[2]] # ← not convincing  
model_outputs_1L_all[[3]] # ← not convincing  
model_outputs_1L_all[[4]] # 
model_outputs_1L_all[[5]] # ← not convincing 
model_outputs_1L_all[[6]] # ← not convincing  
model_outputs_1L_all[[7]] # ← not convincing  
model_outputs_1L_all[[8]] #
model_outputs_1L_all[[9]] # ← not convincing  
model_outputs_1L_all[[10]] # ← not convincing  
model_outputs_1L_all[[11]] # ← not convincing  
model_outputs_1L_all[[12]] # ← not convincing  
model_outputs_1L_all[[13]] # ← not convincing  



interpret_plots_policy_trees_1L_all <- margot_interpret_policy_batch(models_binary_flipped_all, max_depth = 1)


# view interpretation
cat(interpret_plots_policy_trees_1L_all)


# ALL model 1L
all_plots_policy_trees_1L <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  # model_names = rate_interpretation_all$either_model_names, # use all
  # defined above
  original_df = original_df,
  label_mapping = label_mapping_all,
  max_depth = 1L
)
n_models_all <- length(models_binary_flipped_all$results)
n_models_all


model_outputs_1L_all <- purrr::map(1:n_models_all, ~all_plots_policy_trees_1L[[.x]][[3]])

# view
model_outputs_1L_all[[1]] # interesting
model_outputs_1L_all[[2]] # ← not convincing  
model_outputs_1L_all[[3]] # ← not convincing  
model_outputs_1L_all[[4]] # 
model_outputs_1L_all[[5]] # ← not convincing 
model_outputs_1L_all[[6]] # ← not convincing  
model_outputs_1L_all[[7]] # ← not convincing  
model_outputs_1L_all[[8]] #
model_outputs_1L_all[[9]] # ← not convincing  
model_outputs_1L_all[[10]] # ← not convincing  
model_outputs_1L_all[[11]] # ← not convincing  
model_outputs_1L_all[[12]] # ← not convincing  
model_outputs_1L_all[[13]] # ← not convincing  


# interpretation
interpret_plots_policy_trees_1L_all <- margot_interpret_policy_batch(models_binary_flipped_all, max_depth = 1)


# view interpretation
cat(interpret_plots_policy_trees_1L_all)



# all models 2L -----------------------------------------------------------
# ALL model 1L
all_plots_policy_trees_2L <- margot_policy(
  models_binary_flipped_all,
  save_plots = FALSE,
  output_dir = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args = policy_tree_defaults,
  # model_names = rate_interpretation_all$either_model_names, # use all
  # defined above
  original_df = original_df,
  label_mapping = label_mapping_all,
  max_depth = 2L
)

# get n of models
n_models_all <- length(models_binary_flipped_all$results)



model_outputs_2L_all <- purrr::map(1:n_models_all, ~all_plots_policy_trees_2L[[.x]][[3]])

# view
model_outputs_2L_all[[1]] # 
model_outputs_2L_all[[2]] # ← 
model_outputs_2L_all[[3]] # ← 
model_outputs_2L_all[[4]] # 
model_outputs_2L_all[[5]] # ← 
model_outputs_2L_all[[6]] # 
model_outputs_2L_all[[7]] # ←  
model_outputs_2L_all[[8]] #
model_outputs_2L_all[[9]] # ← 
model_outputs_2L_all[[10]] # ← 
model_outputs_2L_all[[11]] # ←  
model_outputs_2L_all[[12]] # ← 
model_outputs_2L_all[[13]] # ← not convincing  


# interpretation
interpret_plots_policy_trees_2L_all <- margot_interpret_policy_batch(models_binary_flipped_all, max_depth = 2)


# view interpretation
cat(interpret_plots_policy_trees_1L_all)

# +--------------------------+
# |   END MODIFY SECTION     |
# +--------------------------+







# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+


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
print(plots_subgroup_wealth_health)

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



# plot options: showcased ---------------------------------------------
# default
margot_plot_decision_tree(models_binary_social, "model_t2_support_z", )
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

# select only plot 2 change size of axis_text
# change colours, modify etc...
margot::margot_plot_policy_tree(
  models_binary,
  "model_t2_support_z",
  plot_selection = "p2",
  axis_title_size = 30,
  split_label_size = 20,
  split_label_color = "red",
  split_line_color = "red",
)

# adjust only the alpha
margot::margot_plot_policy_tree(models_binary_social, "model_t2_support_z", point_alpha = .1)
