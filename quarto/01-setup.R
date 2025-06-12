  ---
title: "Your Title"
abstract: |
  **Background**: (Brief few sentences)
  **Objectives**: 
    1. Estimate the causal effect of YOUR EXPOSURE on YOUR OUTCOMES measured one year later.
    2. Evaluate whether these effects vary across the population.
    3. Provide policy guidance on which individuals might benefit most.
  **Method**: We conducted a three-wave retrospective cohort study (waves XX-XXX, October XXXX--October XXXX) using data *SIMULATED* from the New Zealand Attitudes and Values Study, a nationally representative panel. Participants were eligible if they participated in the NZAVS in the baseline wave (XXXX,...). We defined the exposure as (XXXX  > NUMBER on a 1-7 Likert Scale (1 = yes, 0 = no)). To address attrition, we applied inverse probability of censoring weights; to improve external validity, we applied weights to the population distribution of Age, Ethnicity, and Gender. We computed expected mean outcomes for the population in each exposure condition (high XXXX/low XXXXX). Under standard causal assumptions of unconfoundedness, the contrast provides an unbiased average treatment effect. We then used causal forests to detect heterogeneity in these effects and employed policy tree algorithms to identify individuals ("strong responders") likely to experience the greatest benefits.
  **Results**:   Increasing XXXXX leads to XXXXX. Heterogeneous responses to (e.g. *Forgiveness*, *Personal Well-Being*, and *Life-Satisfaction*...) reveal structural variability in subpopulations...
  **Implications**: (Brief few sentences)
  **Keywords**: *Causal Inference*;  *Cross-validation*; *Distress*; *Employment*; *Longitudinal*; *Machine Learning*; *Religion*; *Semi-parametric*; *Targeted Learning*.
author: 
  - name: YOUR NAME
    affiliation: Victoria University of Wellington, New Zealand
    email: XXXXX
    corresponding: yes
keywords: [Causal Inference, Cross-validation,...]
editor_options: 
  chunk_output_type: console
date: "last-modified"
fontfamily: libertinus
bibliography: references.bib
csl: apa7.csl
format:
  docx: # comment this out if you want pdf
    default: false  # comment this out if you want pdf
  pdf:
    pdf-engine: lualatex
    sanitise: true
    keep-tex: true
    link-citations: true
    colorlinks: true
    documentclass: article
    classoption: ["single column"]
    lof: false
    lot: false
    geometry:
      - top=30mm
      - left=25mm
      - heightrounded
      - headsep=22pt
      - headheight=11pt
      - footskip=33pt
      - ignorehead
      - ignorefoot
    header-includes:
      - \let\oldtabular\tabular
      - \renewcommand{\tabular}{\small\oldtabular}
      - \setlength{\tabcolsep}{4pt}  # adjust this value as needed
execute:
  echo: false
  warning: false
  include: true
  eval: true
---

```{r}
#| label: setup
#| echo: false
#| include: false
#| eval: true

# ── initialisation and setup ──────────────────────────────────────────────────

# save this file in your project root (e.g. 'quarto/1_setup.R')

# load here to manage paths
dep <- requireNamespace("here", quietly = TRUE)
if (!dep) install.packages("here")
library(here)

# create required folders (these will likely already exist)
dirs <- c(
  here("quarto"),
  here("bibliography"),
  here("save_directory"),
  here("csl")
)
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ensure tinytex for PDF rendering
if (!requireNamespace("tinytex", quietly = TRUE)) {
  install.packages("tinytex")
  tinytex::install_tinytex()
}

# ensure pacman for package management
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# min version of margot
if (packageVersion("margot") < "1.0.54") {
  stop(
    "please install margot >= 1.0.54 for this workflow\n
       run: 
       devtools::install_github('go-bayes/margot')
"
  )
}

# call library
library("margot")

# check package version
packageVersion(pkg = "margot")

# load (and install if needed) all required packages
pacman::p_load(
  boilerplate, shiny, tidyverse,
  kableExtra, glue, patchwork, stringr,
  ggplot2, ggeffects, parameters,
  table1, knitr, extrafont, here, cli
)

# load fonts (requires prior extrafont::font_import())
if (requireNamespace("extrafont", quietly = TRUE)) {
  extrafont::loadfonts(device = "all")
} else {
  message("'extrafont' not installed; skipping font loading")
}

# reproducibility
set.seed(123)

# copy CSL and BibTeX into quarto folder
src_files <- list(
  c(here("csl", "apa7.csl"),     here("quarto", "apa7.csl")),
  c(here("bibliography", "references.bib"), here("quarto", "references.bib"))
)
for (f in src_files) {
  if (!file.exists(f[2]) && file.exists(f[1])) {
    file.copy(f[1], f[2])
  }
}

# ── define paths and import data ──────────────────────────────────────────────
push_mods                  <- here::here("save_directory")
final_boilerplate_data     <- here::here("final_boilerplate_data")
unified_db                 <- boilerplate_import(data_path = final_boilerplate_data)

# ── read study variables ──────────────────────────────────────────────────────
baseline_vars              <- margot::here_read("baseline_vars")
exposure_var               <- margot::here_read("exposure_var")
outcome_vars               <- margot::here_read("outcome_vars")

# ── define study waves ────────────────────────────────────────────────────────
baseline_wave              <- margot::here_read("baseline_wave")
exposure_waves             <- margot::here_read("exposure_waves")
outcome_wave               <- margot::here_read("outcome_wave")

baseline_wave_glued        <- glue::glue(baseline_wave)

# ── define study parameters ───────────────────────────────────────────────────
study_years                <- "2018-2021"
name_exposure              <- here_read("name_exposure")
name_outcome_variables     <- "MY OUTCOME VARIABLES IN THIS STUDY"
name_exposure_lower        <- tolower(name_exposure)

# ── read tables for manuscript ────────────────────────────────────────────────
markdown_table_baseline    <- margot::here_read("baseline_table")
markdown_table_exposures   <- margot::here_read("exposure_table")
markdown_table_outcomes    <- margot::here_read("outcomes_table")
# margot_bind_tables_markdown <- margot::here_read("margot_bind_tables_markdown")

# ── sample size information ───────────────────────────────────────────────────
n_total                    <- margot::here_read("n_total")
n_participants             <- here_read("n_participants")

# ── variable labels and mappings ──────────────────────────────────────────────
var_labels_measures        <- here_read("var_labels_measures")
label_mapping_all          <- here_read("label_mapping_all")

# ── import data for visualisation ─────────────────────────────────────────────
original_df                <- margot::here_read('df_wide', push_mods)
df_grf                     <- margot::here_read('df_grf', push_mods)

# co-variates
E                          <- margot::here_read('E', push_mods)
# select covariates and drop numeric attributes
X                          <- margot::remove_numeric_attributes(df_grf[E])

# ── define nice names and regimes ─────────────────────────────────────────────
nice_name_exposure         <- stringr::str_to_sentence(name_exposure)
name_outcomes_lower        <- "multi-dimensional wellbeing"
nice_name_outcome          <- stringr::str_to_sentence(name_outcomes_lower)

# title
ate_title                  <- glue("ATE Effects of {nice_name_exposure} on {nice_name_outcome}")

# ── define exposure thresholds and regimes ────────────────────────────────────
lower_cut                  <- here_read("lower_cut")
upper_cut                  <- here_read("upper_cut")
threshold                  <- here_read("threshold")
inverse_threshold          <- here_read("inverse_threshold")
scale_range                <- margot::here_read("scale_range")

# create and check variables
value_exposure             <- glue::glue(threshold,"", upper_cut,", ", scale_range)
value_control              <- glue::glue(inverse_threshold, upper_cut,", ", scale_range)

# regimes
name_control_regime_lower  <- glue::glue("low {name_exposure_lower}")
value_exposure_regime      <- glue::glue("Set {name_exposure} {threshold} {upper_cut} {scale_range}")
value_control_regime       <- glue::glue("Set {name_exposure} {inverse_threshold} {upper_cut} {scale_range}")

contrast_template          <- "We used causal forests to estimate an average treatment effect as a contrast between *{name_control_regime_lower}* and *{name_exposure_lower}* on {name_outcomes_lower}."
contrast_text              <- glue(contrast_template)

# ── import histogram of binary exposure ───────────────────────────────────────
graph_cut                  <- margot::here_read("graph_cut")

# ── verify assumptions (positivity) ───────────────────────────────────────────
transition_tables          <- margot::here_read("transition_tables")
transition_tables_binary   <- here_read("transition_tables_binary")

# ── generate measures text for methods section ────────────────────────────────
baseline_measures_text     <- boilerplate_generate_measures(
  variable_heading = "Baseline Covariates",
  variables        = baseline_vars,
  db               = unified_db,
  heading_level    = 3,
  subheading_level = 4,
  print_waves      = FALSE,
  label_mappings   = var_labels_measures
)

exposure_measures_text     <- boilerplate_generate_measures(
  variable_heading = "Exposure Variable",
  variables        = name_exposure,
  db               = unified_db,
  heading_level    = 3,
  subheading_level = 4,
  print_waves      = FALSE,
  label_mappings   = var_labels_measures
)

outcome_measures_text      <- boilerplate_generate_measures(
  variable_heading = "Outcome Variables",
  variables        = outcome_vars,
  db               = unified_db,
  heading_level    = 3,
  subheading_level = 4,
  print_waves      = FALSE,
  label_mappings   = var_labels_measures
)

# ── exposure description from database ────────────────────────────────────────
measures_exposure          <- glue::glue(unified_db$measures[[name_exposure]]$description)

# ── set plot defaults for ate plots ───────────────────────────────────────────
base_defaults_binary       <- list(
  type                     = "RD",
  title                    = ate_title,
  e_val_bound_threshold    = 1.2,
  colors                   = c(
    "positive"     = "#E69F00",
    "not reliable" = "grey50",
    "negative"     = "#56B4E9"
  ),
  x_offset                 = -0.25,
  x_lim_lo                 = -0.25,
  x_lim_hi                 = 0.25,
  text_size                = 5,
  linewidth                = 0.75,
  estimate_scale           = 1,
  base_size                = 20,
  point_size               = 4,
  title_size               = 20,
  subtitle_size            = 16,
  legend_text_size         = 10,
  legend_title_size        = 10,
  include_coefficients     = FALSE
)

# ── create plot options for outcomes ──────────────────────────────────────────
outcomes_options_all       <- margot_plot_create_options(
  title           = ate_title,
  base_defaults   = base_defaults_binary,
  subtitle        = "",
  filename_prefix = "grf"
)

# ── load model results ────────────────────────────────────────────────────────
models_binary  <- margot::here_read_qs("models_binary", push_mods)

# ──────────────────────────────────────────────────────────────────────────────
# CRITICAL DECISION POINT: FLIPPED OUTCOMES OR NOT
# ──────────────────────────────────────────────────────────────────────────────

# ********** READ THIS CAREFULLY **********
# if you DID NOT flip any outcomes:
#   - set: use_flipped <- FALSE
#   - this will use models_binary throughout
#   - this will use label_mapping_all throughout
#
# if you DID flip outcomes:
#   - set: use_flipped <- TRUE
#   - ensure models_binary_flipped_all exists
#   - ensure flip_outcomes vector exists
#   - this will create label_mapping_all_flipped

use_flipped <- TRUE  # <- MAKE TRUE IF YOU FLIPPED OUTCOMES, `FALSE` otherwise 

# set up variables based on whether outcomes were flipped
if (use_flipped) {
  # check that required objects exist
  if (!exists("models_binary_flipped_all")) {
    models_binary_flipped_all <- here_read_qs("models_binary_flipped_all", push_mods)
  }
  
  # try to read flip_outcomes and flipped_names
  # use tryCatch to handle missing files gracefully
  flip_outcomes <- tryCatch(
    here_read("flip_outcomes"),
    error = function(e) {
      stop("flip_outcomes file not found. Please ensure it exists if use_flipped = TRUE")
    }
  )
  
  flipped_names <- tryCatch(
    here_read("flipped_names"),
    error = function(e) {
      stop("flipped_names file not found. Please ensure it exists if use_flipped = TRUE")
    }
  )
  
  # create flipped label mapping
  label_mapping_all_flipped <- margot_reversed_labels(label_mapping_all, flip_outcomes)
  
  # use flipped models and labels
  models_for_analysis <- models_binary_flipped_all
  labels_for_analysis <- label_mapping_all_flipped
  flipped_list <- paste(flipped_names, collapse = ", ")
  
} else {
  # use standard models and labels (no flipping)
  models_for_analysis <- models_binary
  labels_for_analysis <- label_mapping_all
  
  # ensure flipped_names is a character vector, not a function
  # remove any existing flipped_names object that might be a function
  if (exists("flipped_names") && is.function(flipped_names)) {
    rm(flipped_names)
  }
  
  flipped_names <- character(0)  # empty character vector
  flipped_list <- ""
}

devtools::load_all("/Users/joseph/GIT/margot/")


# ── average treatment effects (ate) analysis ──────────────────────────────────
ate_results <- margot_plot(
  models_binary$combined_table,
  options               = outcomes_options_all,
  label_mapping         = label_mapping_all,  # always use standard labels for ate
  include_coefficients  = FALSE,
  save_output           = FALSE,
  order                 = "evaluebound_asc",
  original_df           = original_df,
  e_val_bound_threshold = 1.2,
  rename_ate            = TRUE,
  adjust                = "bonferroni",
  alpha                 = 0.05
)


# check original table
models_binary$combined_table

# check results table: 
ate_results$transformed_table


# check interpretation: 
cat(ate_results$interpretation)

# check plot data is correct
# plot_data <- ate_results$plot$data
# print(plot_data[plot_data$outcome == "Social Belonging", c("2.5 %", "97.5 %")])

# ── make nice markdown table -----------------──────────────────────────────────

margot_bind_tables_markdown <- margot_bind_tables(
  ate_results$transformed_table,
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


# ──────────────────────────────────────────────────────────────────────────────
# HETEROGENEITY ANALYSIS
# this section adapts based on whether outcomes were flipped
# ──────────────────────────────────────────────────────────────────────────────

# check package version for heterogeneity analysis
stopifnot(utils::packageVersion("margot") >= "1.0.54")

# helper function for printing tables
print_rate <- function(tbl) {
  tbl |>
    mutate(across(where(is.numeric), \(x) round(x, 2))) |>
    kbl(format = "markdown")
}

# ── 1. screen for heterogeneity (rate autoc + rate qini) ─────────────────────
rate_results <- margot_rate(
  models        = models_for_analysis,
  policy        = "treat_best",
  alpha         = 0.20,
  adjust        = "fdr",
  label_mapping = labels_for_analysis
)

# interpret rate results
if (use_flipped) {
  rate_interp <- margot_interpret_rate(
    rate_results,
    flipped_outcomes      = flipped_names,
    adjust_positives_only = TRUE
  )
} else {
  rate_interp <- margot_interpret_rate(
    rate_results,
    flipped_outcomes      = NULL,  # no flipped outcomes
    adjust_positives_only = TRUE
  )
}

cat(rate_interp$comparison, "\n")
cli_h2("Analysis ready for Appendix ✔")

# organise model names by evidence strength
model_groups <- list(
  autoc       = rate_interp$autoc_model_names,
  qini        = rate_interp$qini_model_names,
  either      = rate_interp$either_model_names,
  exploratory = rate_interp$not_excluded_either
)

# ── 2. plot rate autoc curves (if any exist) ─────────────────────────────────
if (length(model_groups$autoc) > 0) {
  autoc_plots <- margot_plot_rate_batch(
    models        = models_for_analysis,
    save_plots    = FALSE,
    label_mapping = labels_for_analysis,
    model_names   = model_groups$autoc
  )
  
  # store first autoc name if it exists
  if (nrow(rate_results$rate_autoc) > 0) {
    autoc_name_1 <- rate_results$rate_autoc$outcome[[1]]
  }
} else {
  autoc_plots <- list()
  message("no significant rate autoc results found")
}

# ── 3. qini curves + gain interpretation ──────────────────────────────────────

# define plot settings (move here so they exist even if not used)
policy_tree_defaults <- list(
  point_alpha              = 0.5,
  title_size               = 12,
  subtitle_size            = 12,
  axis_title_size          = 12,
  legend_title_size        = 12,
  split_line_color         = "red",
  split_line_alpha         = 0.8,
  split_label_color        = "red",
  split_label_nudge_factor = 0.007
)

decision_tree_defaults <- list(
  span_ratio        = 0.1,
  text_size         = 4,
  y_padding         = 0.5,
  edge_label_offset = 0.02,
  border_size       = 0.01
)

# run initial qini analysis
qini_results <- margot_policy(
  models_for_analysis,
  save_plots         = FALSE,
  output_dir         = here::here(push_mods),
  decision_tree_args = decision_tree_defaults,
  policy_tree_args   = policy_tree_defaults,
  model_names        = names(models_for_analysis$results),
  original_df        = original_df,
  label_mapping      = labels_for_analysis,
  max_depth          = 2L,
  output_objects     = c("qini_plot", "diff_gain_summaries")
)

# interpret qini results
qini_gain <- margot_interpret_qini(
  qini_results,
  label_mapping = labels_for_analysis
)

print_rate(qini_gain$summary_table)
cat(qini_gain$qini_explanation, "\n")

reliable_ids <- qini_gain$reliable_model_ids

# ── 4. policy trees (only if reliable models exist) ──────────────────────────
if (length(reliable_ids) > 0) {
  # recompute for reliable models only
  qini_results_valid <- margot_policy(
    models_for_analysis,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = decision_tree_defaults,
    policy_tree_args   = policy_tree_defaults,
    model_names        = reliable_ids,
    original_df        = original_df,
    label_mapping      = labels_for_analysis,
    max_depth          = 2L,
    output_objects     = c("qini_plot", "diff_gain_summaries")
  )
  
  qini_plots <- map(qini_results_valid, ~ .x$qini_plot)
  qini_names <- margot_get_labels(reliable_ids, labels_for_analysis)
  
  # compute policy trees
  policy_results_2L <- margot_policy(
    models_for_analysis,
    save_plots         = FALSE,
    output_dir         = here::here(push_mods),
    decision_tree_args = decision_tree_defaults,
    policy_tree_args   = policy_tree_defaults,
    model_names        = reliable_ids,
    max_depth          = 2L,
    original_df        = original_df,
    label_mapping      = labels_for_analysis,
    output_objects     = c("combined_plot")
  )
  
  policy_plots <- map(policy_results_2L, ~ .x$combined_plot)
  
  # generate plain language interpretation
  policy_text <- margot_interpret_policy_batch(
    models        = models_for_analysis,
    original_df   = original_df,
    model_names   = reliable_ids,
    label_mapping = labels_for_analysis,
    max_depth     = 2L
  )
  
  cat(policy_text, "\n")
  
} else {
  qini_plots <- list()
  policy_plots <- list()
  qini_names <- character(0)
  policy_text <- "No reliable heterogeneous treatment effects found."
  message("no reliable qini models found - skipping policy tree analysis")
}

cli::cli_h1("Finished: heterogeneity analysis complete ✔")

# ──────────────────────────────────────────────────────────────────────────────
# OPTIONAL: PLANNED SUBGROUP COMPARISON
# uncomment this section if you want to do subgroup analysis
# ──────────────────────────────────────────────────────────────────────────────

# # ── subgroup comparison settings ──────────────────────────────────────────────
# x_offset_comp <- 1.0
# x_lim_lo_comp <- -1.0
# x_lim_hi_comp <- 1.0 
# 
# base_defaults_comparisons <- list(
#   type                  = "RD",
#   title                 = ate_title,
#   e_val_bound_threshold = 1.2,
#   label_mapping         = "label_mapping_all",
#   adjust                = "bonferroni",
#   alpha                 = 0.05,
#   colors                = c(
#     "positive"     = "#E69F00",
#     "not reliable" = "grey50",
#     "negative"     = "#56B4E9"
#   ),
#   x_offset              = x_offset_comp,
#   x_lim_lo              = x_lim_lo_comp,
#   x_lim_hi              = x_lim_hi_comp,
#   text_size             = 8,
#   linewidth             = 0.75,
#   estimate_scale        = 1,
#   base_size             = 18,
#   point_size            = 4,
#   title_size            = 19,
#   subtitle_size         = 16,
#   legend_text_size      = 10,
#   legend_title_size     = 10,
#   include_coefficients  = FALSE
# )
# 
# # define age-based subgroups
# complex_condition_age <- between(X[,"t0_age_z"], -1, 1)
# 
# # check age bounds on the raw scale
# mean(original_df$t0_age) + c(-1, 1) * sd(original_df$t0_age)
# 
# # age subsets
# subsets_standard_age <- list(
#   Younger = list(
#     var      = "t0_age_z",
#     value    = -1,
#     operator = "<",
#     label    = "Age < 35"
#   ),
#   Middle = list(
#     var              = "t0_age_z",
#     subset_condition = complex_condition_age,
#     label            = "Age 35-62"
#   ),
#   Older = list(
#     var      = "t0_age_z",
#     value    = 1,
#     operator = ">",
#     label    = "Age > 62"
#   )
# )
# 
# # run batch subgroup analysis
# planned_subset_results <- margot_planned_subgroups_batch(
#   domain_models = list(models_binary),  # always use models_binary for subgroups
#   X             = X,
#   base_defaults = base_defaults_comparisons,
#   subset_types  = list(cohort = subsets_standard_age),
#   original_df   = original_df,
#   label_mapping = label_mapping_all,  # always use standard labels
#   domain_names  = "wellbeing",
#   subtitles     = "",
#   adjust        = "bonferroni",
#   alpha         = 0.05
# )
# 
# # create comparison plot
# plots_subgroup_age_young_old <- wrap_plots(
#   list(
#     planned_subset_results$wellbeing$cohort$results$`Age < 35`$plot,
#     planned_subset_results$wellbeing$cohort$results$`Age > 62`$plot
#   ), ncol = 1) +
#   plot_annotation(
#     title = "Younger vs Older",
#     theme = theme(plot.title = element_text(size = 18, face = "bold"))
#   )
# 
# # compare young vs old groups
# group_comparison_age_young_old <- margot_compare_groups(
#   group1_name     = "People Under 35 Years Old",
#   group2_name     = "People Over 62 Years Old", 
#   planned_subset_results$wellbeing$cohort$results$`Age < 35`$transformed_table,
#   planned_subset_results$wellbeing$cohort$results$`Age > 62`$transformed_table,
#   type            = "RD",
#   decimal_places  = 3
# )

# ──────────────────────────────────────────────────────────────────────────────
# DEFINE GLOBAL VARIABLES FOR TEXT GENERATION
# ──────────────────────────────────────────────────────────────────────────────

global_vars <- list(
  name_exposure_variable     = nice_name_exposure,
  n_total                    = n_total,
  ate_adjustment             = "bonferroni", 
  ate_alpha                  = "0.05",
  cate_adjustment            = "Benjamini–Hochberg false-discovery-rate adjustment",
  cate_alpha                 = "0.1",             
  sample_ratio_policy        = "70/30",
  n_participants             = n_participants,
  exposure_variable          = name_exposure,
  name_exposure_lower        = name_exposure_lower,
  name_control_regime_lower  = name_control_regime_lower,
  name_outcome_variables     = "Self Esteem", # <- adjust to your study
  name_outcomes_lower        = name_outcomes_lower,
  name_exposure_capfirst     = nice_name_exposure,
  measures_exposure          = measures_exposure,
  value_exposure_regime      = value_exposure_regime,
  value_control_regime       = value_control_regime,
  flipped_list               = flipped_list,
  appendix_explain_grf       = "E",
  appendix_assumptions_grf   = "F",
  name_exposure_threshold    = "1",
  name_control_threshold     = "0",
  appendix_measures          = "A",
  value_control              = value_control,
  value_exposure             = value_exposure,
  appendix_positivity        = "C",
  appendix_rate              = "D",
  appendix_qini_curve        = "D",
  train_proportion_decision_tree = ".7",
  training_proportion        = ".7",
  sample_split               = "70/30",
  sample_ratio_policy        = "70/30",
  baseline_wave              = baseline_wave,
  exposure_waves             = exposure_waves,
  outcome_wave               = outcome_wave,
  protocol_url               = "https://osf.io/ce4t9/", # if used
  appendix_timeline          = "A" # if used
)
```

{{< pagebreak >}}

## Introduction

**Your place to shine here**

## Method

```{r, results='asis'}
#| eval: false # <- set to false, copy, delete, modify, and extend text as needed

# run this code, copy and paste contents into your text
cat(
  boilerplate::boilerplate_generate_text(
    category     = "methods",
    sections     = c(
      "student_sample.nzavs",
      "student_target_population",
      "eligibility.standard",
      "causal_intervention.grf_simple_text",
      "analytic_approach.general_approach_cate_long",
      "exposure_indicator",
      "causal_identification_criteria",
      "confounding_control.vanderweele",
      "statistical_models.grf_short_explanation",
      "missing_data.missing_grf_simple",
      "sensitivity_analysis.short_evalue"
    ),
    global_vars  = global_vars,
    db           = unified_db
  )
)
```

{{< pagebreak >}}

## Results

### Average Treatment Effects

```{r}
#| label: fig-ate
#| fig-cap: "Average Treatment Effects on Multi-dimensional Wellbeing"
#| eval: true
#| fig-height: 12
#| fig-width: 8
ate_results$plot
```

{{< pagebreak >}}

```{r}
#| label: tbl-outcomes
#| tbl-cap: "Average Treatment Effects on Multi-dimensional Wellbeing"
#| eval: true

margot_bind_tables_markdown
```

```{r, results = 'asis'}
#| eval: true # - set to false/ copy and change 

# run this line, copy and paste text into your document 
cat(ate_results$interpretation)
```

{{< pagebreak >}}

<!-- OPTIONAL SECTION: PLANNED SUBGROUP COMPARISONS -->
<!-- uncomment the section below if you performed subgroup analysis -->

<!-- ### Planned Subgroup Comparisons -->

<!-- Based on theoretical findings we expected that the effects of {name_exposure} would vary by age...\@fig-planned-comparison and @tbl-planned-comparison -->

<!-- ```{r} -->
<!-- #| label: fig-planned-comparison -->
<!-- #| fig-cap: "Planned Comparison Plot" -->
<!-- #| eval: true -->
<!-- #| echo: false -->
<!-- #| fig-height: 10 -->
<!-- #| fig-width: 12 -->
<!-- plots_subgroup_age_young_old -->
<!-- ``` -->

<!-- ```{r} -->
<!-- #| label: tbl-planned-comparison -->
<!-- #| tbl-cap: "Planned Comparison Table" -->
<!-- #| eval: true -->
<!-- #| echo: false -->
<!-- group_comparison_age_young_old$results |>  -->
<!--   mutate(across(where(is.numeric), ~ round(., 2))) %>% -->
<!--   kbl(format = "markdown") -->
<!-- ``` -->

<!-- ```{r, results = 'asis'} -->
<!-- #| eval: false # copy and paste your own text -->
<!-- cat(group_comparison_age_young_old$interpretation) -->
<!-- ``` -->

{{< pagebreak >}}

### Heterogeneous Treatment Effects {#results-qini-curve}

We begin by examining the distribution of individual treatment effects (τᵢ) across our sample. @fig-tau-distribution presents the estimated treatment effects for each individual, revealing substantial variability in how people respond to {name_exposure_lower}.

```{r}
#| label: fig-tau-distribution
#| fig-cap: "Distribution of Individual Treatment Effects (τᵢ) Across Outcomes"
#| eval: true
#| echo: false
#| fig-height: 12
#| fig-width: 10
#| 
# create tau plots showing individual treatment effect distributions
tau_plots <- margot_plot_tau(
  models_for_analysis, 
  label_mapping = labels_for_analysis
)

# display the plot
tau_plots
```

The histograms above show considerable heterogeneity in treatment effects across individuals. To determine whether this variability is systematic (i.e., predictable based on individual characteristics) rather than random noise, we employ two complementary approaches: Qini curves to assess the reliability of heterogeneous effects, and policy trees to identify subgroups with differential treatment responses.

```{r, results='asis'}
#| eval: false  # <- copy and modify text as needed

# copy and paste into your text
cat(
  boilerplate::boilerplate_generate_text(
    category     = "results",
    sections     = c("grf.interpretation_qini"),
    global_vars  = global_vars,
    db           = unified_db
  )
)
```

```{r, results = 'asis'}
#| eval: false  # <- set to true and use if you have reliable results

# only use if you have reliable qini results
if (length(reliable_ids) > 0) {
  cat(qini_gain$qini_explanation)
} else {
  cat("No significant heterogeneous treatment effects were detected using Qini curve analysis.")
}
```

<!-- only include this table if you have multiple qini results -->
```{r}
#| tbl-cap: "Qini Curve Results"
#| eval: true  # <- set to true if you have qini results

# only use if you have multiple qini results
if (length(reliable_ids) > 1) {
  knitr::kable(
    qini_gain$summary_table |> 
      mutate(across(where(is.numeric), ~ round(., 2))),
    format = "markdown",
    caption = "Qini Curve Results"
  )
} else {
  cat("*Note: Qini curve table only displayed when multiple significant results are found.*")
}
```

<!-- only include figures if you have qini results -->
```{r}
#| label: fig-qini-combined
#| fig-cap: "Qini Curves for Heterogeneous Treatment Effects"
#| eval: true # <- set to true if you have qini plots
#| echo: false
#| fig-height: 18
#| fig-width: 12

# only run if you have qini plots
if (length(qini_plots) > 0) {
  # create blank plot for spacing
  blank_plot <- plot_spacer()
  
  # determine grid layout (2 columns preferred)
  n_plots <- length(qini_plots)
  n_cols <- 2
  n_rows <- ceiling(n_plots / n_cols)
  
  # create list of plots including blank spacers for even grid
  plot_list <- qini_plots
  n_blanks_needed <- (n_rows * n_cols) - n_plots
  
  # add blank plots to fill the grid
  if (n_blanks_needed > 0) {
    for (i in 1:n_blanks_needed) {
      plot_list <- append(plot_list, list(blank_plot))
    }
  }
  
  # combine plots in a grid
  combined_qini <- wrap_plots(
    plot_list,
    ncol = n_cols,
    nrow = n_rows
  ) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = "Qini Curves for Reliable Heterogeneous Effects",
      subtitle = paste("Models with significant treatment effect heterogeneity (n =", 
                       n_plots, ")")
    ) &
    theme(
      legend.position   = "bottom",
      plot.title        = element_text(hjust = 0.5),
      plot.subtitle     = element_text(hjust = 0.5)
    )
  
  print(combined_qini)
} else {
  message("no qini plots to display")
}
```

{{< pagebreak >}}

### Decision Rules (Who is Most Sensitive to Treatment?)

```{r, results='asis'}
#| eval: false  # <- set to true and modify text as needed

cat(
  boilerplate::boilerplate_generate_text(
    category     = "results",
    sections     = c("grf.interpretation_policy_tree"),
    global_vars  = global_vars,
    db           = unified_db
  )
)
```

The following pages present policy trees for each outcome with reliable heterogeneous effects. Each tree shows: (1) the decision rules for treatment assignment, (2) the distribution of treatment effects across subgroups, and (3) visual representation of how covariates split the population into groups with differential treatment responses.

<!-- the following policy tree figures are optional -->
<!-- only include as many as you have valid policy trees -->

```{r}
#| label: fig-policy-trees
#| fig-cap: "Policy Trees for Treatment Assignment"
#| eval: true # <- set to true if you have policy trees
#| echo: false
#| fig-height: 10
#| fig-width: 12

# display policy trees if they exist - one per page
if (length(policy_plots) > 0) {
  # iterate through each policy tree
  for (i in seq_along(policy_plots)) {
    # add page break before each plot except the first
    if (i > 1) {
      cat("\n\n{{< pagebreak >}}\n\n")
    }
    
    # create individual caption for each tree
    cat(paste0("\n\n#### Policy Tree ", i, ": ", qini_names[[i]], "\n\n"))
    
    # print the policy tree
    print(policy_plots[[i]])
    
    # add some space after
    cat("\n\n")
  }
} else {
  message("no policy trees to display")
}
```

```{r, results = 'asis'}
#| eval: false # <- copy and paste text a insert jsust below graph

# use this text below your decision tree graphs
if (length(reliable_ids) > 0) {
  cat(policy_text, "\n")
}
```

{{< pagebreak >}}

## Discussion

```{r,  results='asis'}
#| eval: false  # <- set to true and modify text as needed
#| echo: false

cat(boilerplate_generate_text(
  category = "discussion",
  sections = c(
    "student_ethics",
    "student_data",
    "student_authors_statement"
  ),
  global_vars = list(
    exposure_variable = name_exposure
  ),
  db = unified_db
))

```

{{< pagebreak >}}

## Appendix A: Measures {#appendix-measures}

### Measures

#### Baseline Covariate Measures

```{r, results='asis'}
cat(baseline_measures_text)
```

#### Exposure Measures

```{r, results='asis'}
cat(exposure_measures_text)
```

#### Outcome Measures

```{r, results='asis'}
cat(outcome_measures_text)
```

{{< pagebreak >}}

## Appendix B: Sample Characteristics {#appendix-sample}

#### Sample Statistics: Baseline Covariates

@tbl-appendix-baseline presents sample demographic statistics.

::: {#tbl-appendix-baseline}
```{r, results = 'asis'}
#| eval: true
#| include: true
#| echo: false
markdown_table_baseline
```

Demographic statistics for New Zealand Attitudes and Values Cohort: {baseline_wave_glued}.
:::

### Sample Statistics: Exposure Variable {#appendix-exposure}

::: {#tbl-appendix-exposures}
```{r, results = 'asis'}
#| eval: true
#| include: true
#| echo: false

markdown_table_exposures
```

Demographic statistics for New Zealand Attitudes and Values Cohort waves 2018.
:::

{{< pagebreak >}}

### Sample Statistics: Outcome Variables {#appendix-outcomes}

::: {#tbl-appendix-outcomes}
```{r, results = 'asis'}
#| eval: true
#| include: true
#| echo: false

markdown_table_outcomes
```

Outcome variables measured at {baseline_wave_glued} and {outcome_wave}
:::

{{< pagebreak >}}

## Appendix C: Transition Matrix to Check The Positivity Assumption {#appendix-transition}

```{r, results = 'asis'}
#| label: tbl-transition
#| tbl-cap: "Transition Matrix Showing Change"
#| eval: true
#| include: true
#| echo: false

transition_tables_binary$tables[[1]]
```

```{r, results = 'asis'}
cat(transition_tables_binary$explanation)
```

{{< pagebreak >}}

## Appendix D: RATE AUTOC and RATE Qini {#appendix-rate}

```{r, results='asis'}
#| eval: false # <- set to true and modify text as needed
#| echo: false

# select appropriate text based on whether outcomes were flipped
if (use_flipped) {
  cat(
    boilerplate::boilerplate_generate_text(
      category     = "results",
      sections     = c("grf.interpretation_rate"),
      global_vars  = global_vars,
      db           = unified_db
    )
  )
} else {
  # use no-flip version if available
  if (!is.null(unified_db$results$grf$interpretation_rate_no_flip)) {
    cat(unified_db$results$grf$interpretation_rate_no_flip)
  } else {
    cat(
      boilerplate::boilerplate_generate_text(
        category     = "results",
        sections     = c("grf.interpretation_rate"),
        global_vars  = global_vars,
        db           = unified_db
      )
    )
  }
}
```

```{r, results='asis'}
#| eval: false # <- set to true as needed
#| echo: false
cat(rate_interp$comparison)
```

Refer to [Appendix D](#appendix-cate-validation) for details.

##### RATE AUTOC RESULTS

```{r, results = 'asis'}
# only show if there are autoc results
if (length(model_groups$autoc) > 0) {
  cat(rate_interp$autoc_results)
} else {
  cat("No significant RATE AUTOC results were found.")
}
```

<!-- only include autoc plots if they exist -->
```{r}
#| label: fig-rate-autoc
#| fig-cap: "RATE AUTOC Curves"
#| eval: false # <- set to true if you have autoc plots
#| echo: false
#| fig-height: 16
#| fig-width: 12

# display autoc plots if they exist
if (length(autoc_plots) > 0) {
  # create blank plot for spacing
  blank_plot <- plot_spacer()
  
  # determine grid layout (2 columns preferred)
  n_plots <- length(autoc_plots)
  n_cols <- 2
  n_rows <- ceiling(n_plots / n_cols)
  
  # create list of plots including blank spacers for even grid
  plot_list <- autoc_plots
  n_blanks_needed <- (n_rows * n_cols) - n_plots
  
  # add blank plots to fill the grid
  if (n_blanks_needed > 0) {
    for (i in 1:n_blanks_needed) {
      plot_list <- append(plot_list, list(blank_plot))
    }
  }
  
  # combine plots in a grid
  combined_autoc <- wrap_plots(
    plot_list,
    ncol = n_cols,
    nrow = n_rows
  ) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = "RATE AUTOC Curves for Heterogeneous Effects",
      subtitle = paste("Outcomes with significant autocorrelation (n =", 
                       n_plots, ")")
    ) &
    theme(
      legend.position   = "bottom",
      plot.title        = element_text(hjust = 0.5),
      plot.subtitle     = element_text(hjust = 0.5)
    )
  
  print(combined_autoc)
} else {
  message("no autoc plots to display")
}
```

{{< pagebreak >}}

## Appendix E: Estimating and Interpreting Heterogeneous Treatment Effects with GRF {#appendix-explain-grf}

```{r, results='asis'}
#| eval: false # <- set to true as needed
#| echo: false

cat(
  boilerplate::boilerplate_generate_text(
    category     = "appendix",
    sections     = c("explain.grf_short"),
    global_vars  = global_vars,
    db           = unified_db
  )
)
```

{{< pagebreak >}}

## Appendix F: Strengths and Limitations of Causal Forests {#appendix-strengths}

```{r, results='asis'}
#| eval: false # <- set to true and modify text as needed

cat(
  boilerplate::boilerplate_generate_text(
    category     = "discussion",
    sections     = c("strengths.strengths_grf_short"),
    global_vars  = global_vars,
    db           = unified_db
  )
)
```

{{< pagebreak >}}

## References {.appendix-refs}