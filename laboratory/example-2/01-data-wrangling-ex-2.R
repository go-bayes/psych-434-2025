# example 2 script 1: data wrangling
# may 2025
# example estimation of average treatment effect - script 1
# questions: joseph.bulbulia@vuw.ac.nz

# restart fresh session if needed
# rstudioapi::restartSession()



# set seed ----------------------------------------------------------------

# set seed for reproducibility
set.seed(123)


# load libraries ---------------------------------------------------------
# install and load 'margot' package if not already installed
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot") # ensure version is at least 1.0.32
  library(margot)
}

# install/  load pacman
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}



# load required packages -------------------------------------------------------
pacman::p_load(
  # causal inference
  clarify,      # sensitivity analysis
  cobalt,       # covariate balance tables and plots
  lmtp,         # longitudinal targeted maximum likelihood estimation
  margot,       # functions for causal inference
  MatchIt,      # matching methods
  MatchThem,    # matching for multiply imputed datasets
  policytree,   # causal inference with policy trees
  WeightIt,     # weighting methods for covariate balancing
  
  # data processing
  data.table,   # fast data wrangling
  fastDummies,  # fast creation of dummy variables
  fs,           # cross-platform file system operations
  qs,           # saving
  here,         # simple and robust file referencing
  janitor,      # data cleaning and validation
  naniar,       # handling and visualization of missing data
  skimr,        # summary statistics for data frames
  tidyverse,    # collection of "R" packages for data science
  
  # machine learning
  glmnet,       # lasso and elastic-net regularized models
  grf,          # generalized random forests
  ranger,       # fast implementation of random forests
  SuperLearner, # ensemble learning
  xgboost,      # extreme gradient boosting
  
  # visualization
  DiagrammeR,   # graph and network visualization
  ggbeeswarm,   # data visualization   
  ggplot2,      # data visualization
  kableExtra,   # advanced table formatting
  
  # parallel processing
  doParallel,   # parallel processing with foreach
  progressr,    # progress reporting for "R"
  
  # analysis
  parameters,   # parameters and performance metrics
  EValue        # compute e-values
)

# check package versions
packageVersion(pkg = 'margot')    # make sure it is 1.0.19 or greater
packageVersion(pkg = 'boilerplate')



# save paths -------------------------------------------------------------------

# data path
pull_data <- here::here("data")

# create a folder called "models" in the main directory
dir.create("models_example_2")

# then create a link to this directory using `here::here()`
push_mods <- here::here("models_example_2")

# read data from saved file
df_nz_long <- margot::here_read_qs("df_nz_long", pull_data)


# prepare initial dataframe -----------------------------------------------
dat_prep <- df_nz_long |>
  arrange(id, wave) |>
  as.data.frame() |>
  margot::remove_numeric_attributes() |>
  mutate(sample_weights = sample_weights) |> 
  mutate(alcohol_intensity = if_else(alcohol_intensity >=15, 15, alcohol_intensity)) |>
  mutate(heavy_drinker = ifelse(df_nz_long$alcohol_frequency %in% c(3, 4), 1,
                                ifelse(df_nz_long$alcohol_frequency %in% c(0, 1, 2), 0, NA))) |> 
  mutate(
    alcohol_frequency_weekly = case_when(
      alcohol_frequency == 0 ~ 0,
      alcohol_frequency == 1 ~ 0.25,
      alcohol_frequency == 2 ~ 1,
      alcohol_frequency == 3 ~ 2.5,
      alcohol_frequency == 4 ~ 4.5,
      alcohol_frequency == 5 ~ NA_real_, # assign NA for 'Don't know'
      TRUE ~ NA_real_ # handles any unexpected values as NA
    )
  ) |> 
  mutate(religion_church = round( ifelse(religion_church > 8, 8, religion_church)),1) |>  # to simplify
  arrange(id, wave) |>
  droplevels()



# define study variables --------------------------------------------------------

# view variable names in the dataset
glimpse(dat_prep)

# count the number of unique participants
length(unique(dat_prep$id))

# define exposure variable
name_exposure <- c("extraversion") # ← ** define for your study **

var_labels_exposure = c("extraversion" = "Extraversion",# ← ** define for your study **
                        "extraversion_binary" = "Extraversion (binary)") # ← ** define **

# save variable labels for manuscript
here_save(var_labels_exposure, "var_labels_exposure")

# define variable names for binary exposure
name_exposure_binary <- paste0(name_exposure, "_binary")
t0_name_exposure_continuous <- paste0("t0_", name_exposure)
t0_name_exposure_binary <- paste0("t0_", name_exposure, "_binary")

# define wide variable names
t0_name_exposure <- paste0("t0_", name_exposure)
t0_name_exposure_continuous <- paste0("t0_", name_exposure)
t0_name_exposure_binary <- paste0("t0_", name_exposure, "_binary")

# define variable names for exposures used in models
t1_name_exposure <- paste0("t1_", name_exposure)
t1_name_exposure_binary <- paste0("t1_", name_exposure, "_binary")

# define study waves -----------------------------------------------------------
baseline_wave <- "2018"
exposure_waves <- c("2019")
outcome_wave <- "2020"

# define wave combinations for analysis
all_waves <- c(baseline_wave, exposure_waves, outcome_wave)
baseline_and_exposure_waves <- c(baseline_wave, exposure_waves)
baseline_and_outcome_waves <- c(baseline_wave, outcome_wave)

# define scale ranges ----------------------------------------------------------
scale_range_exposure <- c(1, 7)  # used for descriptive graphs
scale_ranges_outcomes <- c(1, 7)  # used for descriptive graphs

# data preparation -------------------------------------------------------------
# import and prepare data

# get total sample size
n_total <- skimr::n_unique(df_nz_long$id)
n_total <- margot::pretty_number(n_total)
margot::here_save(n_total, "n_total")

# view
n_total

# Define Baseline Variables ----------------------------------------------------
baseline_vars <- c(
  # note all outcomes will be added to baseline vars later. 
  # demographics
  "age",
  "born_nz_binary",
  "education_level_coarsen",
  "employed_binary",
  "eth_cat",
  "male_binary",
  "not_heterosexual_binary",
  "parent_binary",
  "partner_binary",
  "rural_gch_2018_l",
  "sample_frame_opt_in_binary",
  
  # personality traits
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "neuroticism",
  "openness",
  
  # health and lifestyle
  "alcohol_frequency",
  "alcohol_intensity",
  "hlth_disability_binary",
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_household_inc",
  # "log_hours_community", # commented out because using as exposure
  "short_form_health",
  "smoker_binary",
  
  # social and psychological
  "belong",
  "nz_dep2018",
  "nzsei_13_l",
  "political_conservative",
  "religion_identification_level"
)

# Sort baseline variables
baseline_vars <- sort(baseline_vars)


# baseline vars no log ----------------------------------------------------

# for individual plots
all_waves <- c(baseline_wave, exposure_waves, outcome_wave)
baseline_and_exposure_waves <- c(baseline_wave, exposure_waves)
baseline_and_outcome_waves <- c(baseline_wave, outcome_wave)

# define baseline variables
baseline_vars <- c(
  "age",
  "agreeableness",
  "alcohol_frequency_weekly", 
  "alcohol_intensity",
  "belong",
  "born_nz_binary",
  "conscientiousness",
  "education_level_coarsen",
  "employed_binary",
  "eth_cat",
  "extraversion",
  "hlth_disability_binary",
  "hlth_fatigue",
  "honesty_humility",
  "kessler_latent_anxiety",  # will be added as outcomes
  "kessler_latent_depression", # will be added as outcomes
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_household_inc",
  "log_hours_community",
  "male_binary",
  "neuroticism",
  "not_heterosexual_binary",
  "nz_dep2018",
  "nzsei_13_l",
  "openness",
  "parent_binary",
  "partner_binary",
  "political_conservative",
  "religion_identification_level", 
  "religion_church_binary", 
  "rural_gch_2018_l",
  "rwa",
  "sample_frame_opt_in_binary", 
  "sdo", 
  "short_form_health", # will be added as but use as standard
  "smoker_binary" #,
)

# define baseline variables without log transformation
baseline_vars_no_log <- c(
  baseline_vars,
  c(
    "hours_children",
    "hours_commute",
    "hours_exercise",
    "hours_housework",
    "hours_community",
    "household_inc"
  )
)


# sort baseline variables
baseline_vars <- sort(baseline_vars)
baseline_vars_log <- sort(baseline_vars_no_log)

# outcomes
outcome_vars <- c(
  "alcohol_frequency_weekly", 
  "alcohol_intensity",
  "belong",
  "bodysat",
  "forgiveness",
  "gratitude",
  "hlth_bmi",
  "hlth_fatigue",
  "hlth_sleep_hours",
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "lifesat",
  "log_hours_exercise",
  "meaning_purpose", 
  "meaning_sense",
  "neighbourhood_community",
  # "perfectionism", #  *--* excluded (exposure ) *--*
  "pwi", 
  "rumination",
  "self_control",
  "self_esteem",
  "sexual_satisfaction",
  "short_form_health",
  "support"
)

# sort
outcome_vars <- sort(outcome_vars)

# save outcomes
here_save(outcome_vars, "outcome_vars")

# define outcome variables without log transformation
outcome_vars_no_log <- c(outcome_vars,"hours_exercise")

# sort
outcome_vars_no_log <- sort(outcome_vars_no_log)

# save outcomes
here_save(outcome_vars_no_log, "outcome_vars_no_log")

# raw outcomes 
raw_outcomes_health <- c(
  "alcohol_frequency_weekly", 
  "alcohol_intensity",
  "hlth_bmi", 
  "log_hours_exercise", 
  "hlth_sleep_hours", 
  "short_form_health"
)
# sort
raw_outcomes_health <- sort(raw_outcomes_health)

# save 
here_save(raw_outcomes_health, "raw_outcomes_health")

# with no log
raw_outcomes_health_no_log <- c(raw_outcomes_health, "hours_exercise")

# sort
raw_outcomes_health_no_log <- sort(raw_outcomes_health_no_log)

# save 
here_save(raw_outcomes_health_no_log, "raw_outcomes_health_no_log")

# define psych outcomes
raw_outcomes_psych <- c( 
  "hlth_fatigue", 
  "kessler_latent_anxiety", 
  "kessler_latent_depression",  
  "rumination"
)

# sort
raw_outcomes_psych <- sort(raw_outcomes_psych)

# save
here_save(raw_outcomes_psych, "raw_outcomes_psych")

# define present outcomes
raw_outcomes_present <- c(
  "bodysat",
  "forgiveness",
  # "perfectionism",  *--* excluded  *--*
  "self_control" , 
  "self_esteem", 
  "sexual_satisfaction" )

# sort
raw_outcomes_present <- sort(raw_outcomes_present)

# save
here_save(raw_outcomes_present, "raw_outcomes_present")

# define life outcomes
raw_outcomes_life <- c( 
  "gratitude", 
  "lifesat", 
  "meaning_purpose", 
  "meaning_sense",
  "pwi"  # move personal well-being here if not using individual facents
)

# sort
raw_outcomes_life <- sort(raw_outcomes_life)

# save
here_save(raw_outcomes_life, "raw_outcomes_life")

# define social outcomes
raw_outcomes_social <- c(
  "belong",
  "neighbourhood_community", 
  "support" 
)

# sort
raw_outcomes_social <- sort(raw_outcomes_social)

# save
here_save(raw_outcomes_social, "raw_outcomes_social")

# create all outcome variable names ---------------------------------------
# for tables
raw_outcomes_all = c(
  baseline_vars_no_log,
  raw_outcomes_health_no_log,
  raw_outcomes_psych,
  raw_outcomes_present,
  raw_outcomes_life,
  raw_outcomes_social
)

# select only unique measures
raw_outcomes_all <- unique(raw_outcomes_all)

# save
here_save(raw_outcomes_all, "raw_outcomes_all")


# set time varying confounding --------------------------------------------

# those vars that are commented out are included as time-varying confounders

confounder_vars <- c(
  "hlth_disability_binary"#,
  # outcome_vars # not for grf
)

# select only unique
confounder_vars <- unique(confounder_vars)

# view
confounder_vars

# save
here_save(confounder_vars, "confounder_vars")

# names for outcomes ------------------------------------------------------
# define extra variables
extra_vars <- c("id", "wave", "year_measured", "sample_weights")

# combine all variables
all_vars_prep <- c(baseline_vars, exposure_var, outcome_vars, extra_vars)

# get unique
all_vars <- unique(all_vars_prep)

# sort
all_vars <- sort(all_vars)

# define extra variables for table
extra_vars_table <- c("id", "wave", "year_measured")
not_all_vars_prep <- c(baseline_vars_no_log, exposure_var, outcome_vars_no_log, extra_vars_table)

# get unique
not_all_vars_prep <- unique(not_all_vars_prep)

# sort
not_all_vars <- sort(not_all_vars_prep)

# define columns that will later be handled in a special way
# define continuous columns that we will not standardise
continuous_columns_keep <- c("t0_sample_weights")

# define ordinal columns that we will expand into binary variables
ordinal_columns <- c("t0_education_level_coarsen", "t0_eth_cat", "t0_rural_gch_2018_l")

# eligibility  ------------------------------------------------------------
# select baseline and exposure ids based on eligibility criteria
ids_baseline <- dat_prep |>
  filter(year_measured == 1, wave == baseline_wave) |>
  filter(!is.na(!!sym(name_exposure))) |>
  pull(id)

# if we are allowing missing values in the exposure then ids_study are the ids at baseline
ids_study <- ids_baseline

# get n
n_participants<- length(ids_study)

# save after making a nice number
n_participants <- margot::pretty_number(n_participants)

# check
n_participants

# save
here_save(n_participants, "n_participants")

# filter data
dat_long_1 <- dat_prep |>
  filter(id %in% ids_study & wave %in% c(baseline_wave, exposure_waves, outcome_wave)) |>
  droplevels() # note that we might have more than one exposure wave

# check
head(dat_long_1)

# check
dim(dat_long_1)

# shift intervention graphs -----------------------------------------------
# # see appendix for more styles
# max_exposure <- max(dat_long_1[[name_exposure]], na.rm=TRUE)
# min_exposure <- min(dat_long_1[[name_exposure]], na.rm=TRUE)
# 
# # get data for baseline wave and exposure waves
# dat_shift <- dat_long_1 |> 
#   filter((wave %in% c(baseline_wave, exposure_waves))) |> 
#   select(all_of(name_exposure)) |> 
#   drop_na()
# 
# # shift conditions
# # up
# shift_exposure_up <- margot_plot_shift(
#   dat_shift,
#   col_name = name_exposure,
#   label_mapping = var_labels_exposure,
#   shift = "down",
#   range_highlight = c(3.1, 7),
#   binwidth = .25,
#   save_path = here::here(push_mods),
#   show_avg_line = TRUE
# )
# 
# # view
# shift_exposure_up

# binary graph ------------------------------------------------------------

# get only exposure wave
dat_long_exposure <- dat_long_1 |> filter(wave %in% exposure_waves ) |> droplevels()


# make a plot to evaluate cut points
graph_exposure_binary <- margot_plot_categorical(
  dat_long_exposure,
  label_mapping = var_labels_exposure,
  col_name = name_exposure,
  cutpoint_inclusive = "upper",
  sd_multipliers = c(-1, 1),
  show_mean = TRUE,
  show_median = FALSE,
  show_sd = TRUE,
  # n_divisions = 2, #  ← ** define **
  custom_breaks = c(1,4),#  ← ** define **
  binwidth = .2)

# view
print(graph_exposure_binary)



# end plot ----------------------------------------------------------------

# create categorical variable ---------------------------------------------
dat_long_2 <- create_ordered_variable(
  dat_long_1,  # make sure this is correct
  var_name = name_exposure,
  cutpoint_inclusive = "upper",
  #n_divisions = 2
  custom_breaks = c(1,4),
)

# view
table( dat_long_2$religion_church_binary) 



# convert binary factors to 0, 1 -----------------------------------------
# we do this using a custom function

# religion church binary already named "binary"
dat_long_3 <- margot::margot_process_binary_vars(dat_long_2, exceptions = "religion_church_binary")


dat_long_4 <- dat_long_3




# log-transform 'hours_' variables ----------------------------------------

dat_long_table <- margot_log_transform_vars(
  dat_long_4,
  vars = c(starts_with("hours_"), "household_inc"),
  # consider gen_cohort if used
  #  exceptions = "hours_work", no exceptions
  prefix = "log_",
  keep_original = TRUE ## Set to FALSE
) |>
  select(all_of(not_all_vars)) |>
  droplevels()

# view
dat_long_table


# baseline outcome waves --------------------------------------------------
# make tables ----------------------------------------------------------

var_labels_health <- list(
  "alcohol_frequency_weekly" = "Alcohol Frequency (weekly)", 
  "alcohol_intensity" = "Alcohol Intensity", 
  "hlth_bmi" = "Body Mass Index", 
  "hlth_sleep_hours" = "Sleep", 
  "hours_exercise" = "Hours of Exercise",   #logged in models
  "short_form_health" = "Short Form Health" 
)

# define psych outcomes 
var_labels_psych<- list(
  "hlth_fatigue" = "Fatigue", 
  "kessler_latent_anxiety" = "Anxiety", 
  "kessler_latent_depression" = "Depression",  
  "rumination" = "Rumination"
)

# define present outcomes
var_labels_present<- list(
  "bodysat" = "Body Satisfaction",
  "foregiveness" = "Forgiveness",
  # "perfectionism" = "Perfectionism",  
  "self_control" = "Self Control",  
  "self_esteem" = "Self Esteem", 
  "sexual_satisfaction" = "Sexual Satisfaction"  )

# define life outcomes
var_labels_life <- list(
  "gratitude" = "Gratitude", 
  "lifesat" = "Life Satisfaction", 
  "meaning_purpose" = "Meaning: Purpose", # exposure variable
  "meaning_sense" = "Meaning: Sense",
  "pwi = Personal Well-being Index"
)

# define social outcome names
var_labels_social <- list(
  "belong" = "Social Belonging",
  "neighbourhood_community" = "Neighbourhood Community", 
  "support" = "Social Support" 
)


# make labels
var_labels_baseline <- list(
  "sdo" = "Social Dominance Orientation",
  "belong" = "Social Belonging",
  "born_nz_binary" = "Born in NZ",
  "rural_gch_2018_l" = "Rural Gch 2018 Levels",
  "education_level_coarsen" = "Education Level",
  "employed_binary" = "Employed (binary)",
  "eth_cat" = "Ethnicity",
  "household_inc" = "Household Income",
  "log_household_inc" = "Log Household Income",
  "male_binary" = "Male (binary)",
  "nz_dep2018" = "NZ Deprevation Index 2018",
  "nzsei_13_l" = "NZSEI (Occupational Prestige Index)",
  "parent_binary" = "Parent (binary)",
  "rwa" = "Right Wing Authoritarianism",
  "sample_frame_opt_in_binary" = "Sample Frame Opt-In (binary)",
  "sdo" = "Social Dominance Orientation",
  "smoker_binary" = "Smoker (binary)",
  "support" = "Social Support" 
)




# labels ---------------------------------------------------------------------

var_labels_health <- list(
  "alcohol_frequency_weekly" = "Alcohol Frequency (weekly)", 
  "alcohol_intensity" = "Alcohol Intensity", 
  "hlth_bmi" = "Body Mass Index", 
  "hlth_sleep_hours" = "Sleep", 
  "hours_exercise" = "Hours of Exercise",   #logged in models
  "short_form_health" = "Short Form Health" 
)

# define psych outcomes 
var_labels_psych<- list(
  "hlth_fatigue" = "Fatigue", 
  "kessler_latent_anxiety" = "Anxiety", 
  "kessler_latent_depression" = "Depression",  
  "rumination" = "Rumination"
)

# define present outcomes
var_labels_present<- list(
  "bodysat" = "Body Satisfaction",
  "foregiveness" = "Forgiveness",
  "perfectionism" = "Perfectionism",  
  "self_control" = "Self Control",  
  "self_esteem" = "Self Esteem", 
  "sexual_satisfaction" = "Sexual Satisfaction"  
)

# define life outcomes
var_labels_life <- list(
  "gratitude" = "Gratitude", 
  "lifesat" = "Life Satisfaction", 
  "meaning_purpose" = "Meaning: Purpose", # exposure variable
  "meaning_sense" = "Meaning: Sense",
  "pwi = Personal Well-being Index"
)

# define social outcome names
var_labels_social <- list(
  "belong" = "Social Belonging",
  "neighbourhood_community" = "Neighbourhood Community", 
  "support" = "Social Support" 
)


# make labels
var_labels_baseline <- list(
  "sdo" = "Social Dominance Orientation",
  "belong" = "Social Belonging",
  "born_nz_binary" = "Born in NZ",
  "rural_gch_2018_l" = "Rural Gch 2018 Levels",
  "education_level_coarsen" = "Education Level",
  "employed_binary" = "Employed (binary)",
  "eth_cat" = "Ethnicity",
  "household_inc" = "Household Income",
  "log_household_inc" = "Log Household Income",
  "male_binary" = "Male (binary)",
  "nz_dep2018" = "NZ Deprevation Index 2018",
  "nzsei_13_l" = "NZSEI (Occupational Prestige Index)",
  "parent_binary" = "Parent (binary)",
  "rwa" = "Right Wing Authoritarianism",
  "sample_frame_opt_in_binary" = "Sample Frame Opt-In (binary)",
  "sdo" = "Social Dominance Orientation",
  "smoker_binary" = "Smoker (binary)",
  "support" = "Social Support" 
)

# make variable labels for all measures -----------------------------------
var_labels_all = c(
  var_labels_baseline,
  var_labels_exposure,
  var_labels_health,
  var_labels_psych,
  var_labels_present,
  var_labels_life,
  var_labels_social
)
var_labels_all

# save for manuscript
here_save(var_labels_all, "var_labels_all")


# make table --------------------------------------------------------------
# labels for factors
rural_labels <- c(
  "High Urban Accessibility",
  "Medium Urban Accessibility",
  "Low Urban Accessibility",
  "Remote",
  "Very Remote"
)

# new df for table
dat_long_table_x <- dat_long_table
dat_long_table_x$rural_gch_2018_l <- factor(
  dat_long_table_x$rural_gch_2018_l,
  levels = 1:5,
  labels = rural_labels,
  ordered = TRUE  # Optional: if the levels have an inherent order
)

# only use if using the frequency church variable
# dat_long_table_x$religion_church <- factor(
#   dat_long_table_x$religion_church,
#   levels = 0:8,
#   ordered = TRUE  # Optional: if the levels have an inherent order
# ) 


dat_long_table_baseline = dat_long_table_x |>
  filter(wave %in% c(baseline_wave)) |>
  mutate(
    male_binary = factor(male_binary),
    parent_binary = factor(parent_binary),
    smoker_binary = factor(smoker_binary),
    born_nz_binary = factor(born_nz_binary),
    employed_binary = factor(employed_binary),
    not_heterosexual_binary = factor(not_heterosexual_binary),
    sample_frame_opt_in_binary = factor(sample_frame_opt_in_binary)
  )


dat_long_table_exposure_waves = dat_long_table_x |> 
  filter(wave %in% c(baseline_wave, exposure_waves))

dat_long_table_outcome_waves = dat_long_table_x |> 
  filter(wave %in% c(baseline_wave, outcome_wave))

# make tables
markdown_table_baseline <- margot::margot_make_tables(
  data = dat_long_table_baseline,
  vars = baseline_vars_no_log,
  by = "wave",
  labels = var_labels_baseline,
  factor_vars = c("rural_gch_2018_l", "eth_cat"),
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"#,
  # kable_opts = list(
  #   booktabs = TRUE,
  #   longtable = TRUE,
  #   font_size = 6,
  #   latex_options = c("hold_position", "repeat_header", "striped", "longtable")
  # )
)

# view
markdown_table_baseline

# save
margot::here_save(markdown_table_baseline, "markdown_table_baseline")

# make tables
markdown_table_exposures <- margot::margot_make_tables(
  data = dat_long_table_exposure_waves,
  vars = exposure_var,
  by = "wave",
  labels = var_labels_exposure,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"#,
  # kable_opts = list(
  #   booktabs = TRUE,
  #   longtable = TRUE,
  #   font_size = 6,
  #   latex_options = c("hold_position", "repeat_header", "striped", "longtable")
  # )
)


# view
markdown_table_exposures

# save
here_save(markdown_table_exposures, "markdown_table_exposures")


dat_long_table_outcome_waves = dat_long_table_x |> 
  filter(wave %in% c(baseline_wave, outcome_wave))

# make tables
markdown_table_outcomes_all <- margot::margot_make_tables(
  data = dat_long_table_outcome_waves,
  vars = outcome_vars_no_log,
  by = "wave",
  labels = var_labels_all,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"#,
  # kable_opts = list(
  #   booktabs = TRUE,
  #   longtable = TRUE,
  #   font_size = 6,
  #   latex_options = c("hold_position", "repeat_header", "striped", "longtable")
  # )
)
# view
markdown_table_outcomes_all

# save
here_save(markdown_table_outcomes_all, "markdown_table_outcomes_all")


# data for study, remove originals ----------------------------------------
## create gender weights if needed
dat_long_selected <- dat_long_4

# not needed in this study
# dat_long_selected$gender_weights <- margot_compute_gender_weights_by_wave(
#   dat_long_selected,
#   male_col = "male_binary",
#   wave_col = "wave",
#   target_male_prop = 0.48
# )

# set logs
dat_long_prepare <- margot::margot_log_transform_vars(
  dat_long_selected,
  vars = c(starts_with("hours_"), "household_inc"),
  #  exceptions = "hours_work", no exceptions
  prefix = "log_",
  keep_original = TRUE ## Set to FALSE
) |>
  select(all_of(all_vars)) |>
  droplevels()

# check
margot::here_save(dat_long_prepare, "dat_long_prepare")

# check
colnames(dat_long_prepare)



# get baseline missingness ------------------------------------------------

# get baseline data
dat_baseline <- dat_long_prepare |>
  filter(wave == baseline_wave)

# sample weights
t0_sample_weights <- dat_baseline$sample_weights

margot::here_save(t0_sample_weights, "t0_sample_weights")


# transition matrices -----------------------------------------------------
# used to verify positivity
dt_positivity <- dat_long_table_exposure_waves |>
  select(!!sym(name_exposure), id, wave, year_measured) |>
  mutate(exposure= round( as.numeric(!!sym(name_exposure))),0) |>
  # for this study
  mutate(exposure_binary = ifelse(exposure>=4, 1, 0)) |>
  droplevels()
dt_positivity$wave <- as.numeric((dt_positivity$wave))

# view
transition_table <- margot::margot_transition_table(
  dt_positivity,
  state_var = "exposure",
  id_var = "id",
  observed_var = "year_measured",
  observed_val = 1,
  waves = c(1:2),
  wave_var = "wave",
  table_name = "transition_table"
)
# explanation
cat(transition_table$explanation)

# tables
transition_table$tables[[1]]


# for publication
transition_table$quarto_code()

# save
here_save(transition_tables, "transition_tables")


# binary table
# view
transition_tables_binary <- margot::margot_transition_table(
  dt_positivity,
  state_var = "exposure_binary",
  id_var = "id",
  observed_var = "year_measured",
  observed_val = 1,
  waves = c(1:2),
  wave_var = "wave",
  table_name = "transition_table_binary"
)
# explanation
cat(transition_tables_binary$explanation)

# tables
transition_tables_binary$tables[[1]]

# for publication
transition_tables_binary$quarto_code()


# check missing values ---------------------------------------------------
# and look for other problems
naniar::miss_var_summary(dat_long_prepare) |> print(n = 100)
naniar::gg_miss_var(dat_long_prepare)
naniar::vis_miss(dat_long_prepare, warn_large_data = FALSE)

dat_baseline <- dat_long_prepare |> filter(wave == baseline_wave)

# make percentage missing at baseline
percent_missing_baseline <- naniar::pct_miss(dat_baseline)

# save
here_save(percent_missing_baseline, "percent_missing_baseline")

# view
percent_missing_baseline


# If everything looks OK, save the data and indicators --------------------
# save the data and indicators
margot::here_save(name_exposure, "name_exposure")
margot::here_save(baseline_vars, "baseline_vars")
margot::here_save(exposure_var, "exposure_var")
margot::here_save(outcome_vars, "outcome_vars")
margot::here_save(baseline_wave, "baseline_wave")
margot::here_save(exposure_waves, "exposure_waves")
margot::here_save(outcome_wave, "outcome_wave")
margot::here_save(extra_vars, "extra_vars")
margot::here_save(all_vars, "all_vars")
margot::here_save(continuous_columns_keep, "continuous_columns_keep")
margot::here_save(ordinal_columns, "ordinal_columns")

# save names and labels
margot::here_save(baseline_wave, "baseline_wave")
margot::here_save(exposure_waves, "exposure_waves")
margot::here_save(outcome_wave, "outcome_wave")
margot::here_save(all_waves, "all_waves")
margot::here_save(baseline_and_exposure_waves, "baseline_and_exposure_waves")
margot::here_save(baseline_and_outcome_waves, "baseline_and_outcome_waves")

