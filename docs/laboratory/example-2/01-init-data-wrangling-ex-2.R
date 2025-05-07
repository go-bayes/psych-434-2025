# script 1 causal workflow for estimating average treatment effects using margot
# may 2025
# questions: joseph.bulbulia@vuw.ac.nz

# restart fresh session for a clean workspace
rstudioapi::restartSession()

# set seed for reproducibility
set.seed(123)

# load packages -------------------------------------------------------------
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
  doParallel      # parallel processing
)

# check margot version ------------------------------------------------------
if (packageVersion("margot") < "1.0.33") {
  stop("please install margot >= 1.0.33 for this workflow")
}

# create directories --------------------------------------------------------
# create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")  # first time only: make a folder named 'data'
}

if (!dir.exists("models_example_2")) {
  dir.create("models_example_2")  # first time only: make a folder named 'data'
}

data_dir    <- here::here("data")
push_mods <- here::here("models_example_2") # implicit directory `margot::here_save()` uses

# load data -----------------------------------------------------------------
df_nz_long <- margot::here_read_qs("df_nz_long", data_dir)

# initial data prep ---------------------------------------------------------
# prepare intial data

# define labels for rural classification
rural_labels <- c(
  "High Urban Accessibility", 
  "Medium Urban Accessibility",
  "Low Urban Accessibility", 
  "Remote", 
  "Very Remote"
)

dat_prep <- df_nz_long |>
  arrange(id, wave) |>
  margot::remove_numeric_attributes() |>
  mutate(
    # cap extreme values
    alcohol_intensity = pmin(alcohol_intensity, 15),
    # flag heavy drinkers: freq ≥3 → 1, ≤2 → 0, else NA
    heavy_drinker = case_when(
      alcohol_frequency >= 3 ~ 1,
      alcohol_frequency <= 2 ~ 0,
      TRUE                  ~ NA_real_
    ),
    # map freq categories to weekly counts
    alcohol_frequency_weekly = recode(
      alcohol_frequency,
      `0` = 0, `1` = 0.25,
      `2` = 1, `3` = 2.5,
      `4` = 4.5,
      .default = NA_real_
    ),
    # relabel rural factor
    rural_gch_2018_l = factor(
      rural_gch_2018_l,
      levels = 1:5,
      labels = rural_labels,
      ordered = TRUE
    )
  ) |>
  droplevels()


###############################################################################
# KEY DECISION 1
###############################################################################

# check out variables
colnames(df_nz_long)

# define study variables ----------------------------------------------------
# ** key decision 1: define your exposure variable **
name_exposure <- "extraversion"
exposure_var_binary = paste0(name_exposure, "_binary")
exposure_var  <- c(name_exposure, paste0(name_exposure, "_binary"))

# ** key decision 2: define your study waves **
baseline_wave      <- "2018"        # baseline measurement
exposure_waves     <- c("2019")     # when exposure is measured
outcome_wave       <- "2020"        # when outcomes are measured
all_waves          <- c(baseline_wave, exposure_waves, outcome_wave)

# save key variables --------------------------------------------------------
margot::here_save(name_exposure, "name_exposure",push_mods)
margot::here_save(exposure_var, "exposure_var", push_mods)
margot::here_save(exposure_var_binary, "exposure_var_binary", push_mods)
margot::here_save(all_waves,"all_waves", push_mods)



###############################################################################
# DECISION 2: BASELINE:  IT WILL MAKE SENSE TO KEEP BASELINE VARIABLES 
###############################################################################


# ** key decision 2: define baseline covariates **
# these are demographics, traits, etc. measured at baseline
# no need to change these defaults
baseline_vars <- c(
  # demographics
  "age", "born_nz_binary", "education_level_coarsen",
  "employed_binary", "eth_cat", "male_binary",
  "not_heterosexual_binary", "parent_binary", "partner_binary",
  "rural_gch_2018_l", "sample_frame_opt_in_binary",
  
  # personality traits (excluding exposure)
  "agreeableness", "conscientiousness", "neuroticism", "openness",
  
  # health and lifestyle
  "alcohol_frequency", "alcohol_intensity", "hlth_disability_binary",
  "log_hours_children", "log_hours_commute", "log_hours_exercise",
  "log_hours_housework", "log_household_inc",
  "short_form_health", "smoker_binary",
  
  # social and psychological
  "belong", "nz_dep2018", "nzsei_13_l",
  "political_conservative", "religion_identification_level"
)

# sort for easier reference
baseline_vars <- sort(baseline_vars)

baseline_vars_no_log_init <- c(
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
baseline_vars_no_log = setdiff(baseline_vars_no_log_init, c(
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_hours_community",
  "log_household_inc"
))

# sort
baseline_vars_no_log <- sort(baseline_vars_no_log)

# save
here_save(baseline_vars_no_log, "baseline_vars_no_log")

###############################################################################
# KEY DECISION 3: DEFINE OUTCOME VARIABLES 
###############################################################################

# ** key decision 3: define outcome variables **
outcome_vars <- c(
  # health outcomes
  "alcohol_frequency_weekly", "alcohol_intensity",
  "hlth_bmi", "log_hours_exercise", "hlth_sleep_hours", 
  "short_form_health",
  
  # psychological outcomes
  "hlth_fatigue", "kessler_latent_anxiety", 
  "kessler_latent_depression", "rumination",
  
  # wellbeing outcomes
  "bodysat", "forgiveness", "gratitude", "lifesat", 
  "meaning_purpose", "meaning_sense", "perfectionism", 
  "pwi", "self_control", "self_esteem", 
  "sexual_satisfaction",
  
  # social outcomes
  "belong", "neighbourhood_community", "support"
)

# sort for easier reference
outcome_vars <- sort(outcome_vars)

# outcome vars no log
outcome_vars_no_log_init <- c(outcome_vars,"hours_exercise")

outcome_vars_no_log = setdiff(outcome_vars_no_log_init, c(
  "log_hours_exercise"
))


# sort
outcome_vars_no_log <- sort(outcome_vars_no_log_init)

# save
here_save(baseline_vars_no_log, "baseline_vars_no_log")





# outcome variables by domain ---------------------------------------------
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

# define psych outcomes labels
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

# define present outcomes labels
raw_outcomes_present <- c(
  "bodysat",
  "forgiveness",
  "perfectionism", 
  "self_control" , 
  "self_esteem", 
  "sexual_satisfaction" )

# sort
raw_outcomes_present <- sort(raw_outcomes_present)

# save
here_save(raw_outcomes_present, "raw_outcomes_present")

# define life outcomes labels
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

# define social outcomes labels
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



###############################################################################
# KEY DECISION 4: DEFINE OUTCOME VARIABLES 
###############################################################################


# time-varying confounder if needed ---------------------------------------

# ** key decision 4: define time-varying confounders **
# these are variables that could affect the outcome but cannot be affected by exposure
confounder_vars <- c(
  "hlth_disability_binary"  # consider carefully if this could be affected by the exposure
)

# save variables for reproducibility ----------------------------------------
margot::here_save(baseline_vars,    "baseline_vars", push_mods)
margot::here_save(outcome_vars,     "outcome_vars", push_mods)
margot::here_save(confounder_vars,  "confounder_vars", push_mods)

# select eligible participants ----------------------------------------------
# only include participants who have exposure data at baseline
ids_baseline <- dat_prep |> 
  filter(wave == baseline_wave, !is.na(!!sym(name_exposure))) |> 
  pull(id)

# filter data to include only eligible participants and relevant waves
dat_long_1 <- dat_prep |> 
  filter(id %in% ids_baseline, wave %in% all_waves) |> 
  droplevels()

# ** key decision 6: determine binary cutpoint for exposure **
# visualise distribution to decide on appropriate cutpoint
dat_long_exposure <- dat_long_1 |> filter(wave %in% exposure_waves)

# plot distribution to help with cutpoint decision
graph_cut <- margot::margot_plot_categorical(
  dat_long_exposure,
  col_name         = name_exposure,
  sd_multipliers = c(-1, 1), # select to suit
  # either use n_divisions for equal-sized groups:
  # n_divisions      = 2,
  # or use custom_breaks for specific values:
  custom_breaks    = c(1, 4),  # ** adjust as needed **
  cutpoint_inclusive = "upper",
  show_mean        = TRUE,
  show_sd          = TRUE
)
print(graph_cut)
margot::here_save(graph_cut, "graph_cut", push_mods)

# create binary exposure variable based on chosen cutpoint
dat_long_2 <- margot::create_ordered_variable(
  dat_long_1,
  var_name           = name_exposure,
  custom_breaks      = c(1, 4),  # ** adjust based on your decision **
  cutpoint_inclusive = "upper"
)

# process binary variables and log-transform --------------------------------
# convert binary factors to 0/1 format
dat_long_3 <- margot::margot_process_binary_vars(dat_long_2)

# log-transform hours and income variables: tables for manuscript
dat_long_tables <- margot::margot_log_transform_vars(
  dat_long_3,
  vars            = c(starts_with("hours_"), "household_inc"),
  prefix          = "log_",
  keep_original   = TRUE  # keep both original and transformed variables
) |> 
  # select only variables needed for analysis
  select(all_of(c(baseline_vars_no_log, exposure_var, outcome_vars_no_log, "id", "wave"))) |> 
  droplevels()

# log-transform hours and income variables: tables for analysis (only logged versions of vars)
dat_long_final <- margot::margot_log_transform_vars(
  dat_long_3,
  vars            = c(starts_with("hours_"), "household_inc"),
  prefix          = "log_",
  keep_original   = FALSE  # omit original variables
) |> 
  # select only variables needed for analysis
  select(all_of(c(baseline_vars, exposure_var, outcome_vars, "id", "wave"))) |> 
  droplevels()


# check missing data --------------------------------------------------------
# this is crucial to understand potential biases
missing_summary <- naniar::miss_var_summary(dat_long_final)
print(missing_summary)
margot::here_save(missing_summary, "missing_summary", push_mods)

# visualise missing data pattern
# ** -- takes a while to render ** 
vis_miss <- naniar::vis_miss(dat_long_final, warn_large_data = FALSE)
print(vis_miss)
margot::here_save(vis_miss, "vis_miss", push_mods)

# calculate percentage of missing data at baseline
dat_baseline_pct <- dat_long_final |> filter(wave == baseline_wave)
percent_missing_baseline <- naniar::pct_miss(dat_baseline_pct)
margot::here_save(percent_missing_baseline, "percent_missing_baseline", push_mods)

# save prepared dataset for next stage --------------------------------------
margot::here_save(dat_long_final, "dat_long_prepare", push_mods)

# visualise individual changes in exposure over time ------------------------
# useful for understanding exposure dynamics
individual_plot <- margot_plot_individual_responses(
  dat_long_1,
  y_vars = name_exposure,
  id_col = "id",
  waves = c(2018:2019),
  random_draws = 56,  # number of randomly selected individuals to show
  theme = theme_classic(),
  scale_range = c(1, 7),  # range of the exposure variable
  full_response_scale = TRUE,
  seed = 123
)
print(individual_plot)
margot::here_save(individual_plot, "individual_plot_exposure", push_mods)

# create transition matrices to check positivity ----------------------------
# this helps assess whether there are sufficient observations in all exposure states
dt_positivity <- dat_long_final |>
  filter(wave %in% c(baseline_wave, exposure_waves)) |>
  select(!!sym(name_exposure), id, wave) |>
  mutate(exposure = round(as.numeric(!!sym(name_exposure)), 0)) |>
  # create binary exposure based on cutpoint
  mutate(exposure_binary = ifelse(exposure >= 4, 1, 0)) |>
  mutate(wave = as.numeric(wave) -1 )

# create transition tables
transition_tables <- margot::margot_transition_table(
  dt_positivity,
  state_var = "exposure",
  id_var = "id",
  waves = c(0, 1),
  wave_var = "wave",
  table_name = "transition_table"
)
print(transition_tables$tables[[1]])
margot::here_save(transition_tables, "transition_tables", push_mods)

# create binary transition tables
transition_tables_binary <- margot::margot_transition_table(
  dt_positivity,
  state_var = "exposure_binary",
  id_var = "id",
  waves = c(0, 1),
  wave_var = "wave",
  table_name = "transition_table_binary"
)
print(transition_tables_binary$tables[[1]])
margot::here_save(transition_tables_binary, "transition_tables_binary", push_mods)



###############################################################################
# KEY DECISION 5: DEFINE OUTCOME VARIABLES 
###############################################################################

# create descriptive tables -------------------------------------------------
# define variable labels for tables and plots -------------------------------
# ** key decision 5: define clear labels for all variables **
# create labels by category for better organisation

# exposure variable labels
var_labels_exposure <- list(
  "extraversion" = "Extraversion",
  "extraversion_binary" = "Extraversion (binary)"
)

# baseline variable labels
var_labels_baseline <- list(
  # demographics
  "age" = "Age",
  "born_nz_binary" = "Born in NZ",
  "education_level_coarsen" = "Education Level",
  "employed_binary" = "Employed",
  "eth_cat" = "Ethnicity",
  "male_binary" = "Male",
  "not_heterosexual_binary" = "Non-heterosexual",
  "parent_binary" = "Parent",
  "partner_binary" = "Has Partner",
  "rural_gch_2018_l" = "Rural Classification",
  "sample_frame_opt_in_binary" = "Sample Frame Opt-In",
  
  # economic & social status
  "household_inc" = "Household Income",
  "log_household_inc" = "Log Household Income",
  "nz_dep2018" = "NZ Deprivation Index",
  "nzsei_13_l" = "Occupational Prestige Index",
  "household_inc" = "Household Income",

  
  # personality traits
  "agreeableness" = "Agreeableness",
  "conscientiousness" = "Conscientiousness",
  "neuroticism" = "Neuroticism",
  "openness" = "Openness",
  
  # beliefs & attitudes
  "political_conservative" = "Political Conservatism",
  "religion_identification_level" = "Religious Identification",
  
  # health behaviors
  "alcohol_frequency" = "Alcohol Frequency",
  "alcohol_intensity" = "Alcohol Intensity",
  "hlth_disability_binary" = "Disability Status",
  "smoker_binary" = "Smoker",
  "hours_exercise" = "Hours of Exercise",
  
  
  # time use
  "hours_children" = "Hours with Children",
  "hours_commute" = "Hours Commuting",
  "hours_exercise" = "Hours Exercising",
  "hours_housework" = "Hours on Housework",
  "log_hours_children" = "Log Hours with Children",
  "log_hours_commute" = "Log Hours Commuting",
  "log_hours_exercise" = "Log Hours Exercising",
  "log_hours_housework" = "Log Hours on Housework"
)

# outcome variable labels, organized by domain

var_labels_health <- list(
  "alcohol_frequency_weekly" = "Alcohol Frequency (weekly)",
  "alcohol_intensity" = "Alcohol Intensity",
  "hlth_bmi" = "Body Mass Index",
  "hlth_sleep_hours" = "Sleep",
  "hours_exercise" = "Hours of Exercise",
  "short_form_health" = "Short Form Health"
)

# define psych outcomes
var_labels_psych <- list(
  "hlth_fatigue" = "Fatigue",
  "kessler_latent_anxiety" = "Anxiety",
  "kessler_latent_depression" = "Depression",
  "rumination" = "Rumination"
)

# define present outcomes
var_labels_present <- list(
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
  "meaning_purpose" = "Meaning: Purpose",
  # exposure variable
  "meaning_sense" = "Meaning: Sense",
  "pwi = Personal Well-being Index"
)

# define social outcome names
var_labels_social <- list(
  "belong" = "Social Belonging",
  "neighbourhood_community" = "Neighbourhood Community",
  "support" = "Social Support"
)
# combine all label lists
var_labels_all = c(
  var_labels_baseline,
  var_labels_exposure,
  var_labels_health,
  var_labels_psych,
  var_labels_present,
  var_labels_life,
  var_labels_social
)

# save for manuscript
here_save(var_labels_all, "var_labels_all", push_mods)


# tables ------------------------------------------------------------------
# create baseline characteristics table
dat_baseline = dat_long_tables |>
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

baseline_table <- margot::margot_make_tables(
  data = dat_baseline,
  vars = baseline_vars,
  by = "wave",
  labels = var_labels_all,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)
print(baseline_table)
margot::here_save(baseline_table, "baseline_table", push_mods)

# create exposure table by wave
exposure_table <- margot::margot_make_tables(
  data = dat_long_tables |> filter(wave %in% c(baseline_wave, exposure_waves)),
  vars = exposure_var,
  by = "wave",
  labels = var_labels_all,
  factor_vars = exposure_var_binary,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)
print(exposure_table)
margot::here_save(exposure_table, "exposure_table", push_mods)

# create outcomes table by wave
outcomes_table <- margot::margot_make_tables(
  data = dat_long_tables |> filter(wave %in% c(baseline_wave, outcome_wave)),
  vars = outcome_vars_no_log,
  by = "wave",
  labels = var_labels,
  format = "markdown"
)
print(outcomes_table)
margot::here_save(outcomes_table, "outcomes_table", push_mods)

# note: completed data preparation step -------------------------------------
# you're now ready for the next steps:
# 1. creating wide-format dataset for analysis 
# 2. applying causal inference methods
# 3. conducting sensitivity analyses

# key decisions summary:
# exposure variable: extraversion
# study waves: baseline (2018), exposure (2019), outcome (2020)
# baseline covariates: demographics, traits, health measures (excluding exposure)
# outcomes: health, psychological, wellbeing, and social variables
# time-varying confounders: disability status??
# binary cutpoint for exposure: 4 on the extraversion scale
# label names for tables
