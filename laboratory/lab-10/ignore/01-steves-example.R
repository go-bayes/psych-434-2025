# script 1 workflow lecture 10
# may 2025
# questions: joseph.bulbulia@vuw.ac.nz


# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+

# restart fresh session for a clean workspace
rstudioapi::restartSession()

# set seed for reproducibility
set.seed(123)

# essential library ---------------------------------------------------------
# install and load 'margot' from GitHub if missing
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}


if (packageVersion("margot") < "1.0.43") {
  stop("please install margot >= 1.0.43 for this workflow\n
       run: devtools::install_github(\"go-bayes/margot\")
")
}

# call library
library("margot")

# load packages -------------------------------------------------------------
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
  grf,             # machine learning forests
  kableExtra,      # tables
  ggplot2,         # graphs
  doParallel,       # parallel processing
  grf,             # causal forests
  janitor,          # variables names
  stringr,          # variable names
  patchwork,        # graphs
  table1,          # tables,
  cli
)


# create directories --------------------------------------------------------
# create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")  # first time only: make a folder named 'data'
}

if (!dir.exists("save_directory")) {
  dir.create("save_directory")  # first time only: make a folder named 'data'
}

# set up data directory structure
data_dir    <- here::here("data")
push_mods <- here::here("examples") 


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



# view variable names -----------------------------------------------------
print(colnames(df_nz_long)) 


# get total participants
n_total = length(unique(df_nz_long$id))

# pretty number
n_total = margot::pretty_number(n_total)

# save
here_save(n_total, "n_total")

# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+



# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# +--------------------------+
# | OPTIONALLY MODIFY SECTION|
# +--------------------------+

# define study variables ----------------------------------------------------
# ** key decision 1: define your three study waves **
# **  define your study waves **
baseline_wave      <- "2018"        # baseline measurement
exposure_waves     <- c("2019")     # when exposure is measured
outcome_wave       <- "2020"        # when outcomes are measured
all_waves          <- c(baseline_wave, exposure_waves, outcome_wave)

cli::cli_h1("set waves for three-wave study ✔")


# +--------------------------+
# |END OPTIONALLY MODIFY SEC.|
# +--------------------------+
# +--------------------------+
# |        END ALERT         |
# +--------------------------+


# define exposure variable ----------------------------------------------------
# ** key decision 2: define your exposure variable **

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+

# find variable names
colnames(df_nz_long)

# set exposure
name_exposure <- "political_conservative"

# exposure variable labels
var_labels_exposure <- list(
  "political_conservative" = "Political Conservativism",
  "political_conservative_binary" = "political_conservativism_binary (binary)"
)

cli::cli_h1("set variable name for exposure ✔")

# +--------------------------+
# |        END ALERT         |
# +--------------------------+
# +--------------------------+
# |   END MODIFY SECTION     |
# +--------------------------+






# define outcome variables -------------------------------------------
# ** key decision 3: define your outcome variable **
# +--------------------------+
# |          ALERT           |
# +--------------------------+
# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+
# ** key decision 3: define outcome variables **
# here, we are focussing on a subset of wellbeing outcomes
# chose outcomes relevant to * your * study. Might be all/some/none/exactly 
# these:

# chose outcomes relevant to * your * study. might be all/some/none/exactly these:
outcome_vars <- c(
   "lifesat", 
   "meaning_purpose",
   "meaning_sense",  
   "pwi"
)

cli::cli_h1("set variable name for outcomes ✔")

# +--------------------------+
# |   END MODIFY SECTION     |
# +--------------------------+
# +--------------------------+
# |        END ALERT         |
# +--------------------------+


# +--------------------------+
# |          ALERT           |
# +--------------------------+
# +--------------------------+
# | OPTIONALLY MODIFY SECTION|
# +--------------------------+
# define baseline variables -----------------------------------------------
# key decision 4 **  define baseline covariates **
# these are demographics, traits, etc. measured at baseline, that are common
# causes of the exposure and outcome.  
# note we will automatically include baseline measures of the exposure and outcome
# later in the workflow.

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
  "political_conservative", 
  "religion_identification_level",
  "sdo",
  "rwa"
)


cli::cli_h1("set baseline covariate names  ✔")

# +--------------------------+
# |        END ALERT         |
# +--------------------------+
# +--------------------------+
# |   END MODIFY SECTION     |
# +--------------------------+


# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+

# after selecting your exposure/ baseline / outcome variables do not modify this
# code

# make binary variable (UNLESS YOUR EXPOSURE IS A BINARY VARIABLE)
exposure_var_binary = paste0(name_exposure, "_binary")

# make exposure variable list (we will keep both the continuous and binary variable)
exposure_var  <- c(name_exposure, paste0(name_exposure, "_binary"))

# sort for easier reference
baseline_vars <- sort(baseline_vars)
outcome_vars <- sort(outcome_vars)

# save key variables --------------------------------------------------------
margot::here_save(name_exposure, "name_exposure")
margot::here_save(var_labels_exposure,"var_labels_exposure")
margot::here_save(baseline_vars,"baseline_vars")
margot::here_save(exposure_var, "exposure_var")
margot::here_save(exposure_var_binary, "exposure_var_binary")
margot::here_save(outcome_vars, "outcome_vars")
margot::here_save(baseline_wave, "baseline_wave")
margot::here_save(exposure_waves, "exposure_waves")
margot::here_save(outcome_wave, "outcome_wave")
margot::here_save(all_waves,"all_waves")

cli::cli_h1("saved names and labels to be used for manuscript  ✔")


# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+



# +--------------------------+
# |          ALERT           |
# +--------------------------+
# +--------------------------+
# | OPTIONALLY MODIFY SECTION|
# +--------------------------+

# select eligible participants ----------------------------------------------
# only include participants who have exposure data at baseline

# You might require tighter conditions 
# for example, if you are interested in the effects of hours of childcare, 
# you might want to select only those who were parents at baseline. 
# talk to me if you think you might night tighter eligibility criteria.

ids_baseline <- dat_prep |> 
  # allow missing exposure at baseline
  # this would give us greater confidence that we generalise to the target population
  # filter(wave == baseline_wave) |> 
  # option: do not allow missing exposure at baseline
  # this gives us greater confidence that we recover a incident effect
  filter(wave == baseline_wave, !is.na(!!sym(name_exposure))) |> 
  pull(id)

# n eligible
n_participants <- length(ids_baseline)

# make pretty number
n_participants = margot::pretty_number(n_participants)

# save
here_save(n_participants, "n_participants")

cli::cli_h1("set eligibility criteria for baseline cohort ✔")


# +--------------------------+
# |          ALERT           |
# +--------------------------+

# EXAMPLE count different eligibility conditions ----------------------------------------------


# define eligibility criteria
eligible_ids <- df_nz_long |> 
  filter(wave == 2018 & year_measured == 1 & age < 30 & eth_cat == "pacific") |> 
  distinct(id) |> 
  pull(id)

# count eligible ids
length(eligible_ids)

# filter data to include only eligible participants
dat_long_different_eligibility <- dat_prep |> 
  filter(id %in% eligible_ids, wave %in% all_waves) |> 
  droplevels()

# +--------------------------+
# |        END ALERT         |
# +--------------------------+

# filter using general conditions -----------------------------------------


# filter data to include only eligible participants and relevant waves
dat_long_1 <- dat_prep |> 
  filter(id %in% ids_baseline, wave %in% all_waves) |> 
  droplevels()

# +--------------------------+
# |END OPTIONALLY MODIFY SEC.|
# +--------------------------+
# +--------------------------+
# |        END ALERT         |
# +--------------------------+

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+
# plot distribution to help with cutpoint decision
dat_long_exposure <- dat_long_1 |> filter(wave %in% exposure_waves)

# define cutpoints
cut_points = c(1, 5)

# make graph
graph_cut <- margot::margot_plot_categorical(
  dat_long_exposure,
  col_name         = name_exposure,
  sd_multipliers = c(-1, 1), # select to suit
  # either use n_divisions for equal-sized groups:
  #n_divisions      = 2,
 # or use custom_breaks for specific values:
  custom_breaks    = cut_points,  # ** adjust as needed **
  # could be "lower", no difference in this case, as no one == 4
  cutpoint_inclusive = "upper",
  show_mean        = TRUE,
  show_median      = FALSE,
  show_sd          = TRUE
)
print(graph_cut)

# make graph 


# use later in positivity graph
lower_cut <- cut_points[[1]]
upper_cut <- cut_points[[2]]
threshold <- '>' # if upper
inverse_threshold <- '<='
scale_range = 'scale range 1-7'


# save for manuscript
here_save(lower_cut, "lower_cut")
here_save(upper_cut, "upper_cut")
here_save(threshold, "threshold")
here_save(inverse_threshold, "inverse_threshold")
here_save(scale_range, "scale_range")


cli::cli_h1("set thresholds for binary variable (if variable is continuous) ✔")


# save your graph
margot::here_save(graph_cut, "graph_cut", push_mods)

# create binary exposure variable based on chosen cutpoint
dat_long_2 <- margot::create_ordered_variable(
  dat_long_1,
  var_name           = name_exposure,
  custom_breaks      = cut_points,  # ** -- adjust based on your decision above -- **
  cutpoint_inclusive = "upper"
)


cli::cli_h1("created binary variable (if variable is continuous) ✔")


# +--------------------------+
# |   END MODIFY SECTION     |
# +--------------------------+
# +--------------------------+
# |        END ALERT         |
# +--------------------------+





# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+

# process binary variables and log-transform --------------------------------
# convert binary factors to 0/1 format
dat_long_3 <- margot::margot_process_binary_vars(dat_long_2)

# log-transform hours and income variables: tables for analysis (only logged versions of vars)
dat_long_final <- margot::margot_log_transform_vars(
  dat_long_3,
  vars            = c(starts_with("hours_"), "household_inc"),
  prefix          = "log_",
  keep_original   = FALSE,
  exceptions = exposure_var # omit original variables
) |> 
  # select only variables needed for analysis
  select(all_of(c(baseline_vars, exposure_var, outcome_vars, "id", "wave", "year_measured", "sample_weights"))) |> 
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
margot::here_save(dat_long_final, "dat_long_final", push_mods)

cli::cli_h1("made and saved final long data set for further processign in script 02 ✔")


# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+


# check positivity --------------------------------------------------------

# +--------------------------+
# |          ALERT           |
# +--------------------------+
# +--------------------------+
# |    MODIFY THIS SECTION   |
# +--------------------------+


# check
threshold # defined above
upper_cut # defined above
name_exposure
# create transition matrices to check positivity ----------------------------
# this helps assess whether there are sufficient observations in all exposure states
dt_positivity <- dat_long_final |>
  filter(wave %in% c(baseline_wave, exposure_waves)) |>
  select(!!sym(name_exposure), id, wave) |>
  mutate(exposure = round(as.numeric(!!sym(name_exposure)), 0)) |>
  # create binary exposure based on cutpoint
  mutate(exposure_binary = ifelse(exposure > upper_cut, 1, 0)) |> # check
  ## *-- modify this --* 
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

# +--------------------------+
# |        END ALERT         |
# +--------------------------+

# create tables -----------------------------------------------------------
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
here_save(var_labels_baseline, "var_labels_baseline")

# outcome variable labels, organized by domain
# reivew your outcomes make sure they appear on the list below
# comment out what you do not need
outcome_vars

# get names
var_labels_outcomes <- list(
  # # "alcohol_frequency_weekly" = "Alcohol Frequency (weekly)",
  # # "alcohol_intensity" = "Alcohol Intensity",
  # # "hlth_bmi" = "Body Mass Index",
  # # "hlth_sleep_hours" = "Sleep",
  # #"log_hours_exercise" = "Hours of Exercise (log)",
  # # "short_form_health" = "Short Form Health",
  # #"hlth_fatigue" = "Fatigue",
  # "kessler_latent_anxiety" = "Anxiety", "kessler_latent_depression" = "Depression", # "rumination" = "Rumination",
  # "bodysat" = "Body Satisfaction", # "forgiveness" = "Forgiveness",
  # # "perfectionism" = "Perfectionism",
  # # "self_control" = "Self Control",
  # "self_esteem" = "Self Esteem", "sexual_satisfaction" = "Sexual Satisfaction", # "gratitude" = "Gratitude",
  # "lifesat" = "Life Satisfaction", "meaning_purpose" = "Meaning: Purpose", "meaning_sense" = "Meaning: Sense", "pwi" = "Personal Well-being Index", 
  "belong" = "Social Belonging",
  "neighbourhood_community" = "Neighbourhood Community",
  "support" = "Social Support",
  "log_hours_charity" = "Hours Volunteering (Binary)",
   "charity_donate" = "Charitable Donations"
)

# save for manuscript
here_save(var_labels_outcomes, "var_labels_outcomes")


# save all variable translations
var_labels_measures <- c(var_labels_baseline, var_labels_exposure, var_labels_outcomes)
var_labels_measures

# save for manuscript
here_save(var_labels_measures, "var_labels_measures")

# +--------------------------+
# |   END MODIFY SECTION     |
# +--------------------------+

# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+
# tables ------------------------------------------------------------------
# create baseline characteristics table
dat_baseline = dat_long_final |>
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


# +--------------------------+
# |          ALERT           |
# +--------------------------+

# save sample weights from baseline wave
# save sample weights
t0_sample_weights <- dat_baseline$sample_weights
here_save(t0_sample_weights, "t0_sample_weights")

# +--------------------------+
# |        END ALERT         |
# +--------------------------+


# make baseline table -----------------------------------------------------

baseline_table <- margot::margot_make_tables(
  data = dat_baseline,
  vars = baseline_vars,
  by = "wave",
  labels = var_labels_baseline,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)
print(baseline_table)
margot::here_save(baseline_table, "baseline_table", push_mods)

# create exposure table by wave
exposure_table <- margot::margot_make_tables(
  data = dat_long_final |> filter(wave %in% c(baseline_wave, exposure_waves)),
  vars = exposure_var,
  by = "wave",
  labels = var_labels_exposure,
  factor_vars = exposure_var_binary,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)
print(exposure_table)
margot::here_save(exposure_table, "exposure_table", push_mods)

# create outcomes table by wave
outcomes_table <- margot::margot_make_tables(
  data = dat_long_final |> filter(wave %in% c(baseline_wave, outcome_wave)),
  vars = outcome_vars,
  by = "wave",
  labels = var_labels_outcomes,
  format = "markdown"
)
print(outcomes_table)
margot::here_save(outcomes_table, "outcomes_table", push_mods)

# +--------------------------+
# |     END DO NOT ALTER     |
# +--------------------------+


# +--------------------------+
# |     END                  |
# +--------------------------+


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
# binary cutpoint for exposure: here, 4 on the extraversion scale
# label names for tables




# THIS IS FOR INTEREST ONLY ----------------------------------------------------
# uncomment to view random chang in individuals
# visualise individual changes in exposure over time ------------------------
# useful for understanding exposure dynamics
# individual_plot <- margot_plot_individual_responses(
#   dat_long_1,
#   y_vars = name_exposure,
#   id_col = "id",
#   waves = c(2018:2019),
#   random_draws = 56,  # number of randomly selected individuals to show
#   theme = theme_classic(),
#   scale_range = c(1, 7),  # range of the exposure variable
#   full_response_scale = TRUE,
#   seed = 123
# )
# print(individual_plot)

