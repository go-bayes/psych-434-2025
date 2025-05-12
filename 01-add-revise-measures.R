# 01 - add/revise measures


# boilerplate library ----------------------------------------
# install from GitHub if not already installed
if (!require(boilerplate, quietly = TRUE)) {
  # install devtools if necessary
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("go-bayes/boilerplate")
}

library(boilerplate)

if (packageVersion("boilerplate") < "1.0.41") {
  stop(
    "please install boilerplate >= 1.0.41 for this workflow\n
       run: devtools::install_github(\"go-bayes/boilerplate\")
"
  )
}


cli::cli_h1("installed/loaded boilerplate ✔")





# read boilerplate database ----------------------------------------------------

# set path ----------------------------------------------------------------
# or to whatever you names the database path
my_boilerplate_data_path <- here::here("example_boilerplate_data")

# check path is correct
my_boilerplate_data_path


# read boilerplate database -----------------------------------------------

unified_db <- boilerplate_import(data_path = my_boilerplate_data_path)


# create new data directory if it doesn't exist -----------------------------
if (!dir.exists("final_boilerplate_data")) {
  dir.create("final_boilerplate_data")  # first time only: make a folder named 'data'
}

cli::cli_h1("created data folder ✔")



# new data path -----------------------------------------------------------

path_final = here::here("final_boilerplate_data")



# avoid overwriting original data in case you make a mistake --------------

boilerplate_save(unified_db, output_file = "unified_db",
                   data_path = path_final)





# eyeball structure -------------------------------------------------------

str(unified_db, max.level = 2)

# eyeball-measures --------------------------------------------------------
my_unified_db$measures



# which measures do you need (to fix)? ------------------------------------

# load data -----------------------------------------------------------------
data_dir  <- here::here("data")

# check data
df_nz_long <- margot::here_read_qs("df_nz_long", data_dir)
df_nz_long$friends_money



# example add entry -------------------------------------------------------



# say you needed 'emp_job_satisfaction': is it defined? 
unified_db$measures$emp_job_satisfaction

cli::cli_h1("this variable is not defined ✔")



# measures ----------------------------------------------------------------
# add a measure like so: 

unified_db$measures$emp_job_satisfaction <- list(
  name = "Job Satisfaction",
  description = "Job satisfaction was measured with a single item.",
  reference = "[@eisenbarth2022aspects]",
  waves = "1-present",
  keywords = c("employment", "mental health"),
  items = list(
    "How satisfied are you with your current job?"
  )
)

# save your measure like so (you do not need to create a backup)
boilerplate::boilerplate_save(unified_db, data_path = path_final, create_backup = TRUE)





#  revise a measure like so -----------------------------------------------
unified_db$measures$family_time_binary

unified_db$measures$family_time_binary <- list(
  name = "Job Satisfaction",
  description = "Code string (Binary): (0 = 0, 1 = greater than 0)",  # <- change here
  waves = "10-13",
  reference = "@sibley2020", # <- change here
  keywords = c("cooperation"),
  items = list(
    "Please estimate how much help you have received from the following sources in the last week...family...TIME (hours)."
  )
)

# view
unified_db$measures$family_time_binary




# save changes ------------------------------------------------------------

boilerplate_save(
  db = unified_db,
  data_path = path_final,  # specify correct path
  confirm = TRUE
)




# if you want to read data back -------------------------------------------
# start here: 
unified_db <- boilerplate_import(data_path = path_final) # note we are using 'path final'





# generate bibliography ----------------------------------------------------
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


baseline_text  <- boilerplate_generate_measures(
  variable_heading = "Baseline Covariates",
  variables = baseline_vars,
  db = unified_db,  # or use the extracted measures database
  heading_level = 3,
  subheading_level = 4,
  print_waves = TRUE
)


# see here
cat(baseline_text)


# next week I'll show you how to generate a bibliograph using quarto documents: 

