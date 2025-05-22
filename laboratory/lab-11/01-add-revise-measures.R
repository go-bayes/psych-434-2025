# script 01: add or revise measures (for students)
# load required packages --------------------------------------------------
if (!requireNamespace("boilerplate", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")  # install devtools if missing
  }
  devtools::install_github("go-bayes/boilerplate")  # install boilerplate
}

library(boilerplate)  # tools for measure management
library(cli)          # user-friendly messages
library(here)         # project-friendly file paths

# ensure correct boilerplate version --------------------------------------
if (utils::packageVersion("boilerplate") < "1.0.41") {
  stop("please install boilerplate >= 1.0.41: \
       devtools::install_github('go-bayes/boilerplate')")
}

cli::cli_h1("boilerplate loaded ✔")

# define data paths --------------------------------------------------------
path_src   <- here::here("example_boilerplate_data")
path_final <- here::here("final_boilerplate_data")

# create final data directory if needed -----------------------------------
if (!dir.exists(path_final)) {
  dir.create(path_final)
}
cli::cli_h1("data folder ready ✔")

# import unified database --------------------------------------------------
unified_db <- boilerplate_import(data_path = path_src)

# save a copy to avoid overwriting original --------------------------------
boilerplate_save(
  unified_db,
  data_path   = path_final,
  output_file = "unified_db"
)

ycli::cli_h2("database imported and saved ✔")

# inspect structure and existing measures ----------------------------------
str(unified_db, max.level = 2)     # glance at top-level structure
print(names(unified_db$measures))  # list defined measures

# example: check for a specific measure ------------------------------------
measure_name <- "emp_job_satisfaction"
if (is.null(unified_db$measures[[measure_name]])) {
  cli::cli_alert_info("{measure_name} not defined yet")
} else {
  print(unified_db$measures[[measure_name]])
}

# add a new measure --------------------------------------------------------
unified_db$measures[[measure_name]] <- list(
  name        = "Job Satisfaction",
  description = "job satisfaction was measured with a single item.",
  reference   = "[@eisenbarth2022aspects]",
  waves       = "1-present",
  keywords    = c("employment", "mental health"),
  items       = list("how satisfied are you with your current job?")
)

unified_db$measures[[measure_name]] <- list(
  name        = "Cyberbulling",
  description = "Cyberbulling was measured with a single item.",
  reference   = "wang2019cyberbullying",
  waves       = "6-11",
  keywords    = c("mental health"),
  items       = list("Has someone ever used the internet, a mobile phone, or digital camera to hurt, intimidate or embarrass you?")
)


# saving a new measure
unified_db$measures[["pwi"]] <- list(
  name        = "Personal Well-Being Index",
  description = "The Personal Well-Being Index consists of three items, asking 'How satisfied are you with...' ",
  reference   = "cummins2003development",
  waves       = "1-present",
  keywords    = c("employment", "mental health"),
  items       = list("'Your standard of living.'", "'Your health.'", "'Your future security.'", "'Your personal relationships.'")
)


# save with backup ---------------------------------------------------------
boilerplate_save(
  unified_db,
  data_path     = path_final,
  create_backup = TRUE
)
cli::cli_h2("new measure added and saved ✔")

# revise an existing measure ------------------------------------------------
revise_name <- "family_time_binary"
cli::cli_h1("revising {revise_name}")

# use modifyList to update only changed fields
unified_db$measures[[revise_name]] <- modifyList(
  unified_db$measures[[revise_name]],
  list(
    name        = "Family Time (binary)",
    description = "code string (binary): 0 = none, 1 = any time",
    reference   = "@sibley2020",
    waves       = "10-13",
    keywords    = c("cooperation")
  )
)

# view revised measure ------------------------------------------------------
print(unified_db$measures[[revise_name]])

# save all changes -----------------------------------------------------------
boilerplate_save(
  unified_db,
  data_path = path_final,
  confirm   = TRUE
)
cli::cli_h2("measure revised and saved ✔")

# to reload updated database, uncomment the following line --------------
# unified_db <- boilerplate_import(data_path = path_final)


