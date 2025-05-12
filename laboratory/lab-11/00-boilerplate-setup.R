# student workflow example
# 00-read-initial-boileplate-data



# get required boilerplate library ----------------------------------------

# initialise measures
# install from GitHub if not already installed
if (!require(boilerplate, quietly = TRUE)) {
  # install devtools if necessary
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("go-bayes/boilerplate")
}

library(boilerplate)

if (packageVersion("boilerplate") < "1.0.42") {
  stop("please install boilerplate >= 1.0.42 for this workflow\n
       run: devtools::install_github(\"go-bayes/boilerplate\")
")
}


cli::cli_h1("installed/loaded boilerplate ✔")


# required libraries ------------------------------------------------------
library("here")

cli::cli_h1("loaded required libraries ✔")



# create data directory if it doesn't exist -----------------------------
if (!dir.exists("example_boilerplate_data")) {
  dir.create("example_boilerplate_data")  # first time only: make a folder named 'data'
}

cli::cli_h1("created data folder ✔")


# set path ----------------------------------------------------------------
my_boilerplate_data_path <- here::here("example_boilerplate_data")

# check path is correct
my_boilerplate_data_path




# read data ---------------------------------------------------------------
# functions that imports by categories 
load_student_boilerplate <- function() {
  base_url <- "https://raw.githubusercontent.com/go-bayes/templates/main/student_boilerplate_data/"
  categories <- c("measures", "methods", "results", "discussion", "appendix")
  
  cat("loading student boilerplate data from GitHub...\n")
  
  # load each category and combine
  student_db <- list()
  for (cat in categories) {
    cat("  - loading", cat, "database...")
    
    tryCatch({
      student_db[[cat]] <- readRDS(url(paste0(base_url, cat, "_db.rds")))
      cat(" success\n")
    }, error = function(e) {
      cat(" failed\n")
      warning("failed to load ", cat, ": ", e$message, call. = FALSE)
      student_db[[cat]] <- list()  # empty list as fallback
    })
  }
  
  cat("successfully loaded", length(categories), "categories\n")
  return(student_db)
}
# import
student_unified_db <- load_student_boilerplate()

# save the data 
boilerplate_save(student_unified_db, data_path = my_boilerplate_data_path, create_backup = FALSE)


cli::cli_h1("data are saved, do not use this script again ✔")





# set up bibliography and APA 7 class -------------------------------------

# load needed package
if (!requireNamespace("fs", quietly = TRUE)) {
  install.packages("fs")
}

# create template-partials directory (no error if it already exists)
fs::dir_create("template_partials")

# download the title.tex file into template-partials/
download.file(
  url      = "https://raw.githubusercontent.com/go-bayes/templates/refs/heads/main/quarto/title.tex",
  destfile = "template-partials/title.tex",
  mode     = "wb"
)

# create directories (no error if they already exist)
fs::dir_create("bibliography")
fs::dir_create("csl")

# download the remote .bib into bibliography/
download.file(
  url      = "https://raw.githubusercontent.com/go-bayes/templates/refs/heads/main/bib/references.bib",
  destfile = "bibliography/references.bib",
  mode     = "wb"
)

# download the APA-7 CSL file into the new csl directory
download.file(
  url      = "https://raw.githubusercontent.com/go-bayes/templates/refs/heads/main/csl/apa-7.csl",
  destfile = "csl/apa7.csl",
  mode     = "wb"
)
