# script 00: read initial boilerplate data (for students)

# load required packages --------------------------------------------------
if (!requireNamespace("boilerplate", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")  # install devtools if missing
  }
  devtools::install_github("go-bayes/boilerplate")  # install boilerplate
}

library(boilerplate)  # manage boilerplate data
library(cli)          # friendly messages
library(here)         # project paths
library(fs)           # file system utilities

# ensure correct boilerplate version --------------------------------------
min_version <- "1.0.43"
if (utils::packageVersion("boilerplate") < min_version) {
  stop(
    "please install boilerplate >= ", min_version, ":\n",
    "  devtools::install_github('go-bayes/boilerplate')"
  )
}

cli::cli_h1("boilerplate loaded ✔")

# create local data folders ------------------------------------------------
path_data   <- here::here("example_boilerplate_data")
path_quarto <- here::here("quarto")

fs::dir_create(path_data)    # create data folder if needed
cli::cli_h2("data folder ready ✔")

# import student boilerplate data ------------------------------------------
# close connections 
closeAllConnections()

# function
load_student_boilerplate <- function() {
  base_url   <- "https://raw.githubusercontent.com/go-bayes/templates/main/student_boilerplate_data/"
  categories <- c("measures", "methods", "results", "discussion", "appendix", "template")
  
  cli::cli_text("loading student boilerplate data from GitHub…")
  
  # initialise empty list and assign category names
  student_db <- vector("list", length(categories))
  names(student_db) <- categories
  
  for (cat in categories) {
    cli::cli_text("  – loading {.strong {cat}} database…")
    rds_url  <- paste0(base_url, cat, "_db.rds")
    tmp_file <- tempfile(fileext = ".rds")
    
    student_db[[cat]] <- tryCatch({
      # download to a temporary file, then read it
      utils::download.file(rds_url, tmp_file,
                           mode = "wb",
                           quiet = TRUE,
                           method = "libcurl")
      readRDS(tmp_file)
    }, error = function(e) {
      cli::cli_alert_warning("failed to load {.strong {cat}}: {e$message}")
      list()  # fallback empty list
    }, finally = {
      # always remove the temp file
      unlink(tmp_file)
    })
  }
  
  cli::cli_text("successfully loaded {length(student_db)} categories")
  student_db
}




# after clearing connections, run:
student_unified_db <- load_student_boilerplate()

# save imported data -------------------------------------------------------
boilerplate_save(
  student_unified_db,
  data_path     = path_data,
  create_backup = FALSE
)
cli::cli_h1("data saved ✔")

# set up bibliography and APA-7 template -----------------------------------
fs::dir_create("quarto")  # for title.tex

download.file(
  url      = "https://raw.githubusercontent.com/go-bayes/templates/refs/heads/main/quarto/title.tex",
  destfile = "quarto/title.tex",
  mode     = "wb"
)

fs::dir_create("bibliography")
fs::dir_create("csl")

download.file(
  url      = "https://raw.githubusercontent.com/go-bayes/templates/refs/heads/main/bib/references.bib",
  destfile = "quarto/references.bib",
  mode     = "wb"
)

download.file(
  url      = "https://raw.githubusercontent.com/go-bayes/templates/refs/heads/main/csl/apa-7.csl",
  destfile = "quarto/apa7.csl",
  mode     = "wb"
)

cli::cli_h1("bibliography and CSL setup complete ✔")

# end of script: do not rerun this file ------------------------------------

cat(student_unified_db$methods$analytic_approach$general_approach_cate_long)

