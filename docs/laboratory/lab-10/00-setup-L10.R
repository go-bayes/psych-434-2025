# for students: reproducibility is like following a recipe; each step ensures the same result
# restart fresh session if needed

# +--------------------------+
# |       DO NOT ALTER       |
# +--------------------------+
# fresh session
rstudioapi::restartSession()

# set seed for reproducibility
set.seed(123)

# essential library ---------------------------------------------------------
# install and load 'margot' from GitHub if missing
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}


if (packageVersion("margot") < "1.0.224") {
  stop("please install margot >= 1.0.224 for this workflow\n
       run: devtools::install_github(\"go-bayes/margot\")
")
}

# call library
library("margot")


# load packages ----------------------------------------------------------
# install and load other packages from CRAN if missing
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("qs", quietly = TRUE)) {
  install.packages("qs")
}
library(qs)

if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

if (!requireNamespace("cli", quietly = TRUE)) {
  install.packages("cli")
}
library("cli")


# create data directory if it doesn't exist -----------------------------
if (!dir.exists("data")) {
  dir.create("data")  # first time only: make a folder named 'data'
}

# define file paths ------------------------------------------------------
# use here() to build paths relative to your project root
data_dir <- here::here("data")

cli::cli_h1("created data folder ✔")


# download synthetic data ------------------------------------------------
# specify the url for the data file
url <- "https://www.dropbox.com/scl/fi/ru0ecayju04ja8ky1mhel/df_nz_long.qs?rlkey=prpk9a5v4vcg1ilhkgf357dhd&dl=1"

# download to a temporary file for safety
tmp_file <- tempfile(fileext = ".qs")
download.file(url, tmp_file, mode = "wb")

# read the data into R using qread
df_nz_long <- qread(tmp_file)

# inspect the data -------------------------------------------------------
# view the first few rows to check it loaded correctly
print(head(df_nz_long))

# list column names so you know what variables are available
print(colnames(df_nz_long))

# save a copy of the data ------------------------------------------------
# save the dataset to your data directory for future use
here_save_qs(df_nz_long, "df_nz_long", data_dir)

cli::cli_h1("downloaded data to data folder for furture use ✔")


# +--------------------------+
# |     END                  |
# +--------------------------+

