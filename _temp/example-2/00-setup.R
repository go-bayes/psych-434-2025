# 00 get data in
# example script 1: data wrangling
# spring 2025
# example estimation of average treatment effect - script 1
# questions: joseph.bulbulia@vuw.ac.nz

# restart fresh session if needed
# rstudioapi::restartSession()

# set seed for reproducibility
set.seed(123)

# save paths -------------------------------------------------------------------
# specify the path where data will be saved
# this is the path used by joseph
# push_mods <- here::here('/Users/joseph/v-project\ Dropbox/data/courses/25-psych-434')
# replace with your own path after creating a data file

pull_mods_data <- here::here("data")

# load libraries ---------------------------------------------------------
# install and load 'margot' package if not already installed
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot") # ensure version is at least 1.0.32
  library("margot")
}

# load required libraries
library(margot)
library(tidyverse)
library(qs)
library(here)

# import synthetic data ---------------------------------------------------
#link to synthetic data
url <- "https://www.dropbox.com/scl/fi/ru0ecayju04ja8ky1mhel/df_nz_long.qs?rlkey=prpk9a5v4vcg1ilhkgf357dhd&dl=1"

# download data to a temporary file
tmp <- tempfile(fileext = ".qs")
download.file(url, tmp, mode = "wb")

# read data into R
library(qs)
df_nz_long <- qread(tmp)

# # view first few rows of synthetic data
head(df_nz_long)

# # save data to your directory for later use and comment the above
margot::here_save_qs(df_nz_long,"df_nz_long" ,pull_mods_data)

