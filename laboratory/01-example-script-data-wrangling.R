# Example script 1: data wrangling
# Spring 2025
# example estimation of average treatment effect - script 1
# questions: joseph.bulbulia@vuw.ac.nz



# import synthetic data ---------------------------------------------------
# link
url <- "https://dl.dropboxusercontent.com//scl/fi/ru0ecayju04ja8ky1mhel/df_nz_long.qs?rlkey=prpk9a5v4vcg1ilhkgf357dhd&st=7ijbdcw2&dl=1"

# download to a temporary file
tmp <- tempfile(fileext = ".qs")
download.file(url, tmp, mode = "wb")

# read it into R
library(qs)
df_nz_long <- qread(tmp)

# view synthetic data
head(df_nz_long)
