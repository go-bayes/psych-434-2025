# functions explained here: https://go-bayes.github.io/margot/

# installation
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# reinstall the `margot` packagewith updates
# devtools::install_github("go-bayes/margot", quietly = TRUE)

# call package
library("margot")
library("tidyverse")
library("parameters")
library("skimr")
library("haven")
library("stdReg")
library('mice')
library("clarify")

# uncomment and check simulated data