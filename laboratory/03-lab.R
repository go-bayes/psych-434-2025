# lab 03
# joseph.bulbulia@vuw.ac.nz
# march 11 2024

if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# simulated data (will take time to download)
# devtools::install_github("go-bayes/margot")
library("margot")
library("margot")
# List all datasets available in the package
# data(package = "margot")
#data("df_nz", package = "margot")

# for equations 
# if (!require(devtools, quietly = TRUE)) {
#   install.packages("remotes")
#   library(remotes)
# }
# remotes::install_github("datalorax/equatiomatic")
# 
# library("equatiomatic")

library(margot)
# data wrangling
if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)

}

# regression splines
# for equations 
if (!require(devtools, quietly = TRUE)) {
  install.packages("splines")
  library(splines)
}

# reports
if (!require(report, quietly = TRUE)) {
  install.packages("report")
  library(report)
}

# for equations 
if (!require(performance, quietly = TRUE)) {
  install.packages("performance")
  library(performance)
}

# graphs 
if (!require(ggeffects, quietly = TRUE)) {
  install.packages("ggeffects")
  library(ggeffects)
}

# more graphs
if (!require(sjPlot, quietly = TRUE)) {
  install.packages("sjPlot")
  library(sjPlot)
}

# data wrangling
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

# graphing
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# summary stats
if (!require(skimr, quietly = TRUE)) {
  install.packages("skimr")
  library(skimr)
}

# tables 
if (!require(gtsummary, quietly = TRUE)) {
  install.packages("gtsummary")
  library(gtsummary)
}

# easy tables
if (!require(table1, quietly = TRUE)) {
  install.packages("table1")
  library(table1)
}


# table summaries
if (!require(kableExtra, quietly = TRUE)) {
  install.packages("kableExtra")
  library(kableExtra)
}

# multi-panel plots
if (!require(patchwork, quietly = TRUE)) {
  install.packages("patchwork")
  library(patchwork)
}

if (!require(performance, quietly = TRUE)) {
  install.packages("performance")
  library(performance)
}


# simulate sample size ----------------------------------------------------


# seed
set.seed(123)

# generate 10 samples, average 170, sd = 20
draws_10 <- rnorm(10, mean = 170, sd = 20)

# ggplot quick-histogram
ggplot2::qplot(draws_10, binwidth = 2)
