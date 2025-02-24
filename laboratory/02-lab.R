# your name here
# 29 Feb 2024
# lab 02

# install libraries -------------------------------------------------------
# libraries

# run these lines of code to install packages.  `command + return` (mac)


# graphs
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}

# data wrangling
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
} else {
  library(tidyverse)
}

# tables
if (!require(parameters)) {
  install.packages("parameters")
  library(parameters)
} else {
  library(parameters)
}

# reporting
if (!require(report)) {
  install.packages("report")
  library(report)
} else {
  library(report)
}


# predictive graphs
if (!require(ggeffects)) {
  install.packages("ggeffects")
  library(ggeffects)
} else {
  library(ggeffects)
}

# assembling graphs
if (!require(patchwork)) {
  install.packages("patchwork")
  library(patchwork)
} else {
  library(patchwork)
}


# here package for folder location
if (!require(here)) {
  install.packages("here")
  library(here)
} else {
  library(here)
}



# libraries ---------------------------------------------------------------
# libraries should already be loaded. henceforth, just run them `command + return`
library(tidyverse)
library(ggplot2)
library(patchwork)
library(parameters)
library(report)
library(patchwork)
library(here)


# initial data simulation -------------------------------------------------

set.seed(123) # use any number to set the seed
n <- 100 # number of observations
mean <- 50
sd <- 10

data_continuous <- rnorm(n, mean, sd)


# view
head(data_continuous)

# view using base R histogram
hist(data_continuous)

#  simulate categorical data ----------------------------------------------

levels <- c("Male", "Female")
data_categorical <- sample(levels, n, replace = TRUE)

# view
head(data_categorical)

# check
table(data_categorical)


# generate data with unequal probabilities --------------------------------
# define levels and number of observations
levels <- c("Male", "Female")
n <- 100 # total number of observations

# generate categorical data with unequal probabilities
data_categorical_unequal <- sample(levels, n, replace = TRUE, prob = c(0.3, 0.7))

# view the first few elements
# head(data_categorical_unequal)

# check
table(data_categorical_unequal)



# simulate outcomes for treatments ----------------------------------------

set.seed(123) # reproducibility
groupA_scores <-
  rnorm(100, mean = 100, sd = 15) # simulate scores for group A
groupB_scores <-
  rnorm(100, mean = 105, sd = 15) # simulate scores for group B

# ombine into a data frame
df_scores <-
  data.frame(
    group = rep(c("A", "B"), each = 100),
    scores = c(groupA_scores, groupB_scores)
  )

# commands to view data
# str(df_scores)

# summary of columns
# summary(df_scores)

# top rows (uncomment)
# head(df_scores)

# bottom rows  (uncomment)
# tail(df_scores)

# check structure of data  (uncomment)
# str(df_scores)
str(mtcars)

# make group a factor
df_scores_1 <- df_scores |>
  mutate(group = as.factor(group))

head(df_scores_1)


# this is vs code section heading  ----------------------


# visualising simulated data ----------------------------------------------

# plot your data
ggplot(df_scores_1, aes(x = group, y = scores, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Score Distribution by Group", x = "Group", y = "Scores")



# histogram ---------------------------------------------------------------

library(ggplot2)

# H=histograms for both groups
ggplot(df_scores_1, aes(x = scores, fill = group)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(
    title = "Distribution of Scores by Group",
    x = "Scores",
    y = "Frequency"
  ) +
  facet_wrap(~group, ncol = 1) +
  theme_minimal()

# EXERCISE ----------------------------------------------------------------

## Exercise 1 -----------------------

# Modify the simulation parameters to change each group’s mean and standard deviation.
# Observe how these changes affect the distribution.

# Go to the histogram. Experiment with different bin widths. In your own words, how do large and small numbers speak differently to the data? When might you use one histogram and not another.





# Simulating data for common statistical tests ----------------------------

# simulate some data
set.seed(123)
data <-
  rnorm(100, mean = 5, sd = 1) # 100 random normal values with mean = 5

# perform one-sample t-test
# testing if the mean of the data is reliably different from 4
mod_first_test <- t.test(data, mu = 4)

# table
parameters::parameters(mod_first_test)

# report
report::report(mod_first_test)


# simulate data for two groups --------------------------------------------

# simulate data for two groups
set.seed(123)

group1 <-
  rnorm(50, mean = 5, sd = 1) # 50 random normal values, mean = 5
group2 <-
  rnorm(50, mean = 5.5, sd = 1) # 50 random normal values, mean = 5.5

# two-sample t-test
mod_t_test_result <- t.test(group1, group2)

# report
report::report(mod_t_test_result)

# table
parameters::parameters(mod_t_test_result)



# pre-post simulation -----------------------------------------------------

# simulate pre-test and post-test scores
set.seed(123)

pre_test <- rnorm(30, mean = 80, sd = 10)
post_test <-
  rnorm(30, mean = pre_test + 5, sd = 5) # assume an increase

# perform paired t-test
mod_pre_post <- t.test(pre_test, post_test, paired = TRUE)

# report
report::report(mod_pre_post)

# table
parameters::parameters(mod_pre_post)



# regression understood through simulation --------------------------------
# library for enhanced model reporting
set.seed(123)

library(parameters)
library(report)


# set seed for reproducibility
set.seed(123) # choose a seed number for consistency

# define the number of observations
n <- 100 # total observations

# simulate continuous treatment variable A
treatment <- rnorm(n, mean = 50, sd = 10) # mean = 50, sd = 10 for A

# specify the effect size of A on Y
beta_a <- 2 # explicit effect size

# simulate outcome variable Y including an error term
# Y = intercept + beta_a*A + error
outcome <- 5 + beta_a * treatment + rnorm(n, mean = 0, sd = 20)

# create a dataframe
df <- data.frame(treatment = treatment, outcome = outcome)

# view the structure and first few rows of the data frame
str(df)
head(df)


# regression analysis of continuous treatment -----------------------------


set.seed(123)

# perform linear regression of Y on A
fit <- lm(outcome ~ treatment, data = df)

# display the regression model summary
summary(fit)

# report the model in a reader-friendly format
report_fit <- report::report(fit)
print(report_fit)

# use ggeffects to view predicted values
library(ggeffects)

predicted_values <- ggeffects::ggemmeans(fit,
  terms = c("treatment")
)

# plot see
plot(
  predicted_values,
  dot_alpha = 0.35,
  show_data = TRUE,
  jitter = .1
)

# equivalence of anova and regression -------------------------------------

# nice tables
library(parameters)

set.seed(123) # reproducibility
n <- 90 # total number of observations
k <- 3 # number of groups

# simulate independent variable (grouping factor)
group <- factor(rep(1:k, each = n / k))

# inspect
str(group)

# simulate outcome variable
means <- c(100, 100, 220) # Mean for each group
sd <- 15 # Standard deviation (same for all groups)

# generate random data
y <- rnorm(n, mean = rep(means, each = n / k), sd = sd)

# make data frame
df_1 <- cbind.data.frame(y, group)

# anova -------------------------------------------------------------------
anova_model <- aov(y ~ group, data = df_1)
# summary(anova_model)
table_anova <- model_parameters(anova_model)

# report the model
report::report(anova_model)




# regression --------------------------------------------------------------


# for tables (just installed)
library(parameters)

# regression model
fit_regression <- lm(y ~ group, data = df_1)

# uncomment if you want an ordinary summary
# summary(regression_model)

table_fit <- parameters::model_parameters(fit_regression)

# print table
table_fit



# graph the output of the parameters table
# visualisation
coefficient_plot <- plot(table_fit)


# ggeffects plot
predictive_plot <- plot(
  ggeffects::ggpredict(fit_regression, terms = "group"),
  dot_alpha = 0.35,
  show_data = TRUE,
  jitter = .1,
  colors = "reefs"
) +
  scale_y_continuous(limits = c(0, 260)) + # change y axis
  labs(title = "Predictive Graph", x = "Treatment Group", y = "Response")

# view (uncomment)
predictive_plot

# show all color palettes (uncomment)
show_pals()


# multiple plots
library(patchwork)

# create a plot
my_first_combination_plot <- coefficient_plot / predictive_plot +
  plot_annotation(
    title = "Coefficient and Predictive plots",
    tag_levels = "A"
  )

# view
my_first_combination_plot


# save a plot to your directory, make sure to create one called "figs"

## check directory
here::here()

# save (change values if necessary )
ggsave(
  my_first_combination_plot,
  path = here::here("figs"),
  width = 12,
  height = 8,
  units = "in",
  filename = "my_first_combination_plot.jpeg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 600
)
