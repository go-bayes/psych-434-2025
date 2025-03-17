# ---------------------------------------------------------------
# lab 3 - regression in r, graphing, and elements of simulation
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# step 1: set up your r environment
# ---------------------------------------------------------------

# load libraries for graphing, data wrangling, tables, and reporting
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(parameters, quietly = TRUE)) {
  install.packages("parameters")
  library(parameters)
}

if (!require(report, quietly = TRUE)) {
  install.packages("report")
  library(report)
}

if (!require(ggeffects, quietly = TRUE)) {
  install.packages("ggeffects")
  library(ggeffects)
}

if (!require(patchwork, quietly = TRUE)) {
  install.packages("patchwork")
  library(patchwork)
}

# ---------------------------------------------------------------
# step 2: set a seed for reproducibility
# ---------------------------------------------------------------
set.seed(123) # use any number to set the seed

# ---------------------------------------------------------------
# step 3: simulate continuous data: one variable
# ---------------------------------------------------------------
n <- 100 # number of observations
mean <- 50
sd <- 10
data_continuous <- rnorm(n, mean, sd)

# view
head(data_continuous)

# view using base r histogram
hist(data_continuous)

# ---------------------------------------------------------------
# step 4: simulate categorical data
# ---------------------------------------------------------------
# with equal probabilities
levels <- c("Male", "Female")
data_categorical <- sample(levels, n, replace = TRUE)

# view
head(data_categorical)

# check distribution
table(data_categorical)

# with unequal probabilities
data_categorical_unequal <- sample(levels, n, replace = TRUE, prob = c(0.3, 0.7))

# check distribution
table(data_categorical_unequal)

# ---------------------------------------------------------------
# simulating outcomes from treatments
# ---------------------------------------------------------------
# simulate scores for two groups
set.seed(123) # reproducibility
groupA_scores <- rnorm(100, mean = 100, sd = 15) # simulate scores for group a
groupB_scores <- rnorm(100, mean = 105, sd = 15) # simulate scores for group b

# combine into a data frame
df_scores <- data.frame(
  group = rep(c("A", "B"), each = 100), 
  scores = c(groupA_scores, groupB_scores)
)

# commands to view data
# str(df_scores)
# summary(df_scores)
# head(df_scores)
# tail(df_scores)

# make group a factor (not strictly necessary here, but useful in other applications)
df_scores_1 <- df_scores |> 
  mutate(group = as.factor(group))

head(df_scores_1)

# ---------------------------------------------------------------
# visualising our simulated data
# ---------------------------------------------------------------
# box plot comparing groups
ggplot(df_scores_1, aes(x = group, y = scores, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Score Distribution by Group", x = "Group", y = "Scores")

# histograms for both groups
ggplot(df_scores_1, aes(x = scores, fill = group)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Distribution of Scores by Group",
       x = "Scores",
       y = "Frequency") +
  facet_wrap(~group, ncol = 1) +
  theme_minimal()

# ---------------------------------------------------------------
# testing for group differences
# ---------------------------------------------------------------
# t-test to compare groups
t_test_result <- t.test(scores ~ group, data = df_scores_1)
t_test_result

# or using report package
report::report(t_test_result)

# ---------------------------------------------------------------
# linear regression model
# ---------------------------------------------------------------
# fit a linear model
model <- lm(scores ~ group, data = df_scores_1)

# view model summary
summary(model)

# nicer output with parameters package
parameters::model_parameters(model)

# report model
report::report(model)

# ---------------------------------------------------------------
# visualizing model predictions
# ---------------------------------------------------------------
# create prediction plot
pred_plot <- ggeffects::ggpredict(model, terms = "group") |>
  plot() +
  labs(title = "Predicted Scores by Group",
       x = "Group", 
       y = "Predicted Score")

print(pred_plot)

# ---------------------------------------------------------------
# more complex simulations: effect of treatment conditional on a moderator
# ---------------------------------------------------------------
# simulate a moderator variable
set.seed(456)
n <- 200 # total observations
age <- rnorm(n, mean = 35, sd = 10) # continuous moderator
treatment <- sample(c("Control", "Treatment"), n, replace = TRUE) # treatment assignment

# effect that varies by age (interaction)
# younger people benefit more from treatment
effect_size <- ifelse(treatment == "Treatment", 
                      15 - 0.3 * age, # treatment effect decreases with age
                      0)             # no effect in control group

# create outcome with some noise
outcome <- 50 + effect_size + rnorm(n, mean = 0, sd = 10)

# combine into data frame
df_moderation <- data.frame(
  treatment = factor(treatment),
  age = age,
  outcome = outcome
)

# fit interaction model
mod_model <- lm(outcome ~ treatment * age, data = df_moderation)
summary(mod_model)

# visualize interaction effect
interaction_plot <- ggeffects::ggpredict(mod_model, terms = c("age[all]", "treatment")) |>
  plot() +
  labs(title = "Treatment Effect Conditional on Age",
       x = "Age", 
       y = "Predicted Outcome")

print(interaction_plot)

# ---------------------------------------------------------------
# saving plots and results
# ---------------------------------------------------------------
# save the interaction plot to file
ggsave("treatment_by_age_interaction.png", interaction_plot, width = 8, height = 6)

# save the model results
sink("moderation_model_results.txt")
summary(mod_model)
parameters::model_parameters(mod_model)
report::report(mod_model)
sink()

# ---------------------------------------------------------------
# combining plots with patchwork
# ---------------------------------------------------------------
# create a basic scatter plot
scatter_plot <- ggplot(df_moderation, aes(x = age, y = outcome, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Raw Data with Regression Lines",
       x = "Age", 
       y = "Outcome")

# combine plots
combined_plot <- scatter_plot / interaction_plot + 
  plot_annotation(
    title = "Treatment Effects by Age",
    subtitle = "Raw data (top) and model predictions (bottom)",
    caption = "Simulated data for demonstration"
  )

print(combined_plot)

# save combined plot
ggsave("combined_treatment_age_analysis.png", combined_plot, width = 10, height = 12)