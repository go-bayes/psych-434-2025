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



# seed
set.seed(123)

# generate 10 samples, average 170, sd = 20
draws_10 <- rnorm(10, mean = 170, sd = 20)

# ggplot quick-histogram
ggplot2::qplot(draws_10, binwidth = 2)



# reproducibility 
set.seed(123)

# generate 100 samples, average 170, sd = 20
draws_100 <-rnorm(100, mean = 170, sd = 20)

# graph
ggplot2::qplot(
  draws_100, binwidth = 2
)


# reproducibility
set.seed(123)

# N = 10,000
draws_10000 <- rnorm(1e5, mean = 170, sd = 20)

# plot
ggplot2::qplot(draws_10000, binwidth = 2)


# syntax for an intercept-only model
model <- lm(outcome ~ 1, data = data)

# base R summary
summary(model)


#|code-fold: false

#write the model and get a nice table for it
sjPlot::tab_model(lm(draws_10 ~ 1))


# larger
sjPlot::tab_model(lm(draws_100 ~ 1))


# larger
sjPlot::tab_model(lm(draws_10000 ~ 1))


# compare
sjPlot::tab_model(lm(draws_10 ~ 1),
                  lm(draws_100 ~ 1),
                  lm(draws_10000 ~ 1))


# regression single co-variate

# import data
df_pearson_lee <-
  data.frame(read.table(
    url(
      "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/PearsonLee/data/MotherDaughterHeights.txt"
    ),
    header = TRUE
  ))

# save
# saveRDS(df_pearson_lee, here::here("data", "df_pearson_lee"))

# Center mother's height for later example
df_pearson_lee_centered <- df_pearson_lee |>
  dplyr::mutate(mother_height_c = as.numeric(scale(
    mother_height, center = TRUE, scale = FALSE
  )))

skimr::skim(df_pearson_lee_centered)


# explore pearson lee data
explore_md <-
  ggplot2::ggplot(data = df_pearson_lee_centered, aes(y = daughter_height, x = mother_height)) +
  geom_jitter(alpha = .2) +
  labs(title = "The relationship between mothers height and daughter's height") +
  ylab("Daughter's height") +
  xlab("Mother's height") + theme_classic()

# print
print( explore_md )


# regression 
m1 <-
  lm(daughter_height ~ mother_height, data = df_pearson_lee_centered)

# graph
sjPlot::tab_model(m1)


# get model parameters
t_m1<-parameters::model_parameters(m1,  
                                   ci = 0.95)
# plot 
plot(t_m1) +
  labs(title = "The relationship between mothers height and daughter's height") + 
  ylab("Daughter's height") 


library("equatiomatic")
# extract equation
extract_eq(m1, use_coefs = FALSE)

library(ggeffects)
predictions <- ggeffects::ggpredict(m1, terms = "mother_height",    
                                    add.data = TRUE,
                                    dot.alpha = .1,
                                    jitter = TRUE)


plot_predictions <-
  plot(predictions) +   theme_classic() + labs(title = "Predicted values of daughter's height from the Pearson/Fox 1903 dataset")
plot_predictions


# use the `expand.grid` command to create a sequence of points for mother's height
df_expand_grid <- expand.grid(mother_height = c(25:91))

# use the `predict` function to create a new response
df_predict <-
  predict(m1,
          type = "response",
          interval = "confidence",
          newdata = df_expand_grid)

# have a look at the object
#dplyr::glimpse(df_predict)

# create a new dataframe for the new sequence of points for mother's height and the predicted data
newdata <- data.frame(df_expand_grid, df_predict)
head(newdata)

# graph the expected results
predplot <- ggplot(data = newdata,
                   aes(x = mother_height, y = fit))  +
  geom_point() +  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .1) +
  expand_limits(x = c(20, 91), y = c(0, 81))  + theme_classic() +
  labs(title = "Predicted values for a broader population")

# plot the two graphs together (making the x and y axis at the same scale 
library("patchwork")
# rescale heightplot

# old plot with the new axis and y-axis scales, and remove points

plot_height <- plot(predplot, add.data = FALSE) +   theme_classic()


plot_height_title <-
  plot_height +  expand_limits(x = c(20, 91), y = c(0, 81)) +  labs(title = "Predicted values of daughter's height from the Pearson/Fox 1903 dataset")

# double graph
plot_height_title / predplot  + plot_annotation(title = "What do you notice about these relationships?", tag_levels = "a")


# prediction plots
library(ggeffects)
# predicted values of mother height on daughter height
ggeffects::ggpredict(m1, terms = "mother_height")


# simulate nonlinear relationship between x and y
b <- c(2, 0.75)
set.seed(12)
x <- rnorm(100)
set.seed(12)
y <- rnorm(100, mean = b[1] * exp(b[2] * x))
dat1 <- data.frame(x, y)

ot1 <- lm(y ~ x, data  = dat1)
# performance::check_model(ot1)

# plot linear effect
plot(ggeffects::ggpredict(ot1, terms = "x",
                          add.data = TRUE,
                          dot.alpha = .4))


# model: quadratic
fit_non_linear <- lm(y ~ x + I(x ^ 2), data  = dat1)

# predictive plot
plot(ggeffects::ggpredict(fit_non_linear, terms = "x",
                          add.data = TRUE,
                          dot.alpha = .4))


# non-linear regression 
library(splines)

# fit model 
fit_non_linear_b <- lm(y ~ x + poly(x, 2), data  = dat1)

# graph model
plot(
  ggeffects::ggpredict(fit_non_linear_b, terms = "x",
                       add.data = TRUE,
                       dot.alpha = .4
  ))

# fit spline: not specified
fit_non_linear_c <-lm(y ~ bs(x), data  = dat1)

# model parameters: coefficients are not interpretable
parameters::model_parameters(
  fit_non_linear_c
)

#performance::check_model(ot2)
plot(
  ggeffects::ggpredict(fit_non_linear_c, terms = "x",
                       add.data = TRUE,
                       dot.alpha = .4
  ))


# centering
library(ggeffects)

# fit raw data 
fit_raw <- lm(daughter_height ~ mother_height, data = df_pearson_lee_centered)

# fit centred data
fit_centered <-
  lm(daughter_height ~ mother_height_c, data = df_pearson_lee_centered)

# compare the models
sjPlot::tab_model(fit_raw, fit_centered)


# graph centred model
plot(
  ggeffects::ggpredict(fit_centered, terms = "mother_height_c",
                       add.data = TRUE,
                       dot.alpha = .4
  ))


#. model evaluation

# load library 
library(performance)
# intercept only
fig_intercept_only <- lm(daughter_height ~ 1, data = df_pearson_lee)

# covariate added
fig_covariate <- lm(daughter_height ~ mother_height, data = df_pearson_lee)

# evaluate
performance::compare_performance(fig_intercept_only, fig_covariate)


# report

report::report_statistics(fig_covariate)



#  do simulation ----------------------------------------------------------


