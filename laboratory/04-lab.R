# ---------------------------------------------------------------
# 04-lab-2025
# confounding analysis in causal inference
# exploring different types of confounding using simulated data
# uses tidyverse and ggdag packages
# ---------------------------------------------------------------

# load libraries
if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}
# graphing 
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}
# automated causal diagrams
if (!require(ggdag, quietly = TRUE)) {
  install.packages("ggdag")
  library(ggdag)
}

# code for creating a DAG
graph_fork <- dagify(Y ~ L,
                     A ~ L,
                     exposure = "A",
                     outcome = "Y") |>
  tidy_dagitty(layout = "tree")
# plot the DAG
graph_fork |>
  ggdag() + theme_dag_blank() + labs(title = "L is a common cause of A and Y")

# next
ggdag::ggdag_adjustment_set(graph_fork) + 
  theme_dag_blank() + 
  labs(title = "{L} is the exclusive member of the confounder set for A and Y. Conditioning on L 'd-separates' A and Y ")

# simulate
# set seed
set.seed(123)
# number of observations
N = 1000
# confounder
L = rnorm(N)
# A is caused by 
A = rnorm(N, L)
# Y draws randomly from L but is not caused by A
Y = rnorm(N, L)
# note we did not need to make a data frame
# regress Y on A without control
fit_fork <- lm(Y ~ A)
# A is "significant
parameters::model_parameters(fit_fork)
# regression
fit_fork_controlled <- lm(Y ~ A + L)
# result
parameters::model_parameters(fit_fork_controlled)

# mediation bias
graph_mediation <- dagify(Y ~ M,
                          M ~ A,
                          exposure = "A",
                          outcome = "Y") |>
  ggdag::tidy_dagitty(layout = "tree")
graph_mediation |>
  ggdag() +   
  theme_dag_blank() + 
  labs(title = "Mediation Graph")

# or like this
graph_mediation_full <- ggdag_mediation_triangle(x = "A", 
                                                 y = "Y", 
                                                 m = "M", 
                                                 x_y_associated = FALSE) 
graph_mediation_full + theme_dag_blank() + 
  labs(title = "Fully Mediated Graph")

# ask which vars to condition on
ggdag::ggdag_adjustment_set(graph_fork)

# d connectedness
ggdag::ggdag_dconnected(graph_mediation)

# regressions
# make data
set.seed(123)
N <- 100
x <- rnorm(N) # sim x
m <- rnorm(N, x) # sim X -> M
y <- rnorm(N, x + m) # sim M -> Y
df <- data.frame(x, m, y)
df <- df |>
  dplyr::mutate(x_s = scale(x),
                m_s = scale(m))

fit_mediation <- lm(y ~ x_s, data = df)
parameters::model_parameters(fit_mediation)

# Next we ask, is A related to Y conditional on M? 
fit_total_mediated_effect <- lm(y ~ x_s + m_s, data = df)
parameters::model_parameters(fit_total_mediated_effect)

# initial example showing attenuated effect
fit_total_effect <- lm(y ~ x_s, data = df)
parameters::model_parameters(fit_total_effect)

# ---------------------------------------------------------------
# pipe confounding (full mediation)
# ---------------------------------------------------------------
# load required packages
library(ggdag)
library(dplyr)
library(ggplot2)
library(parameters)
library(skimr)

# visualize the mediation triangle with no direct effect (full mediation)
mediation_triangle(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE
) |>
  ggdag()

# simulate ritual action -> social cohesion -> charity example
set.seed(123)
# participants
N <- 100

# initial charitable giving
c0 <- rnorm(N, 10, 2)

# assign treatments and simulate charitable giving and increase in social cohesion
ritual <- rep(0:1, each = N/2)
cohesion <- ritual * rnorm(N, .5, .2)

# increase in charity
c1 <- c0 + ritual * cohesion 

# dataframe
d <- data.frame(
  c0 = c0, 
  c1 = c1, 
  ritual = ritual, 
  cohesion = cohesion
)
skimr::skim(d)

# does the ritual increase charity?
# model with only ritual condition
parameters::model_parameters(
  lm(c1 ~ c0 + ritual, data = d)
)

# does the ritual increase charity adjusting for levels of social cohesion?
parameters::model_parameters(
  lm(c1 ~ c0 + ritual + cohesion, data = d)
)

# note: the (direct) effect of ritual entirely drops out when we include both 
# ritual and social cohesion. once our model knows cohesion, it does not 
# obtain any new information by knowing ritual.

# ---------------------------------------------------------------
# masked relationships
# ---------------------------------------------------------------
# simulate example where two variables (C and R) affect K
# C has a negative effect on K, R has a positive effect on K
# C also causes R (positive relationship)

library(ggdag)
# create dag where conservatism affects religion and distress
dag_m1 <- dagify(K ~ C + R,
                 R ~ C,
                 exposure = "C",
                 outcome = "K") |>
  tidy_dagitty(layout = "tree")

# graph the dag
dag_m1 |>
  ggdag()

# simulate data for this scenario
# C -> K <- R
# C -> R
set.seed(123)
n <- 100
C <- rnorm(n)
R <- rnorm(n, C)
K <- rnorm(n, R - C)

d_sim <- data.frame(K = K, R = R, C = C)

# model with only C as predictor
ms1 <- parameters::model_parameters(
  lm(K ~ C, data = d_sim)
)
plot(ms1)
ms1

# model with only R as predictor
ms2 <- parameters::model_parameters(
  lm(K ~ R, data = d_sim)
)
plot(ms2)

# model with both C and R as predictors
# note how they "pop" in opposite directions (masking)
ms3 <- parameters::model_parameters(
  lm(K ~ C + R, data = d_sim)
)
plot(ms3)

# check ggdag adjustment recommendation
dag_m1 |>
  ggdag_adjustment_set()

# ---------------------------------------------------------------
# collider confounding
# ---------------------------------------------------------------
# selection-distortion effect (berkson's paradox)
# example from statistical rethinking

# create dag for selection based on newsworthy and trustworthy papers
dag_sd <- dagify(S ~ N,
                 S ~ T,
                 labels = c("S" = "Selection",
                            "N" = "Newsworthy",
                            "T" = "Trustworthy")) |>
  tidy_dagitty(layout = "nicely")

# graph the dag
dag_sd |>
  ggdag(text = FALSE, use_labels = "label") + theme_dag_blank()

# visualize d-separation when controlling for S
ggdag_dseparated(
  dag_sd,
  from = "T",
  to = "N",
  controlling_for = "S",
  text = FALSE,
  use_labels = "label"
) + theme_dag_blank()

# find colliders in the dag
ggdag::ggdag_collider(dag_sd,
                      text = FALSE,
                      use_labels = "label")

# simulate selection distortion effect
set.seed(123)
n <- 1000  # number of grant proposals
p <- 0.05  # proportion to select

d <- 
  # uncorrelated newsworthiness and trustworthiness
  dplyr::tibble(
    newsworthiness = rnorm(n, mean = 0, sd = 1),
    trustworthiness = rnorm(n, mean = 0, sd = 1)
  ) |>
  # total_score
  dplyr::mutate(total_score = newsworthiness + trustworthiness) |>
  # select top 5% of combined scores
  dplyr::mutate(selected = ifelse(total_score >= quantile(total_score, 1 - p), TRUE, FALSE))

# check correlation among selected proposals
d |> 
  dplyr::filter(selected == TRUE) |> 
  dplyr::select(newsworthiness, trustworthiness) |> 
  cor()

# create text for plot annotation
text <-
  dplyr::tibble(
    newsworthiness = c(2, 1),
    trustworthiness = c(2.25, -2.5),
    selected = c(TRUE, FALSE),
    label = c("selected", "rejected")
  )

# visualize the selection distortion effect
d |>
  ggplot2::ggplot(aes(x = newsworthiness, y = trustworthiness, color = selected)) +
  ggplot2::geom_point(aes(shape = selected), alpha = 3/4) +
  ggplot2::geom_text(data = text, aes(label = label)) +
  ggplot2::geom_smooth(
    data = d |> filter(selected == TRUE),
    method = "lm",
    fullrange = TRUE,
    color = "lightblue",
    se = FALSE,
    size = 1
  ) +
  ggplot2::scale_shape_manual(values = c(1, 19)) +
  ggplot2::scale_x_continuous(limits = c(-3, 3.9), expand = c(0, 0)) +
  ggplot2::coord_cartesian(ylim = range(d$trustworthiness)) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::xlab("Newsworthy") +
  ggplot2::ylab("Trustworthy") + theme_bw()

# ---------------------------------------------------------------
# collider bias within experiments
# ---------------------------------------------------------------
# example of collider bias in an experiment with post-treatment variable

# create dag for experiment with unmeasured variable
dag_ex2 <- dagify(
  C1 ~ C0 + U,
  Ch ~ U + R,
  labels = c(
    "R" = "Ritual",
    "C1" = "Charity-post",
    "C0" = "Charity-pre",
    "Ch" = "Cohesion",
    "U" = "Religiousness (Unmeasured)"
  ),
  exposure = "R",
  outcome = "C1",
  latent = "U"
) |>
  control_for(c("Ch", "C0"))  

# visualize the dag
dag_ex2 |>
  ggdag(text = FALSE, use_labels = "label")

# check for collider bias when conditioning on cohesion
dag_ex2 |>
  ggdag_collider(
    text = FALSE,
    use_labels = "label"
  ) +
  ggtitle("Cohesion is a collider that opens a path from ritual to charity")

# create an alternative dag
dag_ex3 <- dagify(
  C1 ~ C0,
  C1 ~ U,
  Ch ~ U + R,
  labels = c(
    "R" = "Ritual",
    "C1" = "Charity-post",
    "C0" = "Charity-pre",
    "Ch" = "Cohesion",
    "U" = "Religiousness (Unmeasured)"
  ),
  exposure = "R",
  outcome = "C1",
  latent = "U"
)
ggdag_adjustment_set(dag_ex3)

# ---------------------------------------------------------------
# taxonomy of confounding
# ---------------------------------------------------------------

# 1. the fork (omitted variable bias)
confounder_triangle(x = "Coffee",
                    y = "Lung Cancer",
                    z = "Smoking") |>
  ggdag_dconnected(text = FALSE, use_labels = "label")

# 2. the pipe (fully mediated effects)
mediation_triangle(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE
) |>
  tidy_dagitty(layout = "nicely") |>
  ggdag()

# 3. the collider
collider_triangle() |>
  ggdag_dseparated(controlling_for = "m")

# 4. confounding by proxy
dag_sd <- dagify(
  Z ~ X,
  Z ~ Y,
  D ~ Z,
  labels = c(
    "Z" = "Collider",
    "D" = "Descendant",
    "X" = "X",
    "Y" = "Y"
  ),
  exposure = "X",
  outcome = "Y"
) |>
  control_for("D") 

dag_sd |>
  ggdag_dseparated(
    from = "X",
    to = "Y",
    controlling_for = "D",
    text = FALSE,
    use_labels = "label"
  ) +
  ggtitle("X --> Y, controlling for D",
          subtitle = "D induces collider bias")

# ---------------------------------------------------------------
# complex example with multiple variables
# ---------------------------------------------------------------
# example of a complex dag with many variables and relationships

library(ggdag)
dg_1 <- ggdag::dagify(
  b ~ im + ordr + rel + sr + st,
  rel ~ age + ses + edu + male + cny,
  ses ~ cny + edu + age,
  edu ~ cny + male + age,
  im ~ mem + rel + cny,
  mem ~ age + edu + ordr,
  exposure = "sr",
  outcome = "b",
  labels = c(
    "b" = "statement credibility",
    "sr" = "source",
    "st" = "statement",
    "im" = "importance",
    "mem" = "memory",
    "s" = "source",
    "rel" = "religious",
    "cny" = "country",
    "mem" = "memory",
    "male" = "male",
    "ordr" = "presentation order",
    "ses" = "perceived SES",
    "edu" = "education",
    "age" = "age"
  )
) |>
  control_for("rel")

# check for colliders
ggdag::ggdag_collider(dg_1, text = FALSE, use_labels = "label")

# check d-separation when controlling for multiple variables
p3 <- ggdag::ggdag_dseparated(
  dg_1,
  from = "sr",
  to = "b",
  controlling_for = c("ses", "age", "cny", "im", "edu", "mem", "male", "rel"),
  text = FALSE,
  use_labels = "label"
) +
  theme_dag_blank() +
  labs(title = "Collider Confounding occurs when we `control for` a bunch of variables")
p3

# find adjustment set
p2 <- ggdag::ggdag_adjustment_set(dg_1,
                                  text = FALSE,
                                  use_labels = "label") +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for Source credibility from belief ")
p2

# ---------------------------------------------------------------
# alternative assumptions 
# ---------------------------------------------------------------
# altering the causal model by assuming source affects religion

# create dag with source affecting religion
dg_3 <- ggdag::dagify(
  b ~ im + ordr + rel + st + sr,
  rel ~ age + ses + edu + male + cny + sr,
  ses ~ cny + edu + age,
  edu ~ cny + male + age,
  im ~ mem + rel + cny,
  mem ~ age + edu + ordr,
  exposure = "rel",
  outcome = "b",
  labels = c(
    "b" = "statement credibility",
    "sr" = "source",
    "st" = "statement",
    "im" = "importance",
    "mem" = "memory",
    "s" = "source",
    "rel" = "religious",
    "cny" = "country",
    "mem" = "memory",
    "male" = "male",
    "ordr" = "presentation order",
    "ses" = "perceived SES",
    "edu" = "education",
    "age" = "age"
  )
) |>
  control_for("rel")

# visualize the dag
ggdag(dg_3, text = FALSE, use_labels = "label")

# get adjustment sets for estimating effect of source on belief
ggdag::ggdag_adjustment_set(
  dg_3,
  exposure = "sr",
  outcome = "b",
  text = FALSE,
  use_labels = "label"
) +
  theme_dag_blank() +
  labs(title = "Adjustment set",
       subtite = "Model for Source credibility from belief ")

# ---------------------------------------------------------------
# nzavs research example
# ---------------------------------------------------------------
# example from new zealand attitudes and values study research

# create dag for wellbeing and belief relationships
tidy_ggdag <- dagify(
  WB ~ belief + age_within + age_between + partner + nzdep + urban + male + pols + empl,
  WB ~~ partner,
  belief ~ age_within + age_between + male + ethn,
  partner ~ nzdep + age_within + age_between + belief, 
  nzdep ~ empl + age_within + age_between,
  pols ~ age_within + age_between + empl + ethn,
  empl ~ edu + ethn + age_within + age_between,
  exposure = "belief",
  outcome = "WB") |>
  tidy_dagitty()

# visualize the dag
tidy_ggdag |>
  ggdag()

# find adjustment sets for this dag
ggdag::ggdag_adjustment_set(tidy_ggdag, node_size = 14) + 
  theme(legend.position = "bottom") + theme_dag_blank()

# ---------------------------------------------------------------
# unmeasured common causes
# ---------------------------------------------------------------
# example with unmeasured common cause of religion and conservatism

# create dag with unmeasured variable
dag_m3 <- dagify(
  K ~ C + R,
  C ~ U,
  R ~ U,
  exposure = "C",
  outcome = "K",
  latent = "U"
) |>
  tidy_dagitty(layout = "nicely")

# visualize the dag
dag_m3 |>
  ggdag()

# find adjustment sets
ggdag::ggdag_adjustment_set(dag_m3)

# simulate this relationship
# C -> K <- R
# C <- U -> R
n <- 100
U <- rnorm(n)
R <- rnorm(n, U)
C <- rnorm(n, U)
K <- rnorm(n, R - C)
d_sim3 <- data.frame(K = K, R = R, U = U, C = C)

# ---------------------------------------------------------------
# smoking and cardiac arrest example
# ---------------------------------------------------------------
# example from ggdag package

# create dag for smoking and cardiac arrest relationship
smoking_ca_dag <- dagify(
  cardiacarrest ~ cholesterol,
  cholesterol ~ smoking + weight,
  smoking ~ unhealthy,
  weight ~ unhealthy,
  labels = c(
    "cardiacarrest" = "Cardiac\n Arrest",
    "smoking" = "Smoking",
    "cholesterol" = "Cholesterol",
    "unhealthy" = "Unhealthy\n Lifestyle",
    "weight" = "Weight"
  ),
  latent = "unhealthy",
  exposure = "smoking",
  outcome = "cardiacarrest"
)

# visualize the dag
ggdag(smoking_ca_dag,
      text = FALSE,
      use_labels = "label")

# find adjustment sets
ggdag_adjustment_set(
  smoking_ca_dag,
  text = FALSE,
  use_labels = "label",
  shadow = TRUE
)

# check effect of controlling for cholesterol
ggdag_dseparated(
  smoking_ca_dag,
  controlling_for = c("weight", "cholesterol"),
  text = FALSE,
  use_labels = "label",
  collider_lines = FALSE
)

# ---------------------------------------------------------------
# selection bias in sampling
# ---------------------------------------------------------------
# example from ggdag documentation

# create coordinates for custom dag layout
coords_mine <- tibble::tribble(
  ~name, ~x, ~y,
  "glioma", 1, 2,
  "hospitalized", 2, 3,
  "broken_bone", 3, 2,
  "reckless", 4, 1,
  "smoking", 5, 2
)

# create and visualize dag for hospitalization selection bias
dagify(hospitalized ~ broken_bone + glioma,
       broken_bone ~ reckless,
       smoking ~ reckless,
       labels = c(hospitalized = "Hospitalization",
                  broken_bone = "Broken Bone",
                  glioma = "Glioma",
                  reckless = "Reckless \nBehavior",
                  smoking = "Smoking"),
       coords = coords_mine) |> 
  ggdag_dconnected("glioma", "smoking", controlling_for = "hospitalized", 
                   text = FALSE, use_labels = "label", collider_lines = FALSE)

# ---------------------------------------------------------------
# selection bias in longitudinal research
# ---------------------------------------------------------------
# example with homeownership selection bias

# create dag for ethnicity and ecological values relationship
dag_sel <- dagify(
  retained ~ homeowner,
  homeowner ~ income + ethnicity,
  ecologicalvalues ~ ethnicity + income,
  labels = c(
    retained = "retained",
    homeowner = "homeowner",
    ethnicity = "ethnicity",
    income = "income",
    ecologicalvalues = "Ecological \n Orientation"
  ),
  exposure = "ethnicity",
  outcome = "ecologicalvalues"
) |>
  control_for("retained")

# visualize the dag with adjustment
dag_sel |>
  ggdag_adjust(
    "retained",
    layout = "mds",
    text = FALSE,
    use_labels = "label",
    collider_lines = FALSE
  )

# check for colliders
ggdag_collider(dag_sel)

# check d-separation when controlling for retained
ggdag_dseparated(
  dag_sel,
  controlling_for = "retained",
  text = FALSE,
  use_labels = "label",
  collider_lines = TRUE
)

# find adjustment sets
ggdag_adjustment_set(dag_sel)

# ---------------------------------------------------------------
# workflow summary
# ---------------------------------------------------------------
# 1. import your data
# 2. check that data types are correct
# 3. graph your data
# 4. consider your question
# 5. if causal, draw your dag/s
# 6. explain your dag's
# 7. write your model
# 8. run your model
# 9. graph and interpret your results
# 10. return to your question, and assess what you have learned

# ---------------------------------------------------------------
# key takeaways
# ---------------------------------------------------------------
# - we control for variables to avoid omitted variable bias
# - omitted variable bias is real, but also commonplace is included variable bias
# - included variable biases arise from "pipes", "colliders", and conditioning on descendant of colliders
# - the ggdag package can help to obtain causal inference, but it relies on assumptions that are not part of your data
# - clarify your assumptions