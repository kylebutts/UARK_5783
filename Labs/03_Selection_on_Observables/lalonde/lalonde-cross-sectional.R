## lalonde-cross-sectional.R -----------------------------------------------
## Cross-sectional treatment-effect estimators using the LaLonde data
library(tidyverse)
library(fixest)
library(haven)
library(MatchIt)
library(WeightIt)
library(cobalt)
library(teffects2) # remotes::install_github("deryauysal/teffects2", subdir = "R")
library(teffects) # remotes::install_github("kylebutts/teffects-r")
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(rpart)

## Experimental sample ---------------------------------------------------------
## Load experimental data
df_exp <- read_csv(
  "Labs/03_Selection_on_Observables/lalonde/lalonde_exp.csv"
) |>
  rename(income_75 = re75, income_78 = re78) |>
  mutate(
    unemployed_75 = income_75 == 0,
    nodegree = education < 12
  ) |>
  mutate(
    Y = income_78,
    D = as.numeric(treat)
  )


### Balance Table --------------------------------------------------------------
# fmt: skip
(balance_exp <- df_exp |>
  cobalt::bal.tab(
    treat ~ age + education + nodegree + married + black + hispanic + income_75 + unemployed_75,
    data = _,
    disp = "means",
    stats = "mean.diffs",
    var.names = c(
      "age" = "Age",
      "education" = "Years of Schooling",
      "nodegree" = "Proportion High School Dropouts",
      "married" = "Proportion Married",
      "black" = "Proportion Black",
      "hispanic" = "Proportion Hispanic",
      "income_75" = "Real Earnings in 1975 (thousand)",
      "unemployed_75" = "Proportion Unemployed in 1975"
    )
  ))

### Difference in means --------------------------------------------------------

## By hand
(with(df_exp, mean(Y[D == 1]) - mean(Y[D == 0])))
mean(df_exp$Y[df_exp$D == 1]) - mean(df_exp$Y[df_exp$D == 0])

## Via regression
feols(
  Y ~ i(D),
  data = df_exp,
  vcov = "HC1" ## Robust standard errors!
)

## Non-experimental sample -----------------------------------------------------
cps <- read_csv(
  "Labs/03_Selection_on_Observables/lalonde/lalonde_nonexp_cps.csv"
) |>
  rename(income_75 = re75, income_78 = re78) |>
  mutate(
    unemployed_75 = income_75 == 0,
    nodegree = education < 12
  ) |>
  mutate(
    Y = income_78,
    D = as.numeric(treat)
  )

df_nonexp <- bind_rows(df_exp |> filter(D == 1), cps)


### Difference in means --------------------------------------------------------

## By hand
(with(df_nonexp, mean(Y[D == 1]) - mean(Y[D == 0])))

## Via regression
feols(
  Y ~ i(D),
  data = df_nonexp,
  vcov = "HC1"
)

### Adding controls ------------------------------------------------------------
# fmt: skip
fixest::setFixest_fml(
  ..x = ~ age + education + i(married) + i(nodegree) + i(black) + i(hispanic) + income_75 +  i(unemployed_75)
)
feols(
  Y ~ i(D) + ..x,
  data = df_nonexp,
  vcov = "HC1"
)


## Selection on Observables Estimators -----------------------------------------
### Nearest-neighbor matching on covariates ------------------------------------
# Match each treated unit to one control using Mahalanobis distance.

# fmt: skip
match_covariates <- matchit(
  treat ~ age + education + nodegree + married + black + hispanic + income_75 + unemployed_75,
  data = df_nonexp,
  method = "nearest",
  distance = "mahalanobis",
  estimand = "ATT",
  exact = ~ nodegree + married,
  ratio = 4,
  replace = FALSE
)

df_nn_matched <- match_data(match_covariates)
feols(
  Y ~ i(D),
  data = df_nn_matched,
  weights = ~weights,
  vcov = "HC1"
)

# Inspect balance before interpreting the estimate.
balance_covariates <- summary(match_covariates, standardize = TRUE)
print(balance_covariates)
love.plot(match_covariates)


### Regression adjustment ------------------------------------------------------
# Fit separate outcome models for treated and control units.
outcome_0 <- feols(Y ~ ..x, data = df_nonexp |> filter(D == 0))
df_nonexp$Y0_hat <- predict(outcome_0, newdata = df_nonexp)

## With regression
feols(
  Y - Y0_hat ~ i(D),
  data = df_nonexp,
  vcov = "HC1"
)

## With teffects2
ra_att <- teffects2::teffect(
  exposure.formula = D ~ 1,
  outcome.formula = xpd(Y ~ ..x),
  treatment.effect = "ATT",
  data = df_nonexp,
  method = "RA"
)
print(ra_att)

### Propensity-score matching --------------------------------------------------
match_propensity <- matchit(
  D ~ age +
    education +
    nodegree +
    married +
    black +
    hispanic +
    income_75 +
    unemployed_75,
  data = df_nonexp,
  method = "nearest",
  distance = "glm",
  estimand = "ATT",
  ratio = 1,
  replace = TRUE
)

df_ps_matched <- match_data(match_propensity)
feols(
  Y ~ i(D),
  data = df_ps_matched,
  weights = ~weights,
  vcov = "HC1"
)

## With teffects2. This targets the same one-to-one ATT, but teffects2 uses
## Matching::Match, so its tie handling can select a different matched sample.
teffects2::teffect(
  exposure.formula = xpd(D ~ ..x),
  outcome.formula = Y ~ 1,
  treatment.effect = "ATT",
  data = df_nonexp,
  method = "PSMatch",
  M = 1,
  replace = TRUE
)

### Propensity score and inverse-probability weighting -------------------------
propensity_model <- feglm(
  D ~ ..x,
  data = df_nonexp,
  family = "logit"
)
ps_hat <- predict(propensity_model, type = "response")

## ATT weights: treated units receive weight 1 and comparison units receive
## odds weights pi(X) / [1 - pi(X)].
w1_att <- df_nonexp$D
w0_att <- (1 - df_nonexp$D) * ps_hat / (1 - ps_hat)

## By hand
ipw_att <- weighted.mean(df_nonexp$Y, w1_att) -
  weighted.mean(df_nonexp$Y, w0_att)
print(ipw_att)

## teffects2::teffect(method = "IPW") only implements the ATE, so it cannot
## reproduce this ATT specification.

### Double ML ------------------------------------------------------------------
dml_x_cols <- c(
  "age",
  "education",
  "married",
  "nodegree",
  "black",
  "hispanic",
  "income_75",
  "unemployed_75"
)

learner <- lrn(
  "regr.glmnet",
  lambda = sqrt(log(length(dml_x_cols)) / nrow(df_nonexp))
)
ml_l <- learner$clone()
ml_m <- learner$clone()

dml_data <- DoubleMLData$new(
  df_nonexp[, c("Y", "D", dml_x_cols)] |>
    na.omit() |>
    data.table::as.data.table(),
  y_col = "Y",
  d_cols = "D",
  use_other_treat_as_covariate = FALSE,
  x_cols = dml_x_cols
)

# Set the active treatment.
dml_data$set_data_model("D")

set.seed(20260920)
dml_mod <- DoubleMLPLR$new(
  dml_data,
  ml_l = ml_l, ## Conditional Expectation model
  ml_m = ml_m ## Propensity-score model
)
dml_mod$fit()

dml_out <- c(
  estimate = unname(dml_mod$coef["D"]),
  std_error = unname(dml_mod$se["D"]),
  dml_mod$confint()["D", ]
)
print(dml_out)
