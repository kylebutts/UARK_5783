# %% 
library(tidyverse)
library(fixest)
library(MatchIt)
library(WeightIt)
library(DRDID)
library(here)

# Almond, Chay and Lee (2005, QJE). The costs of low birth weight
df <- haven::read_dta(here("Labs/03_Selection_on_Observables/data/cattaneo2.dta"))

feols(bweight ~ i(mbsmoke), data = df, vcov = "HC1")

# y = bweight
# d = mbsmoke (mom smokes any)
# X = ...
setFixest_fml(
  ..X =  ~ mage + I(mage^2) + medu + I(medu^2) + mhisp + foreign
)

# Estimate models for \mu_d(x) and \pi(x) --------------------------------------
# %% 
y <- df$bweight
D <- df$mbsmoke

model_y0 <- feols(
  bweight ~ ..X, 
  data = subset(df, mbsmoke == 0)
)
y0_hat <- predict(model_y0, newdata = df)

model_y1 <- feols(
  bweight ~ ..X, 
  data = subset(df, mbsmoke == 1)
)
y1_hat <- predict(model_y1, newdata = df)

model_pi <- feglm(
  mbsmoke ~ ..X,
  data = df, family = "logit"
)
pi_hat <- predict(model_pi, newdata = df)

# Estimation by hand -----------------------------------------------------------
# %% 
# Regression Adjustment
mean(y1_hat) - mean(y0_hat)

# IPW Estimator (Horvitz-Thompson)
w_1 <- D / pi_hat
w_0 <- (1 - D) / (1 - pi_hat)
mean(w_1 * y) - mean(w_0 * y)

# IPW Estimator (Hájek)
w_1_norm <- w_1 / mean(w_1)
w_0_norm <- w_0 / mean(w_0)
mean((w_1 / mean(w_1)) * y) - mean((w_0 / mean(w_0)) * y)

# AIPW Estimator
(mean(y1_hat) - mean(y0_hat)) + 
  (mean((w_1 * (y - y1_hat))[D == 1]) - mean((w_0 * (y - y0_hat))[D == 0]))


# Using packages ---------------------------------------------------------------
# %% 
# Matching with MatchIt
# Read the docs on this one; there are a ton of options: ?MatchIt::matchit
matchit_obj <- MatchIt::matchit(
  xpd(mbsmoke ~ ..X), 
  data = df, method = "nearest"
)
matched_df <- match.data(
  matchit_obj, data = df,
  distance = "prop.score"
)

# Difference-in-means on matched dataset
feols(
  bweight ~ i(mbsmoke),
  data = matched_df, vcov = "HC1"
)

# Post-matching adjustment as recommended by Abadie and Imbens
feols(
  bweight ~ i(mbsmoke) + ..X,
  data = matched_df, vcov = "HC1"
)

# %% 
# IPTW with WeightIt
weightit_obj <- WeightIt::weightit(
  xpd(mbsmoke ~ ..X), 
  data = df, method = "glm", estimand = "ATE"
)
summary(WeightIt::lm_weightit(
  bweight ~ mbsmoke, data = df, weightit = weightit_obj
))

# %% 
# The easiest way to do doubly-robust with valid standard errors is a hack:
# Use DRDID package which is designed for difference-in-differences estimation
# but pass 0 to y0
Xmat = model.matrix(xpd(~ ..X), data = df)
DRDID::drdid_panel(
  y1 = y, y0 = 0, D = D, covariates = Xmat
)

