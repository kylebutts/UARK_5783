library(tidyverse)
library(fixest)
library(here)
df <- haven::read_dta(here("Labs/02_Regression/data/nsw.dta"))

# %%
est_ols <- feols(
  re78 ~ i(treat, ref = 0) + age + I(age^2) + education + i(black) + i(hispanic),
  data = df, vcov = "hc1"
)
coef(est_ols)["treat::1"]

# %%
est_y <- feols(
  re78 ~ age + I(age^2) + education + i(black) + i(hispanic),
  data = df
)
df$re78_residual <- resid(est_y)

est_d <- feols(
  treat ~ age + I(age^2) + education + i(black) + i(hispanic),
  data = df
)
df$treat_residual <- resid(est_d)

est_fwl <- feols(
  re78_residual ~ treat_residual,
  data = df, vcov = "hc1"
)
coef(est_fwl)["treat_residual"]

etable(
  est_ols, est_fwl
)

# %%
# $V_{\text{OLS}} = (n-K) / n * \hat{V}$
# $V_{\text{FWL}} = (n-2) / n * \hat{V}$
# So to get correct standard errors, need to multiply by sqrt((n-K) / (n-2))
n <- nrow(df)
K <- length(coef(est_ols))

se(est_ols)["treat::1"]
sqrt((n - 2) / (n - K)) * se(est_fwl)["treat_residual"]
