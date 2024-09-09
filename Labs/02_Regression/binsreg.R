# %%
library(tidyverse)
library(fixest)
library(binsreg)
cps <- haven::read_dta("Labs/02_Regression/data/cps_controls.dta")

# %%
# Control variables
W <- cps[, c("education", "black", "hispanic", "nodegree")]

# Need masspoints = "off" because age is discrete; we are turning off this
# and treating x as continuous
binsreg(
  y = cps$re75, x = cps$age, w = W,
  masspoints = "off",
  # c(p, s)
  # p = polynomial order
  # s = smoothness constraints
  line = c(0, 0)
)

# Second-order polynomial, no smoothness
binsreg(
  y = cps$re75, x = cps$age, w = W,
  masspoints = "off",
  line = c(2, 0)
)

# Second-order polynomial, 2 degrees of smoothness
binsreg(
  y = cps$re75, x = cps$age, w = W,
  masspoints = "off",
  line = c(2, 2)
)
