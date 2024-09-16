# %%
library(tidyverse) # data-wrangling
library(fixest) # all-things regression
library(haven) # load stata data

# %%
#' NSW experimental treatment and experimental control group
lalonde <- haven::read_dta("Labs/01_Potential_Outcomes/data/nsw.dta")

#' Non-experimental control group
cps <- haven::read_dta("Labs/01_Potential_Outcomes/data/cps_controls.dta")

#' Here we are taking the *treated* units from the NSW experiment and
#' appending the Current Population Survey (CPS) control group. The
#' CPS is representative of the population as a whole, while the NSW is a
#' *highly* selected experimental control group
nonexperimental <- bind_rows(
  lalonde |> filter(treat == 1),
  cps
)

# Treatment-effect using the experimental sample
# %%
tau_experimental <- feols(
  re78 ~ 1 + i(treat),
  data = lalonde
)


# Comparing Lalonde control group
# to the experimental treatment group
# %%
etable(feols(
  c(age, education) ~ i(treat),
  data = nonexperimental
))

tau_nonexperimental <- feols(
  re78 ~ i(treat),
  data = nonexperimental
)

etable(tau_experimental, tau_nonexperimental)

# Regression adjustment
# %%
# 1. Estimate a model for Y_i(0) using untreated units
model_y0 <- feols(
  re78 ~ i(married) + i(black) + i(hispanic) +
    age + I(age^2) +
    education + education^2 + I(education >= 12),
  # ^ Y(0)
  data = subset(nonexperimental, treat == 0)
  # ^ fit using the untreated observations
)

# 2. Estimate a model for Y_i(1) using treated units
model_y1 <- feols(
  re78 ~ i(married) + i(black) + i(hispanic) +
    age + I(age^2) +
    education + education^2 + I(education >= 12),
  # ^ Y(1)
  data = subset(nonexperimental, treat == 1)
  # ^ fit using the treated observations
)

# 3. Predict Y_i(1) and Y_i(0) for all units (to estimate the ATE)
nonexperimental$re78_y0_hat <-
  predict(model_y0, newdata = nonexperimental)

nonexperimental$re78_y1_hat <-
  predict(model_y1, newdata = nonexperimental)

# 4. Take mean of Y_i(1) - Y_i(0)
mean(nonexperimental$re78_y1_hat - nonexperimental$re78_y0_hat)

# ATE estimate
nonexperimental |>
  with(mean(re78_y1_hat - re78_y0_hat))

# ATT estimate
nonexperimental |>
  subset(treat == 1) |>
  with(mean(re78_y1_hat - re78_y0_hat))

# ATC estimate
nonexperimental |>
  subset(treat == 0) |>
  with(mean(re78_y1_hat - re78_y0_hat))

# All in one single regression
# %%
nonexperimental$married_dm <- nonexperimental$married - mean(nonexperimental$married)
nonexperimental$black_dm <- nonexperimental$black - mean(nonexperimental$black)
nonexperimental$hispanic_dm <- nonexperimental$hispanic - mean(nonexperimental$hispanic)
nonexperimental$age_dm <- nonexperimental$age - mean(nonexperimental$age)
nonexperimental$age_sq <- nonexperimental$age^2
nonexperimental$age_sq_dm <- nonexperimental$age_sq - mean(nonexperimental$age_sq)
nonexperimental$education_dm <- nonexperimental$education - mean(nonexperimental$education)
nonexperimental$education_sq <- nonexperimental$education^2
nonexperimental$education_sq_dm <- nonexperimental$education_sq - mean(nonexperimental$education_sq)
nonexperimental$hs_degree <- nonexperimental$education >= 12
nonexperimental$hs_degree_dm <- nonexperimental$hs_degree - mean(nonexperimental$hs_degree)

# ~ treat * (1 + married_dm + black_dm + hispanic_dm + age_dm + age_sq_dm + education_dm + education_sq_dm + hs_degree_dm)

tau_ra <- feols(
  re78 ~ 1 + i(treat) +
    married_dm + black_dm + hispanic_dm + age_dm + age_sq_dm + education_dm + education_sq_dm + hs_degree_dm +
    treat * married_dm + treat * black_dm + treat * hispanic_dm + treat * age_dm + treat * age_sq_dm + treat * education_dm + treat * education_sq_dm + treat * hs_degree_dm,
  # ^ Y(1)
  data = nonexperimental,
  vcov = "HC1"
  # ^ fit using the treated observations
)
etable(tau_ra)
