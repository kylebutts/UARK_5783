library(tidyverse)
library(here)
library(fixest)

# Angrist and Evans (1998)
# 
# What is the labor market impact (on mothers) of having more kids (say 2 -> 3)
# 
# samesex - `TRUE` if first-two kids are of the same-sex
# twins_2 - `TRUE` if second successful pregnancy is twins
# mom_worked - `TRUE` if mom worked for pay
# mom_weeks_worked - number of weeks worked in the year by the mother
# morekids - More than 2 children
# kidcount - Number of children,worked for pay is mom worked,
# moreths - Mom has more than hs education
# blackm - `TRUE` if mom is Black
# hispm - `TRUE` if mom is Hispanic
# whitem - - `TRUE` if mom is White
df <- read_csv("data/angrist_evans.csv")


# IV Estimation ----
# Z_i `samesex` as instrument for
# D_i `morekids`
# y_i `mom_weeks_worked`

# First-stage
# D ~ Z
est_first_stage <- feols(
  morekids ~ 1 + samesex,
  data = df, vcov = "hc1"
)
# only an additional 6% of mothers have more than 2 kids from this instrument


# Reduced-form
# y ~ Z
est_reduced_form <- feols(
  mom_weeks_worked ~ 1 + samesex,
  data = df, vcov = "hc1"
)


# Two-stage least squares (manually)
rf <- coef(est_reduced_form)["samesexTRUE"] 
fs <- coef(est_first_stage)["samesexTRUE"]
rf / fs


# Two-stage least squares (using fixest)
est_2sls <- feols(
  mom_weeks_worked ~ 1 | morekids ~ samesex, 
  # y ~ additional controls | D ~ Z,
  data = df, vcov = "hc1"
)
est_2sls

# Is my first stage strong?
fitstat(est_2sls, type = "ivf")



# Characterizing the compliers ----
# Compare `moreths` of population to compliers

# outcome variable: X * D
mean(df$moreths)
# = E(X_i)
# = 0.29

feols(
  I(moreths * morekids) ~ 1 | morekids ~ samesex, 
  data = df, vcov = "hc1"
)
# = E(X_i | Compliers_i = 1)
# = 0.266421

# Compare share of White/Black/Mother of population to compliers
mean(df$blackm)
# = 0.1237793

feols(
  I(blackm * morekids) ~ 1 | morekids ~ samesex, 
  data = df, vcov = "hc1"
)
# = 0.088673

mean(df$hispm)
# = 0.03053128

feols(
  I(hispm * morekids) ~ 1 | morekids ~ samesex, 
  data = df, vcov = "hc1"
)
# = 0.027052

mean(df$whitem)
# = 0.8170163

feols(
  I(whitem * morekids) ~ 1 | morekids ~ samesex, 
  data = df, vcov = "hc1"
)
# = 0.859090 


# Multiple instruments ----
# Do we have a strong first-stage?
# Z_i `samesex` and `twins_2`
# D_i `morekids`
# y_i `mom_weeks_worked`

feols(
  mom_weeks_worked ~ 1 | morekids ~ samesex + twins_2, 
  # y ~ additional controls | D ~ Z,
  data = df, vcov = "hc1"
)





