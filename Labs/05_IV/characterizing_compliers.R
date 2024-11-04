library(tidyverse)
library(fixest)
library(here)
# http://fmwww.bc.edu/repec/bocode/j/jtpa.dta
# Z = assignmt
# D = training 
# y = earnings
# X = age
jtpa <- haven::read_dta(here::here("Labs/05_IV/data/jtpa.dta"))

# 2sls Estimate of jobs training on earnings -----------------------------------
feols(
  earnings ~ 1 | training ~ assignmt,
  data = jtpa, vcov = "hc1"
)

# Characterizing compliers -----------------------------------------------------
# Mean age
Xbar <- mean(jtpa$age)

# Mean age for always-takers
Xbar_at <- jtpa |>
  filter(training == 1 & assignmt == 0) |>
  with(mean(age))

# Mean age for never-takers
Xbar_nt <- jtpa |>
  filter(training == 0 & assignmt == 1) |>
  with(mean(age))

# Mean of age for complier
est_complier <- feols(
  I(age * training) ~ 1 | training ~ assignmt,
  data = jtpa, vcov = "hc1"
) 
Xbar_complier <- coef(est_complier)["fit_training"]

# Similar alternative from 
# Marbach and Hangartner (2020, Political Analysis)
# "Profiling Compliers and Non-compliers for Instrumental-Variable Analysis"
library(ivdesc)
with(jtpa, ivdesc(X = age, D = training, Z = assignmt, boot = FALSE))
