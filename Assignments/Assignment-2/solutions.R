library(tidyverse)
library(fixest)
library(here)
library(arrow)
library(binsreg)

parcels <- read_parquet(here("Slides/02_Regression/data/MA_parcels_sample.parquet"))

est_cef <- feols(total_value ~ 0 + i(n_rooms), data = parcels)
etable(est_cef)

etable(feols(total_value ~ 1 + i(n_rooms), data = parcels))
# est_cef_with_intercept <- feols(total_value ~ 1 + i(n_rooms), data = parcels)
# etable(est_cef, est_cef_with_intercept)

est_lm <- feols(total_value ~ n_rooms, data = parcels)
etable(est_lm)

binsreg(
  y = parcels$total_value,
  x = parcels$lot_size_acres,
  w = parcels[, "n_rooms"],
  line = c(0, 0)
  #        ^ = p.  ^ = s
  # p is the polynomial order
  # s is the "smoothness constraints"
)
binsreg(
  y = parcels$total_value,
  x = parcels$lot_size_acres,
  w = parcels[, "n_rooms"],
  line = c(2, 2)
  # when s = p, this is called a "B-spline"
  # line = c(2, 0)
)
