library(tidyverse)
library(fixest)
tva <- read_csv("Assignments/Assignment-6/data/tva.csv")

# Unconditional Parallel Trends ------------------------------------------------
# 2x2 estimates
tva |>
  filter(year == 1940 | year == 1950) |>
  feols(
    ln_manufacturing ~ i(treat) | county_code + year, 
    vcov = "hc1"
  )

tva |>
  filter(year == 1940 | year == 1960) |>
  feols(
    ln_manufacturing ~ i(treat) | county_code + year, 
    vcov = "hc1"
  )

# pre-period estimates ("pre-trends")
tva |>
  filter(year == 1940 | year == 1920) |>
  feols(
    ln_manufacturing ~
      i(tva, i.year, ref = 0, ref2 = 1940) | county_code + year,
    vcov = "hc1"
  )

tva |>
  filter(year == 1940 | year == 1930) |>
  feols(
    ln_manufacturing ~
      i(tva, i.year, ref = 0, ref2 = 1940) | county_code + year,
    vcov = "hc1"
  )

# Event-study version
tva |>
  feols(
    ln_manufacturing ~
      i(tva, i.year, ref = 0, ref2 = 1940) | county_code + year,
    vcov = "hc1"
  )

# Creating `event time` indicators (interacted with post)
tva |>
  mutate(
    event_time = if_else(
      tva == 0,
      -10,
      year - 1950
    )
  ) |>
  feols(
    ln_manufacturing ~ i(event_time, ref = -10) | county_code + year,
    vcov = "hc1"
  )



# Conditional Parallel Trends --------------------------------------------------


