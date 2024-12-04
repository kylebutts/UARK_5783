library(tidyverse)
library(fixest)

tva <- read_csv("data/tva.csv")
# tva = D_i
# post = 1(t >= 1950)
# treat = d_it

tva_short <- tva |>
  filter(year == 1940 | year == 1960)

# Difference-in-differences by hand
ybar_treat_post <- tva |>
  filter(year == 1960 & tva == 1) |> 
  pull(ln_manufacturing) |> mean()

ybar_treat_pre <- tva |>
  filter(year == 1940 & tva == 1) |>
  pull(ln_manufacturing) |> mean()

ybar_control_post <- tva |>
  filter(year == 1960 & tva == 0) |> 
  pull(ln_manufacturing) |> mean()

ybar_control_pre <- tva |>
  filter(year == 1940 & tva == 0) |>
  pull(ln_manufacturing) |> mean()

# 0.2774189
(ybar_treat_post - ybar_treat_pre) -
  (ybar_control_post - ybar_control_pre)


# Regression
feols(
  ln_manufacturing ~ 
    # d_it + \mu_c + \lambda_t
    i(treat, ref = 0) | county_code + year,
  data = tva_short, vcov = "hc1"
)

# Using first-differences
first_diff <- tva_short |>
  mutate(
    .by = county_code, 
    # \Delta y_i = y_{i, 1960} - y_{i, 1940}
    delta_ln_manufacturing = 
      ln_manufacturing[year == 1960] -
      ln_manufacturing[year == 1940]
  ) |>
  filter(year == 1960)

feols(
  delta_ln_manufacturing ~ i(tva, ref = 0), 
  data = first_diff, vcov = "HC1"
)

# This opens up the ability to do selection-on-observables type estimators using conditional parallel trends assumption
m <- MatchIt::matchit(
  tva ~ manufacturing_share_1930 + agriculture_share_1930,
  data = first_diff, distance = "mahalanobis"
)
first_diff_matched <- MatchIt::match.data(m)
feols(
  delta_ln_manufacturing ~ i(tva, ref = 0), 
  data = first_diff_matched, vcov = "HC1"
)


# EVENT STUDY
feols(
  ln_manufacturing ~ 
    i(tva, i.year, ref = 0, ref2 = 1940) | 
    county_code + year, 
  data = tva, vcov = "hc1"
)

tva <- tva |>
  mutate(
    event_time = if_else(
      tva == 0, 
      -10, # for the untreated group
      year - 1950 # for the treated group
    )
  )

feols(
  ln_manufacturing ~ 
    i(tva, i.event_time, ref = 0, ref2 = -10) | 
    county_code + year, 
  data = tva, vcov = "hc1"
)


