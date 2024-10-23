# %% 
library(tidyverse)
library(fixest)
library(here)
library(fwlplot)
library(wooldridge)

data(fish, package = "wooldridge")
# df <- haven::read_dta(here("Slides/05_IV/data/Fulton.dta"))

# %% 
(p_fish_raw <- ggplot() + 
  geom_point(
    aes(x = totqty, y = avgprc), 
    data = fish, color = "grey20"
  ) + 
  geom_smooth(
    aes(x = totqty, y = avgprc), 
    data = fish, 
    formula = y ~ x, method = "lm", se = FALSE,
    color = "#B3114B", linetype = "dashed"
  ) +
  labs(
    x = "Quantity sold", y = "Average price"
  ) +
  kfbmisc::theme_kyle(
    base_size = 12
  )
)

# use Stormy as an instrument
# first-stage
(p_fish_first_stage <- ggplot() + 
  geom_point(
    aes(x = wave2, y = totqty), 
    data = fish, color = "grey20"
  ) + 
  geom_smooth(
    aes(x = wave2, y = totqty), 
    data = fish, 
    formula = y ~ x, method = "lm", se = FALSE,
    color = "#B3114B", linetype = "dashed"
  ) +
  labs(
    x = "Wave Height (2-days prior)", y = "Average price"
  ) +
  kfbmisc::theme_kyle(
    base_size = 12
  )
)

# Reduced form
(p_fish_reducted_form <- ggplot() + 
  geom_point(
    aes(x = wave2, y = avgprc), 
    data = fish, color = "grey20"
  ) + 
  geom_smooth(
    aes(x = wave2, y = avgprc), 
    data = fish, 
    formula = y ~ x, method = "lm", se = FALSE,
    color = "#B3114B", linetype = "dashed"
  ) +
  labs(
    x = "Wave Height (2-days prior)", y = "Average price"
  ) +
  kfbmisc::theme_kyle(
    base_size = 12
  )
)

# 2SLS
fish$totqty_hat <- predict(feols(totqty ~ wave2, data = fish))
fish$avgprc_hat <- predict(feols(avgprc ~ wave2, data = fish))

# Reduced form
(p_fish_2sls <- ggplot() + 
  geom_point(
    aes(x = totqty_hat, y = avgprc_hat), 
    data = fish, color = "grey20"
  ) + 
  geom_smooth(
    aes(x = totqty_hat, y = avgprc_hat), 
    data = fish, 
    formula = y ~ x, method = "lm", se = FALSE,
    color = "#B3114B", linetype = "dashed"
  ) +
  labs(
    x = "Wave Height (2-days prior)", y = "Average price"
  ) +
  kfbmisc::theme_kyle(
    base_size = 12
  )
)


feols(
  avgprc ~ 0 | totqty ~ wave2,
  data = fish,
  vcov = "hc1"
)
