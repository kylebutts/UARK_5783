# %%
library(tidyverse)
library(fixest)
library(rdrobust)
library(rdlocrand)
hansen <- haven::read_dta(here::here("Slides/04_RDD/data/hansen_dwi.dta"))
hansen <- hansen |>
  mutate(
    over_limit = bac1 >= 0.08,
    bac1_centered = bac1 - 0.08
  )
  
bw <- 0.04
hansen <- subset(hansen, bac1 >= 0.08 - bw * 2 & bac1 <= 0.08 + bw * 2)

# %%
collapsed <- hansen |>
  summarize(.by = bac1, 
    recidivism_rate = mean(recidivism),
    mean_age = mean(aged),
    pct_white = mean(white),
    pct_male = mean(male)
  ) |>
  mutate(
    over_limit = bac1 >= 0.08
  )

(plot_raw_averages <- ggplot() +
  geom_vline(
    xintercept = 0.08,
    linetype = "dashed", color = kfbmisc::tailwind_color("zinc-700")
  ) +
  geom_point(
    aes(x = bac1, y = recidivism_rate, color = over_limit),
    data = collapsed
  ) +
  labs(
    x = "Blood-alcohol Content",
    y = "Rate of drunk-driving recidivism",
    color = "Over-legal limit"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2DB25F", "FALSE" = "#b21010"),
    guide = guide_none()
  ) +
  kfbmisc::theme_kyle(base_size = 12)
)

kfbmisc::tikzsave(
  here::here("Slides/04_RDD/figures/hansen_raw_avgs.pdf"),
  plot_raw_averages, width = 8, height = 4.2
)



# %% 
feols(
  recidivism ~ 1 + over_limit,
  data = subset(hansen, bac1 >= 0.07 & bac1 <= 0.09)
)

# %% 
feols(
  recidivism ~ 1 + over_limit + I(bac1 - 0.08) + I(bac1 - 0.08) * over_limit,
  data = subset(hansen, bac1 >= 0.04 & bac1 <= 0.12)
)

# %% 
p <- rdplot(y = hansen$recidivism, x = hansen$bac1, c = 0.08, h = bw)

kfbmisc::tikzsave(
  here::here("Slides/04_RDD/figures/hansen_rdplot_bw_0pt4.pdf"),
  p$rdplot, width = 8, height = 3.8
)

est <- rdrobust(
  y = hansen$recidivism, x = hansen$bac1, c = 0.08
)
summary(est)


# %% 
# Balance checks
(plot_balance_check_male <- ggplot() +
  geom_vline(
    xintercept = 0.08,
    linetype = "dashed", color = kfbmisc::tailwind_color("zinc-700")
  ) +
  geom_point(
    aes(x = bac1, y = pct_male, color = over_limit),
    data = collapsed
  ) +
  labs(
    x = "Blood-alcohol Content",
    y = "Percent of drivers that are male",
    color = "Over-legal limit"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2DB25F", "FALSE" = "#b21010"),
    guide = guide_none()
  ) +
  kfbmisc::theme_kyle(base_size = 12)
)

(plot_balance_check_white <- ggplot() +
  geom_vline(
    xintercept = 0.08,
    linetype = "dashed", color = kfbmisc::tailwind_color("zinc-700")
  ) +
  geom_point(
    aes(x = bac1, y = pct_white, color = over_limit),
    data = collapsed
  ) +
  labs(
    x = "Blood-alcohol Content",
    y = "Percent of drivers that are White",
    color = "Over-legal limit"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2DB25F", "FALSE" = "#b21010"),
    guide = guide_none()
  ) +
  kfbmisc::theme_kyle(base_size = 12)
)

kfbmisc::tikzsave(
  here::here("Slides/04_RDD/figures/hansen_balance_check_male.pdf"),
  plot_balance_check_male, width = 8, height = 4.2
)

kfbmisc::tikzsave(
  here::here("Slides/04_RDD/figures/hansen_balance_check_white.pdf"),
  plot_balance_check_white, width = 8, height = 4.2
)

# %% 
# Density checks
(hist_bac1 <- ggplot() +
  geom_vline(
    xintercept = 0.08,
    linetype = "dashed", color = kfbmisc::tailwind_color("zinc-700")
  ) +
  geom_histogram(
    aes(x = bac1, fill = over_limit, group = over_limit),
    data = hansen, color = "white",
    # bins = 50, 
    breaks = seq(0, 0.16, by = 0.005)
  ) + 
  scale_fill_manual(
    values = c("TRUE" = "#2DB25F", "FALSE" = "#b21010"),
    guide = guide_none()
  ) +
  labs(x = "Blood-alcohol Content", y = "Count") +
  kfbmisc::theme_kyle(base_size = 14)
)

kfbmisc::tikzsave(
  here::here("Slides/04_RDD/figures/hansen_density_bac1.pdf"),
  density_bac1, width = 8, height = 4.2
)


