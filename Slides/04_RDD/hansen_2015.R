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
  summarize(.by = bac1, recidivism_rate = mean(recidivism)) |>
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

# kfbmisc::tikzsave(
#   here::here("Slides/04_RDD/figures/hansen_raw_avgs.pdf"),
#   plot_raw_averages, width = 8, height = 4.2
# )

# %% 
feols(
  recidivism ~ 1 + over_limit + bac1_centered + over_limit * bac1_centered,
  data = subset(hansen, bac1 >= 0.08 - 0.04 & bac1 <= 0.08 + 0.04)
)
rdplot(y = hansen$recidivism, x = hansen$bac1, c = 0.08, h = bw)
rdrobust(
  y = hansen$recidivism, x = hansen$bac1, c = 0.08
)
