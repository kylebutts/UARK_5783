# %%
library(tidyverse)
library(fixest)
library(here)
library(palmerpenguins)

data(penguins, package = "palmerpenguins")

penguins <- penguins |>
  mutate(
    .by = species,
    bill_length_mm_demeaned =
      bill_length_mm - mean(bill_length_mm, na.rm = TRUE),
    flipper_length_mm_demeaned =
      flipper_length_mm - mean(flipper_length_mm, na.rm = TRUE)
  )


(p_raw <- ggplot() +
  geom_point(
    aes(x = flipper_length_mm, y = bill_length_mm, color = species),
    data = penguins
  ) +
  # geom_smooth(
  #   aes(x = flipper_length_mm, y = bill_length_mm),
  #   data = penguins,
  #   method = "lm", formula = y ~ x,
  #   color = kfbmisc::tailwind_color("zinc-800")
  # ) +
  labs(
    x = "Flipper Length (mm)",
    y = "Bill Length (mm)",
    color = "Species"
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color(c("navy", "green", "rose"))
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  )
)

(p_demeaned <- ggplot() +
  geom_point(
    aes(x = flipper_length_mm_demeaned, y = bill_length_mm_demeaned, color = species),
    data = penguins
  ) +
  geom_smooth(
    aes(x = flipper_length_mm_demeaned, y = bill_length_mm_demeaned),
    data = penguins,
    method = "lm", formula = y ~ x,
    color = kfbmisc::tailwind_color("zinc-800")
  ) +
  labs(
    x = "\\emph{Demeaned} Flipper Length (mm)",
    y = "\\emph{Demeaned} Bill Length (mm)",
    color = "Species"
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color(c("navy", "green", "rose"))
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  )
)

# %%
kfbmisc::tikzsave(
  here::here("Slides/06_Panel/figures/penguins_ex_raw.pdf"),
  plot = p_raw, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("Slides/06_Panel/figures/penguins_ex_demeaned.pdf"),
  plot = p_demeaned, width = 8, height = 4.2
)


# %%
df <- haven::read_dta(here("Labs/03_Selection_on_Observables/data/cattaneo2.dta"))

df <- df |>
  filter(mage >= 20) |>
  mutate(
    mage_bins = cut(mage, breaks = seq(20, 45, by = 5), include.lowest = TRUE)
  )

df <- df |>
  mutate(
    .by = mage_bins,
    mbsmoke_demeaned =
      mbsmoke - mean(mbsmoke, na.rm = TRUE),
    bweight_demeaned =
      bweight - mean(bweight, na.rm = TRUE)
  )

# %%
(p_raw <- ggplot() +
  geom_point(
    aes(x = mbsmoke, y = bweight, color = mage_bins),
    data = df
  ) +
  labs(
    x = "Whether Mother Smokes",
    y = "Baby's Birthweight",
    color = "Mother's Age Bins"
  ) +
  scale_color_manual(
    values = unname(kfbmisc::kyle_colors)
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  )
)

(p_demeaned <- ggplot() +
  geom_point(
    aes(x = mbsmoke_demeaned, y = bweight_demeaned, color = mage_bins),
    data = df
  ) +
  geom_smooth(
    aes(x = mbsmoke_demeaned, y = bweight_demeaned),
    data = df,
    method = "lm", formula = y ~ x,
    color = kfbmisc::tailwind_color("zinc-800")
  ) +
  labs(
    x = "(Demeaned) Whether Mother Smokes",
    y = "(Demeaned) Baby's Birthweight",
    color = "Mother's Age Bins"
  ) +
  scale_color_manual(
    values = unname(kfbmisc::kyle_colors)
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  )
)

# %%
kfbmisc::tikzsave(
  here::here("Slides/06_Panel/figures/cattaneo_ex_raw.pdf"),
  plot = p_raw, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("Slides/06_Panel/figures/cattaneo_ex_demeaned.pdf"),
  plot = p_demeaned, width = 8, height = 4.2
)
