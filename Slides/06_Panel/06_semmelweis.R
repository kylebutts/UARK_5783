# %%
library(tidyverse)
library(here)

df <- tibble(
  year = c(
    1833, 1834, 1835, 1836, 1837, 1838, 1839, 1840, 1841, 1842, 1843, 1844, 1845, 1846, 1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855, 1856, 1857, 1858,
    1833, 1834, 1835, 1836, 1837, 1838, 1839, 1840, 1841, 1842, 1843, 1844, 1845, 1846, 1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855, 1856, 1857, 1858
  ),
  clinic = c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
  ),
  births = c(
    3737, 2657, 2573, 2677, 2765, 2987, 2781, 2889, 3036, 3287, 3060, 3157, 3492, 4010, 3490, 3556, 3858, 3745, 4194, 4471, 4221, 4393, 3659, 3925, 4220, 4203,
    33, 1744, 1682, 1670, 1784, 1779, 2010, 2073, 2442, 2659, 2739, 2956, 3241, 3754, 3306, 3319, 3371, 3261, 3395, 3360, 3480, 3396, 2938, 3070, 3795, 4179
  ),
  deaths = c(
    197, 205, 143, 200, 251, 91, 151, 267, 237, 518, 274, 260, 241, 459, 176, 45, 103, 74, 75, 181, 94, 400, 198, 156, 124, 86,
    8, 150, 84, 131, 124, 88, 91, 55, 86, 202, 164, 68, 66, 105, 32, 43, 87, 54, 121, 192, 67, 210, 174, 125, 83, 60
  ),
  death_rate = c(
    5.3, 7.7, 5.6, 7.5, 9.1, 3.0, 5.4, 9.2, 7.8, 15.8, 9.0, 8.2, 6.9, 11.4, 5.0, 1.3, 2.7, 2.0, 1.8, 4.0, 2.2, 9.1, 5.4, 4.0, 2.9, 2.0,
    2.3, 8.6, 5.0, 7.8, 7.0, 4.9, 4.5, 2.7, 3.5, 7.6, 6.0, 2.3, 2.0, 2.8, 1.0, 1.3, 2.6, 1.7, 3.6, 5.7, 1.9, 6.2, 5.9, 4.1, 2.2, 1.4
  )
)
df <- df |>
  mutate(clinic_label = case_when(
    clinic == 1 ~ "Clinic 1 (Physicians and Midwives)",
    clinic == 2 ~ "Clinic 2 (Midwives only)",
  ))

counterfactual_trends <- df |>
  filter(clinic == 2) |>
  arrange(year) |>
  mutate(
    D1_death_rate_untreated = death_rate - lag(death_rate, n = 1L)
  ) |>
  select(year, D1_death_rate_untreated)

df$death_rate_y0_hat = df$death_rate

for (t in 1847:1858) {
  df$death_rate_y0_hat[df$year == t & df$clinic == 1] <-
    df$death_rate_y0_hat[df$year == (t - 1) & df$clinic == 1] + 
    counterfactual_trends$D1_death_rate_untreated[counterfactual_trends$year == t]
}

# Clinic 2 was midwives only starting in 1842
df <- df |> filter(year >= 1841)

# %%
(p_raw <- ggplot() +
  geom_vline(
    xintercept = 1846.5, linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  annotate(
    geom = "label",
    label = "$\\leftarrow$ Clinic 1 started washing hands in May 1847",
    x = 1846.55, y = 16.5,
    vjust = 1, hjust = 0, size = rel(4), label.size = 0
  ) +
  geom_point(
    aes(x = year, y = death_rate, color = clinic_label),
    data = df, size = 1.5
  ) +
  geom_line(
    aes(x = year, y = death_rate, color = clinic_label),
    data = df, linewidth = 1.6
  ) +
  labs(x = NULL, y = "Maternal Mortality Rate (\\%)", color = NULL) +
  scale_color_manual(
    values = c(kfbmisc::kyle_color("navy", "green"))
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
(p_implied_y0 <- ggplot() +
  geom_vline(
    xintercept = 1846.5, linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  annotate(
    geom = "label",
    label = "$\\leftarrow$ Clinic 1 started washing hands in May 1847",
    x = 1846.55, y = 19.5,
    vjust = 1, hjust = 0, size = rel(4), label.size = 0
  ) +
  geom_point(
    aes(x = year, y = death_rate_y0_hat),
    data = df |> filter(clinic == 1), size = 1.5,
    color = kfbmisc::kyle_color("navy")
  ) +
  geom_point(
    aes(x = year, y = death_rate_y0_hat),
    data = df |> filter(clinic == 2), size = 1.5,
    color = kfbmisc::kyle_color("green")
  ) +
  geom_line(
    aes(x = year, y = death_rate, linetype = "A", color = "A"),
    data = df |> filter(clinic == 2),
    linewidth = 1.6
  ) +
  geom_line(
    aes(x = year, y = death_rate, linetype = "B", color = "B"),
    data = df |> filter(clinic == 1),
    linewidth = 1.6
  ) +
  geom_line(
    aes(x = year, y = death_rate_y0_hat, linetype = "C", color = "C"),
    data = df |> filter(clinic == 1 & year >= 1846),
    linewidth = 1.6
  ) +
  labs(
    x = NULL, y = "Maternal Mortality Rate (\\%)", 
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color("green", "navy", "navy"),
    labels = c("Clinic 2 (Midwives only)", "Clinic 1 -- Observed $y$", "Clinic 1 -- Implied Post-treatment $y(0)$")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    labels = c("Clinic 2 (Midwives only)", "Clinic 1 -- Observed $y$", "Clinic 1 -- Implied Post-treatment $y(0)$")
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
  here("Slides/06_Panel/figures/semmelweis_raw_plots.pdf"),
  plot = p_raw, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/semmelweis_implied_y0.pdf"),
  plot = p_implied_y0, width = 8, height = 4.2
)
