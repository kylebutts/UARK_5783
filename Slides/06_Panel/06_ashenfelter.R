# %% 
library(tidyverse)
library(here)

# 1978 Ashenfelter's dip Table 1
df <- tibble(
  year = c(1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969),
  earnings_trainees = c(1443, 1533, 1572, 1843, 1810, 1551, 2923, 3750, 3964, 4401, 4717),
  earnings_comparison = c(2588, 2699, 2782, 2963, 3108, 3275, 3458, 4351, 4430, 4955, 5033)
)

df <- df |>
  pivot_longer(
    cols = c(earnings_trainees, earnings_comparison),
    names_to = "group",
    names_prefix = "earnings_",
    values_to = "earnings"
  ) |>
  mutate(
    group = case_when(
      group == "trainees" ~ "Trainees",
      group == "comparison" ~ "Comparison Group"
    )
  )

counterfactual_trends <- df |>
  filter(group == "Comparison Group") |>
  arrange(year) |>
  mutate(
    D1_earnings_untreated = earnings - lag(earnings, n = 1L)
  ) |>
  select(year, D1_earnings_untreated)

df$earnings_y0_hat <- df$earnings
for (t in 1964:1969) {
  df$earnings_y0_hat[df$year == t & df$group == "Trainees"] <-
    df$earnings_y0_hat[df$year == (t - 1) & df$group == "Trainees"] +
    counterfactual_trends$D1_earnings_untreated[counterfactual_trends$year == t]
}

# %%
(plot_raw <- ggplot() +
  geom_vline(
    xintercept = 1963, linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  geom_vline(
    xintercept = 1964.25, linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  annotate(
    geom = "label",
    label = "$\\leftarrow$ Started Training",
    x = 1964.3, y = 5750,
    vjust = 1, hjust = 0, size = rel(4), label.size = 0
  ) +
  geom_point(
    aes(x = year, y = earnings, color = group),
    data = df, size = 2
  ) +
  geom_line(
    aes(x = year, y = earnings, color = group, group = group),
    data = df, linewidth = 1.2
  ) +
  labs(
    x = NULL, y = "Mean Earnings",
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color(c("purple", "blue"))
  ) +
  scale_y_continuous(limits = c(0, 6000), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1958, 1969, by = 1)) +
  kfbmisc::theme_kyle(base_size = 12, grid_minor = "h") +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  ))

# %% 
(plot_implied_y0 <- ggplot() +
  geom_vline(
    xintercept = 1963, linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  geom_vline(
    xintercept = 1964.25, linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  annotate(
    geom = "label",
    label = "$\\leftarrow$ Started Training",
    x = 1964.3, y = 5750,
    vjust = 1, hjust = 0, size = rel(4), label.size = 0
  ) +
  geom_point(
    aes(x = year, y = earnings_y0_hat),
    data = df |> filter(group == "Trainees"), size = 1.5,
    color = kfbmisc::kyle_color("blue")
  ) +
  geom_point(
    aes(x = year, y = earnings_y0_hat),
    data = df |> filter(group == "Comparison Group"), size = 1.5,
    color = kfbmisc::kyle_color("purple")
  ) +
  geom_line(
    aes(x = year, y = earnings, linetype = "A", color = "A"),
    data = df |> filter(group == "Comparison Group"),
    linewidth = 1.6
  ) +
  geom_line(
    aes(x = year, y = earnings, linetype = "B", color = "B"),
    data = df |> filter(group == "Trainees"),
    linewidth = 1.6
  ) +
  geom_line(
    aes(x = year, y = earnings_y0_hat, linetype = "C", color = "C"),
    data = df |> filter(group == "Trainees" & year >= 1963),
    linewidth = 1.6
  ) +
  labs(
    x = NULL, y = "Mean Earnings",
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color("purple", "blue", "blue"),
    labels = c("Comparison Group", "Trainees -- Observed $y$", "Traines -- No Dip Implied $y(0)$")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    labels = c("Comparison Group", "Trainees -- Observed $y$", "Traines -- No Dip Implied $y(0)$")
  ) +
  scale_y_continuous(limits = c(0, 6000), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1958, 1969, by = 1)) +
  kfbmisc::theme_kyle(base_size = 12, grid_minor = "h") +
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
  here("Slides/06_Panel/figures/ashenfelters_dip.pdf"),
  plot = plot_raw, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/ashenfelters_dip_implied_y0.pdf"),
  plot = plot_implied_y0, width = 8, height = 4.2
)
