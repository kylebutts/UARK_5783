# %%
library(tidyverse)
library(fixest)
library(here)


# %%
# DF 1: Parallel trends
df <- expand_grid(
  tibble(
    id = 1:20, D = c(rep(0, 10), rep(1, 10)),
    mu_i = D + rnorm(n = 2, mean = 5, sd = 0.5)
  ),
  tibble(
    t = 1:10, post = as.numeric(t > 7),
    trend = rnorm(n = 10, mean = t / 4, sd = 0.3)
  )
)

df <- df |>
  mutate(
    d_it = D * post,
    rel_year = if_else(
      D == 0, -1, t - 8
    ),
    y0 = mu_i + trend + rnorm(n(), mean = 0, sd = 0.2),
    y = y0 + d_it * 0.5
  )

es <- df |>
  feols(y ~ i(rel_year, ref = -1) | id + t) |>
  broom::tidy() |>
  mutate(
    rel_year = term |>
      str_replace("rel_year::", "") |>
      as.numeric()
  ) |>
  bind_rows(
    tibble(rel_year = -1, estimate = 0, std.error = 0)
  ) |>
  select(rel_year, estimate, std.error) |> 
  arrange(rel_year)

avgs <- df |>
  summarize(.by = c(D, t), y0 = mean(y0), y = mean(y))

(plot_raw_1 <- ggplot() +
  geom_line(
    aes(x = t, y = y, color = "1", linetype = "1"),
    data = avgs |> filter(D == 0),
  ) +
  geom_point(
    aes(x = t, y = y, color = "1"),
    data = avgs |> filter(D == 0),
  ) +
  geom_line(
    aes(x = t, y = y, color = "2", linetype = "2"),
    data = avgs |> filter(D == 1),
  ) +
  geom_point(
    aes(x = t, y = y, color = "2"),
    data = avgs |> filter(D == 1),
  ) +
  geom_line(
    aes(x = t, y = y0, color = "3", linetype = "3"),
    data = avgs |> filter(D == 1),
  ) +
  geom_point(
    aes(x = t, y = y0, color = "3"),
    data = avgs |> filter(D == 1),
  ) +
  labs(
    x = NULL, y = NULL,
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color("green", "navy", "navy"),
    labels = c("Comparison Group", "Treated Group", "Treated $y(0)$")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    labels = c("Comparison Group", "Treated Group", "Treated $y(0)$")
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

(plot_event_study_1 <- ggplot() +
  geom_hline(yintercept = 0, color = kfbmisc::tailwind_color("zinc-700")) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = kfbmisc::tailwind_color("zinc-500")) +
  geom_point(
    aes(x = rel_year, y = estimate, color = rel_year <= -1),
    data = es
  ) + 
  geom_errorbar(
    aes(x = rel_year, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, color = rel_year <= -1),
    data = es, width = 0.1
  ) +
  scale_x_continuous(
    breaks = -7:2
  ) +
  scale_color_manual(
    values = c(
      kfbmisc::tailwind_color("zinc-800"),
      kfbmisc::tailwind_color("zinc-800")
    ),
    labels = c("Pre-trend Estimate", "Post-treatment Estimate"),
    guide = FALSE
  ) + 
  labs(
    x = "Year relative to treatment ($\\ell$)", 
    y = "Event-study Estimate $\\hat{\\tau}^{\\ell}$",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "h") +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  )
)


# %%
# DF 2: Non-parallel
df <- expand_grid(
  tibble(
    id = 1:20, D = c(rep(0, 10), rep(1, 10)),
    mu_i = D + rnorm(n = 2, mean = 5, sd = 0.5)
  ),
  tibble(
    t = 1:10, post = as.numeric(t > 7),
    trend = rnorm(n = 10, mean = t / 4, sd = 0.3)
  )
)

df <- df |>
  mutate(
    trend = trend + (t/2 * D)
  ) |>
  mutate(
    d_it = D * post,
    rel_year = if_else(
      D == 0, -1, t - 8
    ),
    y0 = mu_i + trend + rnorm(n(), mean = 0, sd = 0.2),
    y = y0 + d_it * 0.5
  )

es <- df |>
  feols(y ~ i(rel_year, ref = -1) | id + t) |>
  broom::tidy() |>
  mutate(
    rel_year = term |>
      str_replace("rel_year::", "") |>
      as.numeric()
  ) |>
  bind_rows(
    tibble(rel_year = -1, estimate = 0, std.error = 0)
  ) |>
  select(rel_year, estimate, std.error) |> 
  arrange(rel_year)

avgs <- df |>
  summarize(.by = c(D, t), y0 = mean(y0), y = mean(y))

(plot_raw_2 <- ggplot() +
  geom_line(
    aes(x = t, y = y, color = "1", linetype = "1"),
    data = avgs |> filter(D == 0),
  ) +
  geom_point(
    aes(x = t, y = y, color = "1"),
    data = avgs |> filter(D == 0),
  ) +
  geom_line(
    aes(x = t, y = y, color = "2", linetype = "2"),
    data = avgs |> filter(D == 1),
  ) +
  geom_point(
    aes(x = t, y = y, color = "2"),
    data = avgs |> filter(D == 1),
  ) +
  geom_line(
    aes(x = t, y = y0, color = "3", linetype = "3"),
    data = avgs |> filter(D == 1),
  ) +
  geom_point(
    aes(x = t, y = y0, color = "3"),
    data = avgs |> filter(D == 1),
  ) +
  labs(
    x = NULL, y = NULL,
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color("green", "navy", "navy"),
    labels = c("Comparison Group", "Treated Group", "Treated $y(0)$")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    labels = c("Comparison Group", "Treated Group", "Treated $y(0)$")
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

(plot_event_study_2 <- ggplot() +
  geom_hline(yintercept = 0, color = kfbmisc::tailwind_color("zinc-700")) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = kfbmisc::tailwind_color("zinc-500")) +
  geom_point(
    aes(x = rel_year, y = estimate, color = rel_year <= -1),
    data = es
  ) + 
  geom_errorbar(
    aes(x = rel_year, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, color = rel_year <= -1),
    data = es, width = 0.1
  ) +
  scale_x_continuous(
    breaks = -7:2
  ) +
  scale_color_manual(
    values = c(
      kfbmisc::tailwind_color("zinc-800"),
      kfbmisc::tailwind_color("zinc-800")
    ),
    labels = c("Pre-trend Estimate", "Post-treatment Estimate"),
    guide = FALSE
  ) + 
  labs(
    x = "Year relative to treatment ($\\ell$)", 
    y = "Event-study Estimate $\\hat{\\tau}^{\\ell}$",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "h") +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  )
)


# %%
# DF 2: Non-parallel
df <- expand_grid(
  tibble(
    id = 1:20, D = c(rep(0, 10), rep(1, 10)),
    mu_i = D + rnorm(n = 2, mean = 5, sd = 0.5)
  ),
  tibble(
    t = 1:10, post = as.numeric(t > 7),
    trend = rnorm(n = 10, mean = t / 4, sd = 0.3)
  )
)

df <- df |>
  mutate(
    trend = trend + (-1 * t/2 * D)
  ) |>
  mutate(
    d_it = D * post,
    rel_year = if_else(
      D == 0, -1, t - 8
    ),
    y0 = mu_i + trend + rnorm(n(), mean = 0, sd = 0.2),
    y = y0 + d_it * 0.5
  )

es <- df |>
  feols(y ~ i(rel_year, ref = -1) | id + t) |>
  broom::tidy() |>
  mutate(
    rel_year = term |>
      str_replace("rel_year::", "") |>
      as.numeric()
  ) |>
  bind_rows(
    tibble(rel_year = -1, estimate = 0, std.error = 0)
  ) |>
  select(rel_year, estimate, std.error) |> 
  arrange(rel_year)

avgs <- df |>
  summarize(.by = c(D, t), y0 = mean(y0), y = mean(y))

(plot_raw_3 <- ggplot() +
  geom_line(
    aes(x = t, y = y, color = "1", linetype = "1"),
    data = avgs |> filter(D == 0),
  ) +
  geom_point(
    aes(x = t, y = y, color = "1"),
    data = avgs |> filter(D == 0),
  ) +
  geom_line(
    aes(x = t, y = y, color = "2", linetype = "2"),
    data = avgs |> filter(D == 1),
  ) +
  geom_point(
    aes(x = t, y = y, color = "2"),
    data = avgs |> filter(D == 1),
  ) +
  geom_line(
    aes(x = t, y = y0, color = "3", linetype = "3"),
    data = avgs |> filter(D == 1),
  ) +
  geom_point(
    aes(x = t, y = y0, color = "3"),
    data = avgs |> filter(D == 1),
  ) +
  labs(
    x = NULL, y = NULL,
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color("green", "navy", "navy"),
    labels = c("Comparison Group", "Treated Group", "Treated $y(0)$")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    labels = c("Comparison Group", "Treated Group", "Treated $y(0)$")
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

(plot_event_study_3 <- ggplot() +
  geom_hline(yintercept = 0, color = kfbmisc::tailwind_color("zinc-700")) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = kfbmisc::tailwind_color("zinc-500")) +
  geom_point(
    aes(x = rel_year, y = estimate, color = rel_year <= -1),
    data = es
  ) + 
  geom_errorbar(
    aes(x = rel_year, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, color = rel_year <= -1),
    data = es, width = 0.1
  ) +
  scale_x_continuous(
    breaks = -7:2
  ) +
  scale_color_manual(
    values = c(
      kfbmisc::tailwind_color("zinc-800"),
      kfbmisc::tailwind_color("zinc-800")
    ),
    labels = c("Pre-trend Estimate", "Post-treatment Estimate"),
    guide = FALSE
  ) + 
  labs(
    x = "Year relative to treatment ($\\ell$)", 
    y = "Event-study Estimate $\\hat{\\tau}^{\\ell}$",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "h") +
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
  here("Slides/06_Panel/figures/event_study_ex_1_raw.pdf"),
  plot = plot_raw_1, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/event_study_ex_1_estimates.pdf"),
  plot = plot_event_study_1, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/event_study_ex_2_raw.pdf"),
  plot = plot_raw_2, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/event_study_ex_2_estimates.pdf"),
  plot = plot_event_study_2, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/event_study_ex_3_raw.pdf"),
  plot = plot_raw_3, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/event_study_ex_3_estimates.pdf"),
  plot = plot_event_study_3, width = 8, height = 4.2
)


