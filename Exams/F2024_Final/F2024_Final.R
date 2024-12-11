# %% 
library(tidyverse)
library(fixest)
library(here)
library(did2s)

data("df_het", package = "did2s")
data = df_het |> 
  filter(g == 0 | g == 2010, year >= 2005 & year <= 2015) |>
  mutate(y = dep_var + (g == 2010) * 0.07 * (year - 2010))

est <- feols(
  y ~ i(rel_year, ref = -1) | unit + year, 
  data = data, cluster = "state"
) |> broom::tidy()

est <- est |> 
  mutate(
    rel_year = as.numeric(str_extract(est$term, "rel_year::(.*)", group = 1)),
    .before = term
  ) |>
  bind_rows(tibble(rel_year = -1, estimate = 0)) |>
  arrange(rel_year)

(plot_bad_pretrends <- ggplot() + 
  geom_point(aes(x = rel_year, y = estimate), data = est, size = 1.5) +
  geom_errorbar(
    aes(x = rel_year, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
    data = est, linewidth = 1.2, width = 0.2
  ) + 
  scale_x_continuous(
    breaks = function(lims) seq(floor(lims[1]), ceiling(lims[2]), 1)
  ) +
  labs(
    x = "Event-time", y = "Estimate $\\hat{\\tau}^{\\ell}$"
  ) + 
  kfbmisc::theme_kyle(base_size = 16, grid_minor = ""))

kfbmisc::tikzsave(
  here("Exams/F2024_Final/figures/did_bad_pretrends.pdf"), 
  plot_bad_pretrends, width = 7, height = 4
)

