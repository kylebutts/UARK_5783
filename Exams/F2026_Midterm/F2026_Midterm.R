# %%
library(tidyverse)
library(fixest)
library(MatchIt)
library(WeightIt)
library(teffects)
library(here)

# College balance table ----
# %%
# Match the complete-case sample used in the exam's pre-matching table.
college_data <- wooldridge::card |>
  drop_na(educ, wage, motheduc, fatheduc, momdad14, south66, smsa66) |>
  filter(educ == 12 | educ >= 16) |>
  mutate(college = educ >= 16)

college_balance <- college_data |>
  summarize(
    .by = college,
    across(c(motheduc, fatheduc, momdad14, south66), mean),
    n = n()
  ) |>
  pivot_longer(-college, names_to = "variable", values_to = "value") |>
  mutate(group = if_else(college, "at_least_16", "fewer_than_16")) |>
  select(-college) |>
  pivot_wider(names_from = group, values_from = value) |>
  mutate(
    difference = if_else(
      variable == "n",
      NA_real_,
      at_least_16 - fewer_than_16
    ),
    variable = factor(
      variable,
      levels = c("motheduc", "fatheduc", "momdad14", "south66", "n")
    )
  ) |>
  arrange(variable) |>
  mutate(
    variable = recode(
      as.character(variable),
      motheduc = "Mother's years of schooling",
      fatheduc = "Father's years of schooling",
      momdad14 = "Lived with both parents at age 14",
      south66 = "Lived in the South in 1966",
      n = "Number of observations"
    )
  ) |>
  select(variable, fewer_than_16, at_least_16, difference)

college_balance_table <- college_balance |>
  set_names(c("", "High School", "College", "Difference")) |>
  tinytable::tt() |>
  tinytable::format_tt(i = 1:2, j = 2:4, digits = 2, num_fmt = "decimal") |>
  tinytable::format_tt(i = 3:4, j = 2:4, digits = 3, num_fmt = "decimal") |>
  tinytable::format_tt(
    i = 5,
    j = 2:3,
    digits = 0,
    num_fmt = "decimal",
    num_mark_big = ","
  ) |>
  tinytable::format_tt(i = 5, j = 4, replace = "")

college_balance_table


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%
library(tidyverse)
library(here)

fs::dir_create(here("Slides/04_RDD/figures"))

tikzDevice::setTikzDefaults()
default_packages <- getOption("tikzLatexPackages")
packages <- c(
  system.file("tikzsave/paper.sty", package = "kfbmisc"),
  system.file("tikzsave/math.sty", package = "kfbmisc")
)
pkg_tex <- sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
options(
  "tikzLatexPackages" = c(
    default_packages,
    "\\usepackage{bm}\n",
    pkg_tex
  )
)

# %%
# Simulate an SAT-based admissions rule with a known cutoff. Students from
# higher-income families are more likely to retake after narrowly missing it.
set.seed(20260922)

n_students <- 15000
sat_cutoff <- 1350
plot_window <- 180
bin_width <- 20
top_college_effect <- 12 # Thousands of dollars

sat_data <- tibble(
  student_id = seq_len(n_students),
  academic_ability = rnorm(n_students),
  log_family_income = log(80) +
    0.20 * academic_ability +
    rnorm(n_students, sd = 0.45),
  family_income = exp(log_family_income), # Thousands of dollars
  income_z = as.numeric(scale(log_family_income)),
  first_sat = round(
    1100 + 105 * academic_ability + 28 * income_z + rnorm(n_students, sd = 70)
  )
) |>
  mutate(
    near_cutoff = first_sat >= sat_cutoff - 100 & first_sat < sat_cutoff,

    ## Higher-income students are more likely to retake after narrowly missing.
    retake_probability = if_else(
      near_cutoff,
      plogis(-5 + 4 * income_z + 0.018 * (first_sat - sat_cutoff + 100)),
      0
    ),
    retook_sat = rbinom(n(), size = 1, prob = retake_probability) == 1,

    ## Retakers improve with 90% probability; successful gains are positive
    retake_gain = if_else(
      retook_sat & rbinom(n(), size = 1, prob = 0.90) == 1,
      pmax(1, round(rnorm(n(), mean = 65, sd = 30))),
      0
    ),
    final_sat = pmax(first_sat, first_sat + retake_gain),

    ##
    sat_centered = final_sat - sat_cutoff,
    top_college = final_sat >= sat_cutoff,
    future_earnings = 52 +
      6 * academic_ability +
      0.11 * family_income +
      top_college_effect * top_college +
      rnorm(n(), sd = 12)
  ) |>
  filter(abs(sat_centered) <= plot_window)

# Bin means make the discontinuities visible without plotting thousands of
# individual observations. The regression lines are estimated separately on
# each side of the cutoff, as in a standard RD plot.
sat_bins <- sat_data |>
  mutate(
    sat_bin = bin_width * floor(sat_centered / bin_width) + bin_width / 2,
    above_cutoff = sat_centered >= 0
  ) |>
  summarize(
    .by = c(sat_bin, above_cutoff),
    future_earnings = mean(future_earnings),
    family_income = mean(family_income),
    n = n()
  )

side_colors <- c("Below cutoff" = "#B3114B", "Above cutoff" = "#2DB25F")

base_rd_plot <- ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1,
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  scale_x_continuous(
    breaks = c(-150, -100, -50, 0, 50, 100, 150),
    labels = sat_cutoff + c(-150, -100, -50, 0, 50, 100, 150)
  ) +
  scale_color_manual(values = side_colors, guide = guide_none()) +
  labs(x = "Final SAT score") +
  kfbmisc::theme_kyle(base_size = 18, grid_minor = "")

# %%
(plot_earnings <- base_rd_plot +
  geom_point(
    aes(
      x = sat_bin,
      y = future_earnings,
      color = if_else(above_cutoff, "Above cutoff", "Below cutoff")
    ),
    data = sat_bins,
    size = 2.8
  ) +
  geom_smooth(
    aes(
      x = sat_centered,
      y = future_earnings,
      color = if_else(top_college, "Above cutoff", "Below cutoff"),
      group = top_college
    ),
    data = sat_data,
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1.2
  ) +
  labs(
    y = "Average future earnings (\\$1,000s)"
  ))

(plot_family_income <- base_rd_plot +
  geom_point(
    aes(
      x = sat_bin,
      y = family_income,
      color = if_else(above_cutoff, "Above cutoff", "Below cutoff")
    ),
    data = sat_bins,
    size = 2.8
  ) +
  geom_smooth(
    aes(
      x = sat_centered,
      y = family_income,
      color = if_else(top_college, "Above cutoff", "Below cutoff"),
      group = top_college
    ),
    data = sat_data,
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1.2
  ) +
  labs(
    y = "Average family income (\\$1,000s)"
  ))

rdrobust::rdplot(
  y = sat_data$family_income,
  x = sat_data$final_sat,
  c = sat_cutoff
)

summary(rdrobust::rdrobust(
  y = sat_data$family_income,
  x = sat_data$final_sat,
  c = sat_cutoff
))

rddensity::rdplotdensity(
  rddensity::rddensity(
    X = sat_data$first_sat,
    c = sat_cutoff
  ),
  X = sat_data$first_sat,
)

rddensity::rdplotdensity(
  rddensity::rddensity(
    X = sat_data$final_sat,
    c = sat_cutoff
  ),
  X = sat_data$final_sat,
)


# %%
kfbmisc::tikzsave(
  here("Exams/F2026_Midterm/sat_sorting_earnings.pdf"),
  plot_earnings,
  width = 8,
  height = 4.2
)

kfbmisc::tikzsave(
  here("Exams/F2026_Midterm/sat_sorting_family_income.pdf"),
  plot_family_income,
  width = 8,
  height = 4.2
)
