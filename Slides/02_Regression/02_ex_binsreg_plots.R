# %%
library(fixest)
library(tidyverse)
library(arrow)
library(binsreg)
library(here)

colors <- c(
  "red_pink" = "#e64173",
  "turquoise" = "#20B2AA",
  "orange" = "#FFA500",
  "red" = "#fb6107",
  "blue" = "#3b3b9a",
  "green" = "#8bb174",
  "purple" = "#6A5ACD",
  "zinc50" = "#fafafa",
  "zinc100" = "#f4f4f5",
  "zinc200" = "#e4e4e7",
  "zinc300" = "#d4d4d8",
  "zinc400" = "#a1a1aa",
  "zinc500" = "#71717a",
  "zinc600" = "#52525b",
  "zinc700" = "#3f3f46",
  "zinc800" = "#27272a",
  "zinc900" = "#18181b",
  "zinc950" = "#09090b"
)

# %%
set.seed(20240904)
parcels <- open_dataset("/Users/kylebutts/Library/CloudStorage/Dropbox/Zoning-and-Housing-Supply/data/base/MA-parcels_panel_geocoded.parquet")

sample <- parcels |>
  filter(between(year_built, 1900, 2023)) |>
  filter(total_value < 2e6) |>
  filter(between(use_code, 101, 110)) |>
  filter(between(n_rooms, 3, 8)) |>
  filter(between(lot_size_acres, 0.05, 4)) |>
  select(latitude, longitude, year_built, total_value, lot_size_acres, n_rooms) |>
  collect() |>
  slice_sample(n = 100000)

write_parquet(sample, here("Slides/02_Regression/data/MA_parcels_sample.parquet"))

# %%
parcels <- read_parquet(here("Slides/02_Regression/data/MA_parcels_sample.parquet"))

# %%
opt_binsreg <- binsregselect(
  y = parcels$total_value,
  x = parcels$year_built
)

# %%
bins <- function(x, p, s, n_bins) {
  knots <- binsreg:::genKnot.qs(x = x, J = n_bins)

  # Calls `splines::splineDesign` internally
  design <- binsreg:::binsreg.spdes(eval = x, p = p, s = s, knot = knots, deriv = 0)
  return(design)
}

create_predict_grid <- function(knots, n_pts = 5) {
  x <- c(knots[1])
  id <- c(1)
  for (j in 2:(length(knots) - 1)) {
    x <- c(x, knots[j] - 0.001, knots[j])
    id <- c(id, j - 1, j)
  }
  x <- c(x, knots[length(knots)])
  id <- c(id, length(knots) - 1)

  grid <- tibble(bin_id = id, x = as.numeric(x))

  grid <- grid |>
    reframe(
      .by = bin_id,
      x = base::seq(from = as.numeric(x[1]), to = as.numeric(x[2]), length.out = n_pts)
    )
  return(grid)
}


knots <- opt_binsreg$knot
Jopt <- length(knots) - 1

# %%
predictions <- create_predict_grid(knots, n_pts = 30) |>
  rename(year_built = x)

est <- feols(
  total_value ~ 0 + bins(year_built, p = 0, s = 0, n_bins = Jopt),
  data = parcels
)
predictions$y_hat <- predict(est, newdata = predictions)

est_p_1_s_0 <- feols(
  total_value ~ 0 + bins(year_built, p = 1, s = 0, n_bins = Jopt),
  data = parcels
)
predictions$y_hat_p_1_s_0 <- predict(est_p_1_s_0, newdata = predictions)

est_p_2_s_2 <- feols(
  total_value ~ 0 + bins(year_built, p = 2, s = 2, n_bins = Jopt),
  data = parcels
)
predictions$y_hat_p_2_s_2 <- predict(est_p_2_s_2, newdata = predictions)

# %%
(p_raw <- ggplot() +
  geom_point(
    aes(x = year_built, y = total_value),
    data = parcels |> slice_sample(n = 5000),
    alpha = 0.1, shape = 20
    # data = parcels,
    # alpha = 0.05, shape = 20
  ) +
  labs(x = "Year Built", y = "Value of Property (\\$100K)") +
  scale_y_continuous(
    labels = scales::label_currency(
      prefix = "\\$", scale_cut = scales::cut_short_scale()
    ),
    expand = expansion(0, c(0, 0.01))
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    axis.title = element_text(size = rel(1))
  ))

# %%
(p_split_into_bins <- p_raw +
  geom_vline(
    xintercept = opt_binsreg$knot,
    color = colors["zinc800"],
    linewidth = 1,
    linetype = "dashed"
  ))

# %%
(p_bins <- p_split_into_bins +
  geom_line(
    aes(x = year_built, y = y_hat, group = bin_id),
    data = predictions,
    color = colors["blue"],
    linewidth = 1.5
  ))

(p_bins_add_linear <- p_bins +
  geom_line(
    aes(x = year_built, y = y_hat_p_1_s_0, group = bin_id),
    data = predictions,
    color = colors["red_pink"],
    linewidth = 1.5
  ))

(p_bins_add_smooth <- p_bins_add_linear +
  geom_line(
    aes(x = year_built, y = y_hat_p_2_s_2, group = bin_id),
    data = predictions,
    color = colors["green"],
    linewidth = 1.5
  ))

# %%
kfbmisc::tikzsave(
  here::here("Slides/02_Regression/figures/ex_binsreg_raw.pdf"),
  plot = p_raw, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("Slides/02_Regression/figures/ex_binsreg_split_into_bins.pdf"),
  plot = p_split_into_bins, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("Slides/02_Regression/figures/ex_binsreg_bins.pdf"),
  plot = p_bins, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("Slides/02_Regression/figures/ex_binsreg_bins_add_linear.pdf"),
  plot = p_bins_add_linear, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("Slides/02_Regression/figures/ex_binsreg_bins_add_smooth.pdf"),
  plot = p_bins_add_smooth, width = 8, height = 4.2
)
