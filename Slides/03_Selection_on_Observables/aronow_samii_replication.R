# %%
library(tidyverse)
library(patchwork)
library(fixest)
library(sf)
library(haven)
library(here)
library(rmapshaper)

# %%
jensen.cc <- haven::read_dta(
  here("Slides/03_Selection_on_Observables/data/jensen-rep.dta")
)
setFixest_fml(
  ..x = ~ var5 + market + lgdppc + gdpgrowt + tradeofg + overallb + generalg + country + d2 + d3
)
fit.y <- feols(Fvar5 ~ regime + ..x, data = jensen.cc, vcov = "hc1")
fit.d <- feols(regime ~ ..x, data = jensen.cc)
d.tilde <- as.numeric(residuals(fit.d))
jensen.cc$w <- d.tilde^2
w1 <- tapply(jensen.cc$w, jensen.cc$country, mean)

# %%
mapnames <- read_csv(
  here("Slides/03_Selection_on_Observables/data/mapnames_filled.csv"),
  show_col_types = FALSE
)
mapnames <- mapnames |>
  left_join(
    w1 |> enframe(name = "jensen", value = "weight"),
    by = "jensen"
  ) |>
  mutate(
    incl = !is.na(weight),
    norm_weights = abs(weight) / max(abs(weight), na.rm = TRUE),
    norm_weights_v2 = weight / sum(weight, na.rm = TRUE)
  )

world <- sf::read_sf(
  here("Slides/03_Selection_on_Observables/data/world_countries_boundary_file_world_2002.shp")
)
world <- rmapshaper::ms_simplify(world, keep = 0.25, keep_shapes = TRUE)
world <- world |>
  tidylog::left_join(mapnames, by = join_by(NAME == mapname)) |>
  filter(NAME != "Antarctica")

# %%
# Nominal Sample
(map_ate_weights <- ggplot(world) +
  geom_sf(
    aes(fill = as.numeric(incl)),
    color = "grey", linewidth = 0.5, na.rm = TRUE
  ) +
  scale_fill_gradient(
    low = "white", high = "#404040",
    guide = guide_colorsteps(
      title = "ATE weights on $\\tau_c$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  kfbmisc::theme_map() +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(2, "cm")
  )
)

# %%
# Effective sample
(map_effective_weights <- ggplot(world) +
  geom_sf(
    aes(fill = norm_weights_v2),
    color = "grey", linewidth = 0.5, na.rm = TRUE
  ) +
  scale_fill_gradient(
    low = "white", high = "#404040",
    guide = guide_colorsteps(
      title = "Effective weights on $\\tau_c$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  kfbmisc::theme_map() +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(2, "cm")
  )
)

# %%
kfbmisc::tikzsave(
  here("Slides/03_Selection_on_Observables/figures/jensen_ate_weights.pdf"),
  map_ate_weights,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("Slides/03_Selection_on_Observables/figures/jensen_regression_weights.pdf"),
  map_effective_weights,
  width = 8, height = 4
)
