# %%
library(tidyverse)
library(here)
library(kfbmisc)

# %%
# Endogeneity problem
(p_endogeneity_problem <- ggplot() +
  geom_function(
    aes(linetype = "Market 1"),
    fun = \(x) x + 2
  ) +
  geom_function(
    aes(linetype = "Market 1"),
    fun = \(x) -1 * x + 8
  ) +
  geom_function(
    aes(linetype = "Market 2"),
    fun = \(x) 0.8 * x + 4
  ) +
  geom_function(
    aes(linetype = "Market 2"),
    fun = \(x) -1.1 * x + 10
  ) +
  annotate("point", x = 3, y = 5) +
  annotate("point", x = 6 / 1.9, y = 0.8 * 6 / 1.9 + 4) +
  # annotate("segment", 
  #   x = 3, y = 5, xend = 6 / 1.9, yend = 0.8 * 6 / 1.9 + 4,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  scale_x_continuous(
    limits = c(0, 7)
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed")
  ) +
  labs(x = "Quantity", y = "Price", linetype = NULL) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

# %% 
# Supply shifters
(p_supply_shifters <- ggplot() +
  geom_function(
    aes(linetype = "Market 1"),
    fun = \(x) x + 2
  ) +
  geom_function(
    aes(linetype = "Market 1"),
    fun = \(x) -1 * x + 8
  ) +
  geom_function(
    aes(linetype = "Market 2"),
    fun = \(x) 0.8 * x + 4
  ) +
  geom_function(
    aes(linetype = "Market 2"),
    fun = \(x) -1 * x + 8
  ) +
  annotate("point", x = 3, y = 5) +
  annotate("point", x = 4 / 1.8, y = 0.8 * 4 / 1.8 + 4) +
  # annotate("segment", 
  #   x = 3, y = 5, xend = 6 / 1.9, yend = 0.8 * 6 / 1.9 + 4,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  scale_x_continuous(
    limits = c(0, 7)
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed")
  ) +
  labs(x = "Quantity", y = "Price", linetype = NULL) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

# %% 
# Demand shifters
(p_demand_shifters <- ggplot() +
  geom_function(
    aes(linetype = "Market 1"),
    fun = \(x) x + 2
  ) +
  geom_function(
    aes(linetype = "Market 1"),
    fun = \(x) -1 * x + 8
  ) +
  geom_function(
    aes(linetype = "Market 2"),
    fun = \(x) x + 2
  ) +
  geom_function(
    aes(linetype = "Market 2"),
    fun = \(x) -1.1 * x + 10
  ) +
  annotate("point", x = 3, y = 5) +
  annotate("point", x = 8 / 2.1, y = 8 / 2.1 + 2) +
  # annotate("segment", 
  #   x = 3, y = 5, xend = 6 / 1.9, yend = 0.8 * 6 / 1.9 + 4,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  scale_x_continuous(
    limits = c(0, 7)
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed")
  ) +
  labs(x = "Quantity", y = "Price", linetype = NULL) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

# %% 
kfbmisc::tikzsave(
  here("Slides/05_IV/figures/ex_endogeneity_problem.pdf"),
  p_endogeneity_problem, width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/05_IV/figures/ex_supply_shifters.pdf"),
  p_supply_shifters, width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/05_IV/figures/ex_demand_shifters.pdf"),
  p_demand_shifters, width = 8, height = 3.8
)
