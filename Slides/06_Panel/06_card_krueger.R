library(tidyverse)
library(here)

# Taken from AER table 2
df <- tibble(
  location = c("NJ", "NJ", "Eastern PA", "Eastern PA"),
  time = ymd(c("1992-03-01", "1992-12-15", "1992-03-01", "1992-12-15")),
  # n_firms = c()
  employment = c(
    20.4, 21.0, 23.3, 21.2
  ),
  se_employment = c(
    0.51, 0.52, 1.35, 0.94
  )
)

df_implied <- tibble(
  location = c("NJ", "NJ", "NJ (Implied $y(0)$)", "NJ (Implied $y(0)$)", "Eastern PA", "Eastern PA"),
  time = ymd(c("1992-03-01", "1992-12-15", "1992-03-01", "1992-12-15", "1992-03-01", "1992-12-15")),
  # n_firms = c()
  employment = c(
    20.4, 21.0, 20.4, 18.3, 23.3, 21.2
  )
)

# %%
(plot_raw <- ggplot() +
  geom_point(
    aes(x = time, y = employment, color = location),
    data = df, size = 2
  ) +
  geom_line(
    aes(x = time, y = employment, color = location, group = location),
    data = df, linewidth = 1.2
  ) + 
  labs(
    x = NULL, y = "Full-time Employment Rate (\\%)", 
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = c("#98BC78", "#3B7D9B")
  ) +
  scale_y_continuous(limits = c(17, 25), expand = c(0, 0)) + 
  scale_x_date(date_labels = "%, %Y") + 
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
    axis.title = element_text(size = rel(1)),
  ))

(plot_implied_y0 <- ggplot() +
  geom_point(
    aes(x = time, y = employment, color = location),
    data = df_implied, size = 2
  ) +
  geom_line(
    aes(x = time, y = employment, color = location, group = location, linetype = location),
    data = df_implied,
    linewidth = 1.2
  ) + 
  annotate(
    geom = "segment",
    x = ymd("1992-12-15"), xend = ymd("1992-12-15"), 
    y = 18.5, yend = 20.8,
    color = "#3B7D9B", linewidth = 1,
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  labs(
    x = NULL, y = "Full-time Employment Rate (\\%)", 
    color = NULL, linetype = NULL
  ) +
  scale_color_manual(
    values = c("#98BC78", "#3B7D9B", "#3B7D9B")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed")
  ) +
  scale_y_continuous(limits = c(17, 25), expand = c(0, 0)) + 
  scale_x_date(date_labels = "%, %Y") + 
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
  here("Slides/06_Panel/figures/card_krueger_raw_plots.pdf"),
  plot = plot_raw, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/card_krueger_implied_y0.pdf"),
  plot = plot_implied_y0, width = 8, height = 4.2
)



