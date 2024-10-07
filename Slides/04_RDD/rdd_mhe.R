# %%
library(tidyverse)
library(patchwork)
library(here)

# three dgps
y0_linear <- function(x) {
  x
}
y0_nonlin <- function(x) {
  0.5 * sin(6 * (x - 0.5)) + 0.5
}
y0_mistake <- function(x) {
  1 / (1 + exp(-25 * (x - 0.5)))
}

nobs <- 100
x <- runif(nobs)
eps <- rnorm(n = nobs, mean = 0, sd = 0.1)
y_linear <- y0_linear(x) + 0.25 * (x >= 0.5) + eps
y_nonlin <- y0_nonlin(x) + 0.25 * (x >= 0.5) + eps
y_mistake <- y0_mistake(x) + eps
rd_series <- tibble(
  x,
  treat = x > 0.5,
  y_linear, y_nonlin, y_mistake
)

x_seq <- seq(0, 1, by = 0.01)
true_dgp <- tibble(
  x = x_seq, treat = x_seq >= 0.5,
  y_linear = y0_linear(x),
  y_nonlin = y0_nonlin(x),
  y_mistake = y0_mistake(x)
)

# %%
# Make graph with ggplot2
base_plot <- ggplot(rd_series, aes(x = x, group = x > 0.5)) +
  geom_vline(
    xintercept = 0.5, linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  kfbmisc::theme_kyle(base_size = 14)

(p_linear <- base_plot +
  geom_point(aes(y = y_linear)) +
  stat_smooth(
    aes(y = y_linear),
    method = "lm", formula = y ~ x, se = FALSE,
    color = "#5C4CBF"
  ) +
  geom_line(
    aes(x = x, y = y_linear, group = treat),
    data = true_dgp,
    linetype = "dashed", color = kfbmisc::tailwind_color("zinc-800")
  ) +
  annotate("label",
    label = "\\color[HTML]{27272a}{$\\expec{ Y_i(0) }{ X_i = x }$}",
    x = 0.75, y = y0_linear(0.75) - 0.1,
    vjust = 1, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  labs(
    y = NULL, x = "Running Variable",
    title = "Linear $\\expec{Y_i(0)}{X_i}$"
  )
)

(p_nonlin <- base_plot +
  geom_point(aes(y = y_nonlin)) +
  stat_smooth(
    aes(y = y_nonlin),
    method = "lm",
    formula = y ~ poly(x, 2),
    se = FALSE,
    color = "#5C4CBF"
  ) +
  geom_line(
    aes(x = x, y = y_nonlin, group = treat),
    data = true_dgp,
    linetype = "dashed", color = kfbmisc::tailwind_color("zinc-800")
  ) +
  annotate("label",
    label = "\\color[HTML]{27272a}{$\\expec{ Y_i(0) }{ X_i = x }$}",
    x = 0.75, y = y0_nonlin(0.75) - 0.15,
    vjust = 1, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  labs(
    y = NULL, x = "Running Variable",
    title = "Non-Linear $\\expec{Y_i(0)}{X_i}$"
  )
)

(p_mistake <- base_plot +
  geom_point(aes(y = y_mistake)) +
  stat_smooth(
    aes(y = y_mistake),
    method = "lm", formula = y ~ x,
    se = FALSE,
    color = "#5C4CBF"
  ) +
  geom_line(
    aes(x = x, y = y_mistake, group = treat),
    data = true_dgp,
    linetype = "dashed", color = kfbmisc::tailwind_color("zinc-800")
  ) +
  annotate("label",
    label = "\\color[HTML]{27272a}{$\\expec{ Y_i(0) }{ X_i = x }$}",
    x = 0.75, y = y0_mistake(0.75) - 0.2,
    vjust = 1, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  ylab("Outcome") +
  labs(
    y = NULL, x = "Running Variable",
    title = "Non-Linear mistaken for linear"
  )
)

# %%
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/mhe_ex_linear.pdf"),
  p_linear,
  width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/mhe_ex_nonlinear.pdf"),
  p_nonlin,
  width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/mhe_ex_mistake.pdf"),
  p_mistake,
  width = 8, height = 4.2
)

# ggsave(p.rd.examples, file = "Figure 6-1-1-R.pdf", width = 5, height = 9)
