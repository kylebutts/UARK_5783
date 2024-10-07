# %%
library(tidyverse)
library(fixest)
library(here)

fs::dir_create(here("Slides/04_RDD/figures/"))

tikzDevice::setTikzDefaults()
default_packages <- getOption("tikzLatexPackages")
packages <- c(system.file("tikzsave/paper.sty", package = "kfbmisc"), system.file("tikzsave/math.sty", package = "kfbmisc"))
pkg_tex <- sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
options("tikzLatexPackages" = c(default_packages, "\\usepackage{bm}\n", pkg_tex))

# %%
#| 'Define DGP 1'
df_dgp_1_y0 <- tribble(
  ~x, ~y,
  -3.2, -0.3,
  -2, -0.3,
  -1.2, -0.3,
  0, -0.1,
  0.5, 0.2,
  1, 0.2,
  2, 0.4,
  3.2, 1.0
)

dgp_1_y0 <- function(x) {
  est <- feols(y ~ poly(x, 4), data = df_dgp_1_y0)
  predict(est, newdata = data.frame(x = x))
}

df_dgp_1_y1 <- tribble(
  ~x, ~y,
  -3.2, 0.55,
  -2, 0.6,
  -1.2, 0.6,
  -0.5, 0.6,
  0, 0.9,
  0.5, 0.9,
  1, 1.2,
  2, 1.2,
  3.2, 1.4
)

dgp_1_y1 <- function(x) {
  est <- feols(y ~ poly(x, 4), data = df_dgp_1_y1)
  predict(est, newdata = data.frame(x = x))
}

ggplot() +
  # geom_point(aes(x = x, y = y), df_dgp_1_y1, color = "red") +
  geom_function(fun = dgp_1_y1, xlim = c(-3, 3), color = "red") +
  # geom_point(aes(x = x, y = y), df_dgp_1_y0, color = "blue") +
  geom_function(fun = dgp_1_y0, xlim = c(-3, 3), color = "blue") +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-2, 2)) +
  kfbmisc::theme_kyle(grid_minor = "")

# %%
#| 'Define DGP 2'
df_dgp_2_y1 <- tribble(
  ~x, ~y,
  -3, 2.5,
  -2.7, 2,
  -2.5, 1.2,
  -2, 0.8,
  -1.5, 0.7,
  -0.8, 0.8,
  -0.4, 0.9,
  0, 1,
  0.5, 1.3,
  1, 1.2,
  1.3, 1,
  1.7, 0.4,
  2, -0.2,
  2.3, -1.6
)

dgp_2_y1 <- function(x) {
  est <- feols(y ~ poly(x, 6), data = df_dgp_2_y1)
  predict(est, newdata = data.frame(x = x))
}

df_dgp_2_y0 <- tribble(
  ~x, ~y,
  -3, 1.8,
  -2.5, 0.7,
  -2, -0.1,
  -1.5, -0.45,
  -1, -0.4,
  0, -0.1,
  0.5, 0.2,
  1, 0,
  2, -1.4,
  2.1, -1.6
)

dgp_2_y0 <- function(x) {
  est <- feols(y ~ poly(x, 6), data = df_dgp_2_y0)
  predict(est, newdata = data.frame(x = x))
}

ggplot() +
  geom_point(aes(x = x, y = y), df_dgp_2_y1, color = "red") +
  geom_function(fun = dgp_2_y1, xlim = c(-3, 2.5), color = "red") +
  geom_point(aes(x = x, y = y), df_dgp_2_y0, color = "blue") +
  geom_function(fun = dgp_2_y0, xlim = c(-3, 2.5), color = "blue") +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-2, 2)) +
  kfbmisc::theme_kyle(grid_minor = "")



# %%
gen_data <- function(n = 1000, fun_y1, fun_y0) {
  assertthat::assert_that(is.function(fun_y1), is.function(fun_y0), msg = "`fun_y1` and `fun_y0` must be functions")

  x <- runif(n, min = -2.5, max = 2.5)
  y0 <- fun_y0(x)
  y1 <- fun_y1(x)
  eps <- rnorm(n = n, mean = 0, sd = 0.25)

  bw <- 0.4
  y0_rand <- y0
  y0_rand[x > -1 * bw & x < bw] <- mean(y0[x > -1 * bw & x < bw])

  y1_rand <- y1
  y1_rand[x > -1 * bw & x < bw] <- mean(y1[x > -1 * bw & x < bw])

  y <- (x < 0) * y0 + (x >= 0) * y1
  y_rand <- (x < 0) * y0_rand + (x >= 0) * y1_rand

  true_te <- fun_y1(0) - fun_y0(0)
  true_te_rand <- mean(y1[x < 0 & x > -1 * bw]) - mean(y0[x > 0 & x < bw])

  group <- if_else(x < 0, "y0", "y1")

  df <- data.frame(
    x = x, treat = x > 0, group = group, eps = eps,
    y0 = y0, y1 = y1, y = y, y_obs = y + eps,
    y0_rand = y0_rand, y1_rand = y1_rand,
    true_te = true_te
  )
  df <- df |> arrange(x)
  return(df)
}

# %%
set.seed(20241002)
df_dgp_1 <- gen_data(n = 200, fun_y1 = dgp_1_y1, fun_y0 = dgp_1_y0)
df_dgp_2 <- gen_data(n = 200, fun_y1 = dgp_2_y1, fun_y0 = dgp_2_y0)

# %%
(p_dgp_1 <- ggplot() +
  geom_vline(
    xintercept = 0, linetype = "dashed", linewidth = 1.2, color = kfbmisc::tailwind_color("zinc-800")
  ) +
  geom_line(
    aes(x = x, y = y0, color = "y0", group = group),
    data = df_dgp_1 |> filter(x < 0), linewidth = 1.5
  ) +
  geom_line(
    aes(x = x, y = y0, color = "y0", group = group),
    data = df_dgp_1 |> filter(x > 0), linewidth = 1.5, linetype = "22"
  ) +
  geom_line(
    aes(x = x, y = y1, color = "y1", group = group),
    data = df_dgp_1 |> filter(x < 0), linewidth = 1.5, linetype = "22"
  ) +
  geom_line(
    aes(x = x, y = y1, color = "y1", group = group),
    data = df_dgp_1 |> filter(x > 0), linewidth = 1.5
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{ffc517}{$\\expec{ Y_i(0) }{ X_i = x }$}",
    x = -1, y = dgp_1_y0(-1) - 0.2,
    vjust = 1, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{B3114B}{$\\expec{ Y_i(1) }{ X_i = x }$}",
    x = 1, y = dgp_1_y1(0.2) + 0.2,
    vjust = 0, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  scale_color_manual(
    values = c("y1" = "#B3114B", "y0" = "#ffc517"),
    guide = guide_none()
  ) +
  scale_x_continuous(
    limits = c(-2.5, 2.5),
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("", "", "$c$", "", "")
  ) +
  scale_y_continuous(limits = c(-2, 2)) +
  labs(x = "Score, $X_i$", y = "$Y_i(1)$ and $Y_i(0)$") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "")
)

(p_dgp_1_tau <- p_dgp_1 +
  annotate(
    "label",
    label = "\\color[HTML]{3f3f46}{$\\tau_{\\texttt{RD}}$}",
    x = 0.05, y = (dgp_1_y1(0) + dgp_1_y0(0)) / 2,
    vjust = 0.5, hjust = 0, size = rel(4), label.size = 0
  ) +
  annotate(
    "segment",
    x = 0, xend = 0, y = dgp_1_y1(0), yend = dgp_1_y0(0),
    color = "#3f3f46",
    linewidth = 1, arrow = arrow(ends = "both", length = unit(0.2, "cm"))
  )
)

# %%
bw <- 0.4
yhat_plus <- df_dgp_1 |>
  filter(x > 0 & x < bw) |>
  pull(y1) |>
  mean()
yhat_minus <- df_dgp_1 |>
  filter(x < 0 & x > -1 * bw) |>
  pull(y0) |>
  mean()

(p_dgp_1_rand_tau <- ggplot() +
  geom_vline(
    xintercept = 0, linetype = "dashed", linewidth = 1.2, color = kfbmisc::tailwind_color("zinc-800")
  ) +
  geom_vline(
    xintercept = c(-0.4, 0.4), linetype = "dashed", linewidth = 1.2, color = kfbmisc::tailwind_color("zinc-400")
  ) +
  annotate("line",
    x = c(-1 * bw, 0), y = yhat_minus, color = "#ffc517",
    linewidth = 1.5
  ) +
  annotate("line",
    x = c(0, bw), y = yhat_minus, color = "#ffc517",
    linewidth = 1.5, linetype = "22"
  ) +
  annotate("line",
    x = c(-1 * bw, 0), y = yhat_plus, color = "#B3114B",
    linewidth = 1.5, linetype = "22"
  ) +
  annotate("line",
    x = c(0, bw), y = yhat_plus, color = "#B3114B",
    linewidth = 1.5
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{ffc517}{$\\expec{ Y_i(0) }{ X_i = x }$}",
    x = -0.9, y = yhat_minus,
    vjust = 1, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{B3114B}{$\\expec{ Y_i(1) }{ X_i = x }$}",
    x = 0.9, y = yhat_plus,
    vjust = 0, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{3f3f46}{$\\tau_{\\texttt{Rand}}$}",
    x = 0.05, y = (yhat_minus + yhat_plus) / 2,
    vjust = 0.5, hjust = 0, size = rel(4), label.size = 0
  ) +
  annotate(
    "segment",
    x = 0, xend = 0, y = yhat_minus, yend = yhat_plus,
    color = "#3f3f46",
    linewidth = 1, arrow = arrow(ends = "both", length = unit(0.2, "cm"))
  ) +
  scale_color_manual(
    values = c("y1" = "#B3114B", "y0" = "#ffc517"),
    guide = guide_none()
  ) +
  scale_x_continuous(
    limits = c(-2.5, 2.5),
    breaks = c(-2, -1, -0.4, 0, 0.4, 1, 2),
    labels = c("", "", "$c - h$", "$c$", "$c + h$", "", "")
  ) +
  scale_y_continuous(limits = c(-2, 2)) +
  labs(x = "(Test) Score, $X_i$", y = "$Y_i(1)$ and $Y_i(0)$") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "")
)

# %%
#| label: "DGP 2"
(p_dgp_2_tau <- ggplot() +
  geom_vline(
    xintercept = 0, linetype = "dashed", linewidth = 1.2, color = kfbmisc::tailwind_color("zinc-800")
  ) +
  geom_line(
    aes(x = x, y = y0, color = "y0", group = group),
    data = df_dgp_2 |> filter(x < 0), linewidth = 1.5
  ) +
  geom_line(
    aes(x = x, y = y0, color = "y0", group = group),
    data = df_dgp_2 |> filter(x > 0), linewidth = 1.5, linetype = "22"
  ) +
  geom_line(
    aes(x = x, y = y1, color = "y1", group = group),
    data = df_dgp_2 |> filter(x < 0), linewidth = 1.5, linetype = "22"
  ) +
  geom_line(
    aes(x = x, y = y1, color = "y1", group = group),
    data = df_dgp_2 |> filter(x > 0), linewidth = 1.5
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{ffc517}{$\\expec{ Y_i(0) }{ X_i = x }$}",
    x = -1, y = dgp_2_y0(-1) - 0.2,
    vjust = 1, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{B3114B}{$\\expec{ Y_i(1) }{ X_i = x }$}",
    x = 1, y = dgp_2_y1(0.2) + 0.2,
    vjust = 0, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  annotate(
    "label",
    label = "\\color[HTML]{3f3f46}{$\\tau_{\\texttt{RD}}$}",
    x = 0.05, y = (dgp_2_y1(0) + dgp_2_y0(0)) / 2,
    vjust = 0.5, hjust = 0, size = rel(4), label.size = 0
  ) +
  annotate(
    "segment",
    x = 0, xend = 0, y = dgp_2_y1(0), yend = dgp_2_y0(0),
    color = "#3f3f46",
    linewidth = 1, arrow = arrow(ends = "both", length = unit(0.2, "cm"))
  ) +
  scale_color_manual(
    values = c("y1" = "#B3114B", "y0" = "#ffc517"),
    guide = guide_none()
  ) +
  scale_x_continuous(
    limits = c(-2.5, 2.5),
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("", "", "$c$", "", "")
  ) +
  scale_y_continuous(limits = c(-2, 2)) +
  labs(x = "Score, $X_i$", y = "$Y_i(1)$ and $Y_i(0)$") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "")
)


# %%
#| label: "dgp 2 data"
estimation_base_plot <- function(df, bw = 0.4) {
  if (length(bw) == 1) {
    labels_neg_bw <- "$c - h$"
    labels_pos_bw <- "$c + h$"
  } else {
    labels_neg_bw <- map(seq_along(bw), \(i) {
      sprintf("$c - h_{%s}$", i)
    })
    labels_pos_bw <- map(seq_along(bw), \(i) {
      sprintf("$c + h_{%s}$", i)
    })
  }
  breaks <- c(-2, -1, -1 * bw, 0, bw, 1, 2)
  labels <- c("", "", labels_neg_bw, "$c$", labels_pos_bw, "", "")
  x_scale <- scale_x_continuous(
    limits = c(-2.5, 2.5),
    breaks = breaks,
    labels = labels
  )

  (p_est_base <- ggplot() +
    geom_vline(
      xintercept = 0, linetype = "dashed", linewidth = 1.2, color = kfbmisc::tailwind_color("zinc-800")
    ) +
    geom_vline(
      xintercept = c(-1 * bw, bw), linetype = "dashed", linewidth = 1.2, color = kfbmisc::tailwind_color("zinc-400")
    ) +
    geom_point(
      aes(x = x, y = y_obs),
      data = df, size = 0.8
    ) +
    geom_line(
      aes(x = x, y = y0),
      data = df |> filter(x < 0),
      linewidth = 1.4, color = kfbmisc::tailwind_color("zinc-200")
    ) +
    geom_line(
      aes(x = x, y = y1),
      data = df |> filter(x > 0),
      linewidth = 1.4, color = kfbmisc::tailwind_color("zinc-200")
    ) +
    x_scale +
    scale_y_continuous(limits = c(-2, 2)) +
    labs(
      x = "Score, $X_i$", y = "$Y_i$",
      title = "$\\hat{\\tau}_{\\texttt{RD}} = \\color[HTML]{B3114B}{\\hat{\\mu}^+} - \\color[HTML]{ffc517}{\\hat{\\mu}^-}$"
    ) +
    kfbmisc::theme_kyle(base_size = 14, grid_minor = "") +
    theme(
      plot.title = element_text(margin = margin(b = 12, unit = "pt"))
    )
  )
}

# %%
local_constant <- function(df, bw) {
  est <- feols(
    y_obs ~ 0 + i(treat),
    data = df |>
      filter(abs(x) < bw)
  )
}
local_linear <- function(df, bw) {
  est <- feols(
    y_obs ~ 1 + i(treat) + i(treat, x),
    data = df |>
      filter(abs(x) < bw)
  )
}
local_quadratic <- function(df, bw) {
  est <- feols(
    y_obs ~ 1 + i(treat) + i(treat, x) + i(treat, x^2),
    data = df |>
      filter(abs(x) < bw)
  )
}
add_estimator <- function(df, estimator, bw = 0.4, add_text = TRUE) {
  est <- estimator(df, bw)
  pts <- bind_rows(
    data.frame(x = seq(-1 * bw, 0, bw / 10), treat = FALSE),
    data.frame(x = seq(0, bw, bw / 10), treat = TRUE)
  )
  pts$y <- predict(est, newdata = pts)
  yhat_plus <- pts[pts$x == 0 & pts$treat == TRUE, "y"]
  yhat_minus <- pts[pts$x == 0 & pts$treat == FALSE, "y"]

  res <- list(
    geom_line(
      aes(x = x, y = y),
      data = pts[pts$treat == FALSE, ],
      color = "#ffc517", linewidth = 1.5
    ),
    geom_line(
      aes(x = x, y = y),
      data = pts[pts$treat == TRUE, ],
      color = "#B3114B", linewidth = 1.5
    ),
    annotate("point", x = 0, y = yhat_minus, color = "#ffc517", size = 2),
    annotate("point", x = 0, y = yhat_plus, color = "#B3114B", size = 2)
  )

  if (add_text == TRUE) {
    res <- c(res, list(
      annotate("text",
        label = "$\\color[HTML]{ffc517}{\\hat{\\mu}^-}$",
        x = 0.06, y = yhat_minus,
        vjust = 0.5, hjust = 0, size = rel(4)
      ),
      annotate("text",
        label = "$\\color[HTML]{B3114B}{\\hat{\\mu}^+}$",
        x = -0.06, y = yhat_plus,
        vjust = 0.5, hjust = 1, size = rel(4)
      )
    ))
  }

  return(res)
}

# %%
estimation_base_plot(df = df_dgp_2, bw = c(0.4, 0.8))

p_dgp_2_local_constant_est <-
  estimation_base_plot(df = df_dgp_2, bw = 0.4) +
  add_estimator(df = df_dgp_2, estimator = local_constant, bw = 0.4)

p_dgp_2_local_linear_est <-
  estimation_base_plot(df = df_dgp_2, bw = 0.4) +
  add_estimator(df = df_dgp_2, estimator = local_linear, bw = 0.4)

p_dgp_2_local_quadratic_est <-
  estimation_base_plot(df = df_dgp_2, bw = 0.4) +
  add_estimator(df = df_dgp_2, estimator = local_quadratic, bw = 0.4)

(p_dgp_2_local_constant_est_large_bw <-
  estimation_base_plot(df = df_dgp_2, bw = c(0.3, 0.8)) +
  add_estimator(df = df_dgp_2, estimator = local_constant, bw = 0.3, add_text = FALSE) +
  add_estimator(df = df_dgp_2, estimator = local_constant, bw = 0.8, add_text = FALSE)
)

(p_dgp_2_local_linear_est_large_bw <-
  estimation_base_plot(df = df_dgp_2, bw = c(0.3, 0.8)) +
  add_estimator(df = df_dgp_2, estimator = local_linear, bw = 0.3, add_text = FALSE) +
  add_estimator(df = df_dgp_2, estimator = local_linear, bw = 0.8, add_text = FALSE)
)



# %%



# %%
1 + 1

# %%
# Slow to save, so down here
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_1_lines.pdf"),
  p_dgp_1,
  width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_1_tau.pdf"),
  p_dgp_1_tau,
  width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_1_rand_tau.pdf"),
  p_dgp_1_rand_tau,
  width = 8, height = 3.8
)

kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_2_tau.pdf"),
  p_dgp_2_tau,
  width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_2_local_constant_estimator.pdf"),
  p_dgp_2_local_constant_est,
  width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_2_local_linear_estimator.pdf"),
  p_dgp_2_local_linear_est,
  width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_2_local_quadratic_estimator.pdf"),
  p_dgp_2_local_quadratic_est,
  width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_2_local_constant_large_bw.pdf"),
  p_dgp_2_local_constant_est_large_bw,
  width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/plot_dgp_2_local_linear_large_bw.pdf"),
  p_dgp_2_local_linear_est_large_bw,
  width = 8, height = 3.8
)
