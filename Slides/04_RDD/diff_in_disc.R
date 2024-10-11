# %%
library(tidyverse)
library(kfbmisc)
library(patchwork)

tikzDevice::setTikzDefaults()
default_packages <- getOption("tikzLatexPackages")
packages <- c(system.file("tikzsave/paper.sty", package = "kfbmisc"), system.file("tikzsave/math.sty", package = "kfbmisc"))
pkg_tex <- sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
options("tikzLatexPackages" = c(default_packages, "\\usepackage{bm}\n", pkg_tex))

# ---- Pre Plot ----------------------------------------------------------------
# %%
te_0 <- 0.2
x_grid <- tibble(x = seq(-1, 1))

# Y_0(0)
y_pre_0 <- function(x) {
  0.05 + .02 * x + 0.08 * x^2 + 0.1 * x^3 +
    te_0 * (x > 0)
}

(plot_y_pre <- ggplot(x_grid, aes(x = x)) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey20", size = 1) +
  # Left and right-hand side of the cutoff
  geom_function(
    fun = y_pre_0, colour = "#107895", xlim = c(-1, 0), linetype = "solid",
    size = 1.1
  ) +
  geom_function(
    fun = y_pre_0, colour = "#107895", xlim = c(0.0001, 1), linetype = "solid",
    size = 1.1
  ) +
  annotate("label",
    label = "$\\expec{Y_{t=0}(0)}{X_i = x}$",
    x = 0.5, y = y_pre_0(0.5) - 0.06,
    colour = "#107895", hjust = 0, vjust = 0,
    size = rel(4), label.size = 0
  ) +
  annotate("label",
    x = 0.05, y = y_pre_0(-0.001) + (y_pre_0(0.001) - y_pre_0(-0.001)) / 2,
    label = "$\\tau_{t=0}$", colour = "grey60",
    hjust = 0, size = rel(4), label.size = 0
  ) +
  annotate("segment",
    x = 0, xend = 0, y = y_pre_0(0), yend = y_pre_0(0) + te_0,
    size = 1, colour = "grey60"
  ) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(limits = c(0, 0.75)) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c(" ", "Border Cutoff", " "), limits = c(-1, 1.2)) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    axis.text.y = element_blank(), axis.ticks = element_blank()
  )
)


# ---- Post Plot ---------------------------------------------------------------
# %%
# Y_0(0)
y_post_0 <- function(x) {
  0.05 + .02 * x + 0.08 * x^2 + 0.1 * x^3 +
    te_0 * (x > 0)
}
# Y_0(1)
y_post_1 <- function(x) {
  0.55 + .05 * x + 0.05 * x^2 + 0.1 * x^3
}


x_grid <- tibble(x = seq(-1, 1))
(plot_y_post <- ggplot(x_grid, aes(x = x)) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey20", size = 1) +
  # Y_1(0)
  geom_function(
    fun = y_post_0, colour = "#107895", xlim = c(-1, 0), linetype = "solid",
    size = 1.1
  ) +
  geom_function(
    fun = y_post_0, colour = "#107895", xlim = c(0.001, 1), linetype = "dashed",
    size = 1.1
  ) +
  # Y_1(1)
  geom_function(
    fun = y_post_1, colour = "#9A2515", xlim = c(-1, 0), linetype = "dashed",
    size = 1.1
  ) +
  geom_function(
    fun = y_post_1, colour = "#9A2515", xlim = c(0, 1), linetype = "solid",
    size = 1.1
  ) +
  annotate("label",
    x = 0.5, y = y_post_0(0.5) - 0.03, colour = "#107895",
    label = "$\\expec{Y_{t=1}(0)}{X_i = x}$",
    hjust = 0, size = rel(4), label.size = 0
  ) +
  annotate("label",
    x = 0.4, y = y_post_1(0.5) + 0.08, colour = "#9A2515",
    label = "$\\expec{Y_{t=1}(1)}{X_i = x}$",
    hjust = 0.5, size = rel(4), label.size = 0
  ) +
  # RD_0
  annotate("segment",
    x = 0, xend = 0, y = y_post_0(0), yend = y_post_0(0) + te_0,
    size = 1, color = "grey60"
  ) +
  annotate("label",
    x = 0.02, y = y_post_0(0) + te_0 / 2,
    label = "$\\tau_{t=0}$",
    color = "grey60", hjust = 0, size = rel(4), label.size = 0
  ) +
  # RD_1 - RD_0
  annotate("segment",
    x = 0, xend = 0, y = y_post_0(0) + te_0, yend = y_post_1(0),
    size = 1
  ) +
  annotate("label",
    x = 0.02, y = y_post_0(0) + te_0 + (y_post_1(0) - y_post_0(0) - te_0) * 3 / 5,
    label = "$LATE = \\tau_{t=1} - \\tau_{t=0}$",
    hjust = 0, size = rel(4), label.size = 0
  ) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c(" ", "Border Cutoff", " "), limits = c(-1, 1.2)) +
  scale_y_continuous(limits = c(0, 0.75)) +
  labs(x = NULL, y = NULL) +
  kfbmisc::theme_kyle(base_size = 14) +
  # Remove x- and y-axes
  theme(
    axis.text.y = element_blank(), axis.ticks = element_blank()
  )
)

# %%
combined <- (plot_y_pre + plot_y_post)

# %%
kfbmisc::tikzsave(
  here::here("Slides/04_RDD/figures/diff_in_disc_pre.pdf"),
  plot = plot_y_pre, width = 6, height = 4.2
)
kfbmisc::tikzsave(
  here::here("Slides/04_RDD/figures/diff_in_disc_post.pdf"),
  plot = plot_y_post, width = 6, height = 4.2
)
# kfbmisc::tikzsave(
#   here::here("Slides/04_RDD/figures/diff_in_disc.pdf"),
#   plot = combined, width = 8, height = 4.2
# )
