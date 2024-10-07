# %%
library(tidyverse)
library(fixest)
library(here)

tikzDevice::setTikzDefaults()
default_packages <- getOption("tikzLatexPackages")
packages <- c(system.file("tikzsave/paper.sty", package = "kfbmisc"), system.file("tikzsave/math.sty", package = "kfbmisc"))
pkg_tex <- sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
options("tikzLatexPackages" = c(default_packages, "\\usepackage{bm}\n", pkg_tex))

# %%
# Access from here: https://economics.mit.edu/people/faculty/josh-angrist/mhe-data-archive
df <- haven::read_dta(here("Slides/04_RDD/data/lee2008_indiv.dta")) |>
  rename(
    won_election_t = myoutcome,
    won_election_tp1 = myoutcomenext,
    vote_margin_t = difshare
  ) |>
  # No missing data
  filter(!is.na(vote_margin_t) & !is.na(won_election_t)) |>
  filter(abs(vote_margin_t) < 0.25)

propensity_model_next <- feglm(
  won_election_tp1 ~ poly(vote_margin_t, 4) + won_election_t * poly(vote_margin_t, 4),
  data = df |> filter(!is.na(won_election_tp1)), # Only candidates who run again
  family = "logit"
)
df$prob_win_next <- predict(propensity_model_next, newdata = df, type = "response")

nonparametric_propensity_to_win <- df |>
  # Chop into 0.005 bins
  mutate(vote_margin_t_bin = floor(vote_margin_t / 0.005) * 0.005) |>
  summarize(
    .by = c(won_election_t, vote_margin_t_bin),
    prob_win_next = mean(won_election_tp1, na.rm = TRUE),
    mean_years_experience = mean(mofficeexp, na.rm = TRUE)
  )


(plot_rd_win_tp1 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = kfbmisc::tailwind_color("zinc-500")) +
  geom_point(
    aes(
      x = vote_margin_t_bin, y = prob_win_next,
      group = as.logical(won_election_t),
      color = as.logical(won_election_t)
    ),
    data = nonparametric_propensity_to_win
  ) +
  geom_smooth(
    aes(
      x = vote_margin_t, y = won_election_tp1,
      group = as.logical(won_election_t),
      color = as.logical(won_election_t)
    ),
    data = df,
    formula = y ~ poly(x, 4),
    method = "lm",
    se = FALSE
  ) +
  annotate(
    "label",
    # label = "\\color[HTML]{2DB25F}{$\\prob{W_{t+1}}{\\text{Margin}_{t} = m, W_t = 0 }$}",
    label = "\\color[HTML]{2DB25F}{$\\prob{W_{t+1}}{\\text{Margin}_{t} = m, W_t = 1}$}",
    x = 0.15, y = 0.55,
    vjust = 0, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  annotate(
    "label",
    # label = "\\color[HTML]{b21010}{\\prob{W_{t+1}}{\\text{Margin}_{t} = m, W_t = 0 }}",
    label = "\\color[HTML]{b21010}{$\\prob{W_{t+1}}{\\text{Margin}_{t} = m, W_t = 0}$}",
    x = -0.15, y = 0.15,
    vjust = 0, hjust = 0.5, size = rel(4), label.size = 0
  ) +
  labs(
    x = "Democratic Vote Share, Election $t$",
    y = "$\\prob{W_{t+1}}$",
    color = NULL
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2DB25F", "FALSE" = "#b21010"),
    guide = guide_none()
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
)

(plot_rd_mean_years_experience <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = kfbmisc::tailwind_color("zinc-500")) +
  geom_point(
    aes(
      x = vote_margin_t_bin, y = mean_years_experience,
      group = as.logical(won_election_t),
      color = as.logical(won_election_t)
    ),
    data = nonparametric_propensity_to_win
  ) +
  geom_smooth(
    aes(
      x = vote_margin_t, y = mofficeexp,
      group = as.logical(won_election_t),
      color = as.logical(won_election_t)
    ),
    data = df,
    formula = y ~ poly(x, 2),
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Democratic Vote Share, Election $t$",
    y = "Number of Past Victories before Election $t$",
    color = NULL
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2DB25F", "FALSE" = "#b21010"),
    guide = guide_none()
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
)


# %%
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/lee_win_tp1.pdf"),
  plot_rd_win_tp1,
  width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/04_RDD/figures/lee_win_prior_office_exp.pdf"),
  plot_rd_mean_years_experience,
  width = 8, height = 4.2
)
