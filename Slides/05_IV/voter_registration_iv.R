## voter_registration_iv.R -----------------------------------------------------
## Encouragement design: party registration and political beliefs
## Gerber, Huber, and Washington (2010, APSR)
library(tidyverse)
library(fixest)

# Z: treat
# randomized information encouraging party registration
# D: pt_id_with_lean
# registered with the party toward which the voter leaned
# Y: pt_voteevalalignindex
# post-treatment index of alignment with that party's positions
voters <- read_csv(
  "data/gerber_et_al_2010.csv",
  show_col_types = FALSE
)


## Two-stage least squares estimates -------------------------------------------
## Relevance asks whether randomized encouragement changes party registration.
first_stage <- feols(
  pt_id_with_lean ~ treat,
  data = voters,
  vcov = "HC1"
)

reduced_form <- feols(
  pt_voteevalalignindex ~ treat,
  data = voters,
  vcov = "HC1"
)

## Two-stage least squares estimate "by hand"
coef(reduced_form)["treat"] / coef(first_stage)["treat"]

## Using fixest
iv_model <- feols(
  pt_voteevalalignindex ~ 1 | pt_id_with_lean ~ treat,
  data = voters,
  vcov = "HC1"
)

## Notice the F stat is very small
fitstat(iv_model, "ivf")

## Plot the two experimental contrasts -----------------------------------------
first_stage_means <- voters |>
  summarize(
    estimate = mean(pt_id_with_lean, na.rm = TRUE),
    std_error = sd(pt_id_with_lean, na.rm = TRUE) /
      sqrt(sum(!is.na(pt_id_with_lean))),
    .by = treat
  ) |>
  mutate(contrast = "First stage: registered with leaned party")

reduced_form_means <- voters |>
  summarize(
    estimate = mean(pt_voteevalalignindex, na.rm = TRUE),
    std_error = sd(pt_voteevalalignindex, na.rm = TRUE) /
      sqrt(sum(!is.na(pt_voteevalalignindex))),
    .by = treat
  ) |>
  mutate(contrast = "Reduced form: post-election alignment index")

experimental_means <- bind_rows(first_stage_means, reduced_form_means) |>
  mutate(
    treatment = factor(
      treat,
      levels = c(0, 1),
      labels = c("No encouragement", "Encouragement")
    )
  )

encouragement_plot <- ggplot(
  experimental_means,
  aes(x = treatment, y = estimate, color = treatment)
) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std_error,
      ymax = estimate + 1.96 * std_error
    ),
    width = 0.08,
    linewidth = 0.7
  ) +
  geom_point(size = 3) +
  facet_wrap(~contrast, scales = "free_y", nrow = 1) +
  scale_color_manual(
    values = c("#B3114B", "#2B5269"),
    labels = c("No Encouragement", "Encouragement"),
    guide = "none"
  ) +
  labs(x = NULL, y = "Group mean and 95% confidence interval") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 12, hjust = 1),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path("figures", "voter_registration_encouragement.pdf"),
  encouragement_plot,
  width = 8,
  height = 4.2
)


## Characterize the compliers (appendix; not covered during class) -------------
## For a pre-treatment characteristic X, the 2SLS coefficient from regressing
## X * D on D, instrumented by Z, identifies E[X | complier].

complier_mean <- function(variable, data = voters) {
  model <- feols(
    xpd(
      I(.[variable] * pt_id_with_lean) ~ 1 |
        pt_id_with_lean ~ treat
    ),
    data = data,
    vcov = "HC1"
  )

  c(
    population_mean = mean(data[[variable]], na.rm = TRUE),
    complier_mean = unname(coef(model)["fit_pt_id_with_lean"]),
    complier_se = unname(se(model)["fit_pt_id_with_lean"])
  )
}

complier_profile <- rbind(
  age = complier_mean("age"),
  voted_in_2006 = complier_mean("voted2006"),
  church_attendance = complier_mean("pt_church")
)

cat("\nComplier profiles (full sample vs. compliers):\n")
print(complier_profile)
