# %%
library(tidyverse)
library(here)
# 5th graders
df_5 <- read_csv(
  "/Users/kbutts/Downloads/dataverse_files/final5.csv",
  show_col_types = FALSE
)

collapsed_5 <- df_5 |>
  filter(c_leom==1 & c_pik<3) |> 
  summarize(
    .by = c_size,
    # mean_class_size = mean(mean_class_size)
    mean_class_size = mean(classize)
  ) |>
  arrange(c_size)

maimondes_rule <- function(enroll) {
  enroll / (floor((enroll - 1) / 40) + 1)
}
predicted <- tibble(
  c_size = seq(5, 210, by = 1),
  maimondes_class_size = maimondes_rule(c_size)
)

# %%
(plot_maimondes <- ggplot() +
  geom_hline(
    yintercept = c(40, 41 / 2, 81 / 3, 121 / 4, 161 / 5),
    linetype = "dotted",
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  geom_line(
    aes(x = c_size, y = maimondes_class_size),
    data = predicted,
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_y_continuous(limits = c(5, NA), breaks = seq(5, 40, by = 5)) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Enrollment count", y = "Class size") +
  kfbmisc::theme_kyle(base_size = 12, grid = "", grid_minor = "") +
  theme(axis.title = element_text(size = rel(1)))
)

(plot_observed <- plot_maimondes +
  geom_line(
    aes(x = c_size, y = mean_class_size),
    data = collapsed_5,
  )
)

ggplot() +
  geom_hline(
    yintercept = c(40, 41 / 2, 81 / 3, 121 / 4, 161 / 5),
    linetype = "dotted",
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  geom_line(
    aes(x = c_size, y = mean_class_size),
    data = collapsed_5,
  ) +
  scale_y_continuous(limits = c(5, NA), breaks = seq(5, 40, by = 5)) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Enrollment count", y = "Class size") +
  kfbmisc::theme_kyle(base_size = 12, grid = "", grid_minor = "") +
  theme(axis.title = element_text(size = rel(1)))
