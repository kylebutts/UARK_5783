# %% 
library(tidyverse)
library(fixest)
library(did)
library(here)

df <- here("Slides/06_Panel/data/ehec_data.csv") |>
  read_csv(show_col_types = FALSE) |>
  mutate(yexp2 = replace_na(yexp2, 0)) |>
  mutate(
    state = as.numeric(as.factor(stfips))
  )

# %% 
attgt_dins <- did::att_gt(
  yname = "dins", tname = "year", 
  idname = "state", gname = "yexp2", data = df,
  control_group = "nevertreated",
  base_period = "universal"
)

(p_attgt_2014 <- did::ggdid(attgt_dins, group = 2014) + 
  facet_null() + 
  labs(title = "$\\widehat{\\textrm{ATT}}(2014, t)$") +
  kfbmisc::theme_kyle() + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

(p_attgt_2015 <- did::ggdid(attgt_dins, group = 2015) + 
  facet_null() + 
  labs(title = "$\\widehat{\\textrm{ATT}}(2015, t)$") +
  kfbmisc::theme_kyle() + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

(p_attgt_2016 <- did::ggdid(attgt_dins, group = 2016) + 
  facet_null() + 
  labs(title = "$\\widehat{\\textrm{ATT}}(2016, t)$") +
  kfbmisc::theme_kyle() + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

(p_attgt_2017 <- did::ggdid(attgt_dins, group = 2017) + 
  facet_null() + 
  labs(title = "$\\widehat{\\textrm{ATT}}(2017, t)$") +
  kfbmisc::theme_kyle() + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

(p_attgt_2019 <- did::ggdid(attgt_dins, group = 2019) + 
  facet_null() + 
  labs(title = "$\\widehat{\\textrm{ATT}}(2019, t)$") +
  kfbmisc::theme_kyle() + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

# %% 
att_dynamic_dins <- aggte(attgt_dins, type = "dynamic")

(p_dynamic <- did::ggdid(att_dynamic_dins) +
  kfbmisc::theme_kyle() + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot",
  )
)

# %% 
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/ex_medicare_expansion_attgt_2014.pdf"),
  p_attgt_2014, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/ex_medicare_expansion_attgt_2015.pdf"),
  p_attgt_2015, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/ex_medicare_expansion_attgt_2016.pdf"),
  p_attgt_2016, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/ex_medicare_expansion_attgt_2017.pdf"),
  p_attgt_2017, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/ex_medicare_expansion_attgt_2019.pdf"),
  p_attgt_2019, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/06_Panel/figures/ex_medicare_expansion_dynamic.pdf"),
  p_dynamic, width = 8, height = 4.2
)
