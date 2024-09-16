# %%
library(tidyverse) # data-wrangling
library(fixest) # all-things regression
library(haven) # load stata data

# %%
#' NSW experimental treatment and experimental control group
lalonde <- haven::read_dta("Labs/01_Potential_Outcomes/data/nsw.dta")

#' Non-experimental control group
cps <- haven::read_dta("Labs/01_Potential_Outcomes/data/cps_controls.dta")

#' Here we are taking the *treated* units from the NSW experiment and
#' appending the Current Population Survey (CPS) control group. The
#' CPS is representative of the population as a whole, while the NSW is a
#' *highly* selected experimental control group
nonexperimental <- bind_rows(
  lalonde |> filter(treat == 1),
  cps
)

# Regression adjustment
# %%
