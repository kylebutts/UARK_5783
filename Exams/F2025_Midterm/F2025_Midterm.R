# %% 
library(tidyverse)
library(fixest)
library(MatchIt)
library(WeightIt)
library(DRDID)
library(here)

# Almond, Chay and Lee (2005, QJE). The costs of low birth weight
df <- haven::read_dta(here("Labs/03_Selection_on_Observables/data/cattaneo2.dta"))

feols(bweight ~ i(mbsmoke), data = df, vcov = "HC1")
