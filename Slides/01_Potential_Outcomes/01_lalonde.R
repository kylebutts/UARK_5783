# %% 
library(tidyverse)
library(fixest)
library(haven)

# Experimental
lalonde <- haven::read_dta("Slides/01_Potential_Outcomes/data/nsw.dta")

cps <- haven::read_dta("Slides/01_Potential_Outcomes/data/cps_controls.dta")

