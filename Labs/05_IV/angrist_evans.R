library(tidyverse)
library(here)
library(fixest)

# Angrist and Evans (1998)
# 
# samesex - `TRUE` if first-two kids are of the same-sex
# twins_2 - `TRUE` if second successful pregnancy is twins
# mom_worked - `TRUE` if mom worked for pay
# mom_weeks_worked - number of weeks worked in the year by the mother
# morekids - More than 2 children
# kidcount - Number of children,worked for pay is mom worked,
# moreths - Mom has more than hs education
# blackm - `TRUE` if mom is Black
# hispm - `TRUE` if mom is Hispanic
# whitem - - `TRUE` if mom is White
df <- read_csv("Labs/05_IV/data/angrist_evans.csv")


# IV Estimation ----
# Z_i `samesex` as instrument for
# D_i `morekids`
# y_i `mom_weeks_worked`

# First-stage


# Reduced-form



# Two-stage least squares



# Characterizing the compliers ----
# Compare `moreths` of population to compliers


# Compare share of White/Black/Mother of population to compliers




# Multiple instruments ----
# Z_i `samesex` and `twins_2`
# D_i `morekids`
# y_i `mom_weeks_worked`




# Do we have a strong first-stage?


