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

# %%
#' The variables are:
#' - `re75` Real-earnings in 1975 (before the training)
#' - `re78` Real-earnings in 1978 (after the training)
#' - `treat` Indicator variable for whether the worker entered the works
#'   training program
#' - `age` worker's age
#' - `education` Years of schooling
#' - `black`/`hispanic` Indicator variable for whether the
#'   worker is Black/Hispanic
#' - `married` Indicator variable for whether the worker is married
#' - `nodegree` Indicator variable for whether the worker has no
#'   high school degree
lalonde

#' ## Experimental Analysis
# %%
#' Difference-in-means estimator (by hand)
treated_group_mean <- mean(lalonde$re78[lalonde$treat == 1])
control_group_mean <- mean(lalonde$re78[lalonde$treat == 0])
dim <- treated_group_mean - control_group_mean

#' For the experimental population (where treatment is randomly assigned),
#' we have the difference-in-means estimator is consistent for the
#' ATE = ATT = ATC
dim

#' The variance of the difference-in-means estimator
var <-
  1 / sum(lalonde$treat == 1) * var(lalonde$re78[lalonde$treat == 1]) +
  1 / sum(lalonde$treat == 0) * var(lalonde$re78[lalonde$treat == 0])

#' Standard error of the difference-in-means estimator
sqrt(var)

# %%
#' Difference-in-means and correct standard error (HC2)
feols(
  re78 ~ 1 + i(treat, ref = 0),
  data = lalonde,
  vcov = function(x) sandwich::vcovHC(x, type = "HC2")
)

#' The HC1 standard error will do fine except under small sample sizes
feols(
  re78 ~ 1 + i(treat, ref = 0),
  data = lalonde, vcov = "HC1"
)

# %%
#' Note that the regression intercept is our control group mean
control_group_mean
#' And the coefficient on the treatment indicator is the difference-in-means
#' estimate
treated_group_mean - control_group_mean

# %%
#' In experiments, it is common to check that the difference in
#' baseline covaraites are on-average the same between the treated and control
#' group. They theoretically should be in large samples when treatment is
#' randomized, but finite samples could have imbalances.
#'
#' When imbalances are present, we can control for them in the regression (more
#' on this in topic 3)
#'
balance_check <- feols(
  c(age, education, married, black, hispanic, nodegree) ~
    i(treat, ref = 0),
  data = lalonde,
  vcov = "HC1"
)

etable(balance_check)

# %%
#' When we have an indicator variable on the left-hand side
#' the mean tells you the proportion of individuals with that variable = 1.
feols(
  married ~ i(treat, ref = 0),
  data = lalonde
)

#' Proporition of control group that is married
mean(lalonde$married[lalonde$treat == 0])

#' 16.2% of workers in the experiment are married
mean(lalonde$married)


#' ## Non-experimental Analysis
# %%
#' Difference-in-means is really biased from selection-bias. That is,
#' the CPS population probably has a much larger Y_i(0) on average than the
#' NSW treatment group (who were drawn from workers without high school
#' degrees, ex-drug addicts, and ex-convicts).
#'
feols(
  re78 ~ i(treat, ref = 0),
  data = nonexperimental, vcov = "HC1"
)

#' Trying to absorb differences in Y_i(0)
feols(
  re78 ~ i(treat, ref = 0) + i(black) + i(hispanic)
    + age + I(age^2) + education + I(education^2) + i(nodegree) + i(married),
  data = nonexperimental, vcov = "HC1"
)

# %%
#' We also see balance checks failing for the nonexperimental dataset
balance_check_nonexp <- feols(
  c(age, education, married, black, hispanic, nodegree) ~
    i(treat, ref = 0),
  data = nonexperimental,
  vcov = "HC1"
)

etable(balance_check_nonexp)
