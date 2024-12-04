library(fixest)
library(did2s)
data(df_het, package = "did2s")

# First-stage model
fs <- feols(
  dep_var ~ 0 | unit + year,
  data = df_het[df_het$treat == 0, ] # d_{it} = 0
)

# Impute \hat{y}_{it}(\infty)
df_het$y0_hat <- predict(fs, newdata = df_het)
df_het$y_resid <- df_het$dep_var - df_het$y0_hat
#                      y_{it}    - \hat{y}_{it}(\infty)

# Second-stage model
ss <- feols(
  y_resid ~ 0 + i(rel_year, ref = Inf),
  data = df_het
)
coefplot(ss)


# did2s
est_did2s <- did2s(
  data = df_het, 
  yname = "dep_var",
  first_stage  = ~ 0 | unit + year,
  second_stage = ~ 0 + i(rel_year, ref = Inf),
  treatment = "treat", 
  cluster_var = "unit"
)

coefplot(
  list(ss, est_did2s)
)




