``` r
library(tidyverse) # data-wrangling
library(fixest) # all-things regression
library(haven) # load stata data
```

NSW experimental treatment and experimental control group

``` r
lalonde <- haven::read_dta("Labs/01_Potential_Outcomes/data/nsw.dta")
```

Non-experimental control group

``` r
cps <- haven::read_dta("Labs/01_Potential_Outcomes/data/cps_controls.dta")
```

Here we are taking the *treated* units from the NSW experiment and
appending the Current Population Survey (CPS) control group. The
CPS is representative of the population as a whole, while the NSW is a
*highly* selected experimental control group

``` r
nonexperimental <- bind_rows(
  lalonde |> filter(treat == 1),
  cps
)
```

The variables are:
- `re75` Real-earnings in 1975 (before the training)
- `re78` Real-earnings in 1978 (after the training)
- `treat` Indicator variable for whether the worker entered the works
training program
- `age` worker’s age
- `education` Years of schooling
- `black`/`hispanic` Indicator variable for whether the
worker is Black/Hispanic
- `married` Indicator variable for whether the worker is married
- `nodegree` Indicator variable for whether the worker has no
high school degree

``` r
lalonde
#> # A tibble: 722 × 10
#>    data_id    treat   age education black hispanic married nodegree  re75   re78
#>    <chr>      <dbl> <dbl>     <dbl> <dbl>    <dbl>   <dbl>    <dbl> <dbl>  <dbl>
#>  1 Lalonde S…     1    37        11     1        0       1        1     0  9930.
#>  2 Lalonde S…     1    22         9     0        1       0        1     0  3596.
#>  3 Lalonde S…     1    30        12     1        0       0        0     0 24909.
#>  4 Lalonde S…     1    27        11     1        0       0        1     0  7506.
#>  5 Lalonde S…     1    33         8     1        0       0        1     0   290.
#>  6 Lalonde S…     1    22         9     1        0       0        1     0  4056.
#>  7 Lalonde S…     1    23        12     1        0       0        0     0     0 
#>  8 Lalonde S…     1    32        11     1        0       0        1     0  8472.
#>  9 Lalonde S…     1    22        16     1        0       0        0     0  2164.
#> 10 Lalonde S…     1    33        12     0        0       1        0     0 12418.
#> # ℹ 712 more rows
```

## Experimental Analysis

Difference-in-means estimator (by hand)

``` r
treated_group_mean <- mean(lalonde$re78[lalonde$treat == 1])
control_group_mean <- mean(lalonde$re78[lalonde$treat == 0])
dim <- treated_group_mean - control_group_mean
```

For the experimental population (where treatment is randomly assigned),
we have the difference-in-means estimator is consistent for the
ATE = ATT = ATC

``` r
dim
#> [1] 886.3037
```

The variance of the difference-in-means estimator

``` r
var <-
  1 / sum(lalonde$treat == 1) * var(lalonde$re78[lalonde$treat == 1]) +
  1 / sum(lalonde$treat == 0) * var(lalonde$re78[lalonde$treat == 0])
```

Standard error of the difference-in-means estimator

``` r
sqrt(var)
#> [1] 488.2045
```

Difference-in-means and correct standard error (HC2)

``` r
feols(
  re78 ~ 1 + i(treat, ref = 0),
  data = lalonde,
  vcov = function(x) sandwich::vcovHC(x, type = "HC2")
)
#> OLS estimation, Dep. Var.: re78
#> Observations: 722
#> Standard-errors: vcovHC(x, type = "HC2") 
#>             Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 5090.048    277.368 18.35124 < 2.2e-16 ***
#> treat::1     886.304    488.205  1.81544  0.069873 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 6,233.4   Adj. R2: 0.003489
```

The HC1 standard error will do fine except under small sample sizes

``` r
feols(
  re78 ~ 1 + i(treat, ref = 0),
  data = lalonde, vcov = "HC1"
)
#> OLS estimation, Dep. Var.: re78
#> Observations: 722
#> Standard-errors: Heteroskedasticity-robust 
#>             Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 5090.048    277.426 18.34741 < 2.2e-16 ***
#> treat::1     886.304    488.139  1.81568  0.069835 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 6,233.4   Adj. R2: 0.003489
```

Note that the regression intercept is our control group mean

``` r
control_group_mean
#> [1] 5090.048
```

And the coefficient on the treatment indicator is the difference-in-means
estimate

``` r
treated_group_mean - control_group_mean
#> [1] 886.3037
```

In experiments, it is common to check that the difference in
baseline covaraites are on-average the same between the treated and control
group. They theoretically should be in large samples when treatment is
randomized, but finite samples could have imbalances.

When imbalances are present, we can control for them in the regression (more
on this in topic 3)

``` r
balance_check <- feols(
  c(age, education, married, black, hispanic, nodegree) ~
    i(treat, ref = 0),
  data = lalonde,
  vcov = "HC1"
)

etable(balance_check)
#>                   balance_check.1   balance_check.2    balance_check.3
#> Dependent Var.:               age         education            married
#>                                                                       
#> Constant        24.45*** (0.3197) 10.19*** (0.0785) 0.1576*** (0.0177)
#> treat = 1         0.1792 (0.5027)   0.1922 (0.1315)    0.0107 (0.0280)
#> _______________ _________________ _________________ __________________
#> S.E. type       Heteroskeda.-rob. Heteroskeda.-rob. Heteroskedas.-rob.
#> Observations                  722               722                722
#> R2                        0.00018           0.00308            0.00020
#> Adj. R2                  -0.00121           0.00170           -0.00118
#> 
#>                    balance_check.4    balance_check.5    balance_check.6
#> Dependent Var.:              black           hispanic           nodegree
#>                                                                         
#> Constant        0.8000*** (0.0194) 0.1129*** (0.0154) 0.8141*** (0.0189)
#> treat = 1          0.0013 (0.0302)   -0.0187 (0.0229) -0.0835** (0.0320)
#> _______________ __________________ __________________ __________________
#> S.E. type       Heteroskedas.-rob. Heteroskedas.-rob. Heteroskedas.-rob.
#> Observations                   722                722                722
#> R2                         2.75e-6            0.00090            0.00983
#> Adj. R2                   -0.00139           -0.00049            0.00845
#> ---
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

When we have an indicator variable on the left-hand side
the mean tells you the proportion of individuals with that variable = 1.

``` r
feols(
  married ~ i(treat, ref = 0),
  data = lalonde
)
#> OLS estimation, Dep. Var.: married
#> Observations: 722
#> Standard-errors: IID 
#>             Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 0.157647   0.017898 8.808236 < 2.2e-16 ***
#> treat::1    0.010703   0.027905 0.383551   0.70142    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 0.368459   Adj. R2: -0.001184
```

Proporition of control group that is married

``` r
mean(lalonde$married[lalonde$treat == 0])
#> [1] 0.1576471
```

16.2% of workers in the experiment are married

``` r
mean(lalonde$married)
#> [1] 0.1620499
```

## Non-experimental Analysis

Difference-in-means is really biased from selection-bias. That is,
the CPS population probably has a much larger Y_i(0) on average than the
NSW treatment group (who were drawn from workers without high school
degrees, ex-drug addicts, and ex-convicts).

``` r
feols(
  re78 ~ i(treat, ref = 0),
  data = nonexperimental, vcov = "HC1"
)
#> OLS estimation, Dep. Var.: re78
#> Observations: 16,289
#> Standard-errors: Heteroskedasticity-robust 
#>             Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 14846.66    76.2907 194.6064 < 2.2e-16 ***
#> treat::1    -8870.31   408.2979 -21.7251 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 9,604.2   Adj. R2: 0.014979
```

Trying to absorb differences in Y_i(0)

``` r
feols(
  re78 ~ i(treat, ref = 0) + i(black) + i(hispanic)
    + age + I(age^2) + education + I(education^2) + i(nodegree) + i(married),
  data = nonexperimental, vcov = "HC1"
)
#> OLS estimation, Dep. Var.: re78
#> Observations: 16,289
#> Standard-errors: Heteroskedasticity-robust 
#>                  Estimate  Std. Error   t value   Pr(>|t|)    
#> (Intercept)    -8619.7728 1200.534330  -7.17995 7.2751e-13 ***
#> treat::1       -3601.0937  459.689387  -7.83375 5.0257e-15 ***
#> black::1       -2307.9768  266.020554  -8.67593  < 2.2e-16 ***
#> hispanic::1     -927.1411  274.471762  -3.37791 7.3210e-04 ***
#> age              956.8941   50.384063  18.99200  < 2.2e-16 ***
#> I(age^2)         -12.3591    0.705170 -17.52635  < 2.2e-16 ***
#> education        644.1112  122.515503   5.25739 1.4796e-07 ***
#> I(education^2)   -17.5853    5.095655  -3.45103 5.5986e-04 ***
#> nodegree::1     -895.8247  224.892424  -3.98335 6.8247e-05 ***
#> married::1      3186.2199  191.494090  16.63874  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 9,069.9   Adj. R2: 0.121091
```

We also see balance checks failing for the nonexperimental dataset

``` r
balance_check_nonexp <- feols(
  c(age, education, married, black, hispanic, nodegree) ~
    i(treat, ref = 0),
  data = nonexperimental,
  vcov = "HC1"
)

etable(balance_check_nonexp)
#>                 balance_check_n..1 balance_check_n..2 balance_check_no..3
#> Dependent Var.:                age          education             married
#>                                                                          
#> Constant         33.23*** (0.0873)  12.03*** (0.0227)  0.7117*** (0.0036)
#> treat = 1       -8.599*** (0.3971) -1.647*** (0.1077) -0.5434*** (0.0220)
#> _______________ __________________ __________________ ___________________
#> S.E. type       Heteroskedas.-rob. Heteroskedas.-rob. Heteroskedast.-rob.
#> Observations                16,289             16,289              16,289
#> R2                         0.01086            0.00592             0.02526
#> Adj. R2                    0.01080            0.00586             0.02520
#> 
#>                 balance_check_n..4 balance_check_n..5 balance_check_n..6
#> Dependent Var.:              black           hispanic           nodegree
#>                                                                         
#> Constant        0.0735*** (0.0021) 0.0720*** (0.0020) 0.2958*** (0.0036)
#> treat = 1       0.7278*** (0.0232)    0.0222 (0.0171) 0.4348*** (0.0260)
#> _______________ __________________ __________________ __________________
#> S.E. type       Heteroskedas.-rob. Heteroskedas.-rob. Heteroskedas.-rob.
#> Observations                16,289             16,289             16,289
#> R2                         0.11962            0.00013            0.01600
#> Adj. R2                    0.11956            7.04e-5            0.01594
#> ---
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

<sup>Created on 2024-08-26 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
