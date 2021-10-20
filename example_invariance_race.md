## Load Packages

``` r
library(tidyverse)
library(here)
library(lavaan)
library(modelsummary)
```

## Data Import

Alcohol use data from the 2014 National Survey on Drug Use and Health
(NSDUH)

``` r
sud_dat <- haven::read_sav(here("data", "ALC_MJ_depabuse_race.sav"), 
                           user_na = TRUE)
```

## Recode the DSM AUD Items

``` r
sud_alc <- sud_dat %>% 
  filter(!ALCLOTTM %in% c(83, 91, 93)) %>%
  mutate(alc1 = case_when(
    ALCLOTTM == 1 ~ 1, 
    ALCGTOVR == 1 ~ 1, 
    ALCLOTTM == 2 ~ 0, 
    ALCGTOVR == 2 ~ 0, 
    ALCGTOVR == 99 ~ 0
    ), 
    alc2 = case_when(
      ALCKPLMT == 2 ~ 1, 
      (ALCKPLMT == 1 | ALCKPLMT == 99) ~ 0
    ), 
    alc3 = case_when(
      ALCNDMOR == 1 ~ 1, 
      ALCLSEFX == 1 ~ 1, 
      ALCNDMOR == 2 ~ 0, 
      ALCLSEFX == 2 ~ 0, 
      ALCLSEFX == 99 ~ 0
    ), 
    alc4 = case_when(
      ALCCUTEV == 2 ~ 1, 
      (ALCCUTEV == 1 | ALCCUTEV == 99) ~ 0
    ), 
    alc5 = case_when(
      ALCEMCTD == 1 ~ 1,
      ALCPHCTD == 1 ~ 1, 
      (ALCEMCTD == 2 | ALCEMCTD == 99) ~ 0, 
      (ALCPHCTD == 2 | ALCPHCTD == 99) ~ 0
    ), 
    alc6 = case_when(
      ALCLSACT == 1 ~ 1, 
      ALCLSACT == 2 ~ 0
    ), 
    alc7 = case_when(
      ALCWDSMT == 1 ~ 1, 
      (ALCWDSMT == 2 | ALCWDSMT == 99) ~ 0
    )) %>% select(IRSEX, CATAGE, ALCLOTTM:ALCFMCTD, 
                  alc1:alc7, DEPNDALC, NEWRACE2) %>% 
  mutate(alc_dep = rowSums(select(., alc1:alc7)))
```

## Racial Distribution

``` r
sud_alc$race <- as_factor(sud_alc$NEWRACE2)
# Sort the data by Race
sud_alc <- arrange(sud_alc, race)
count(sud_alc, race)
```

    ## # A tibble: 7 × 2
    ##   race                                n
    ##   <fct>                           <int>
    ## 1 NonHisp White                   19120
    ## 2 NonHisp Black/Afr Am             3113
    ## 3 NonHisp Native Am/AK Native       426
    ## 4 NonHisp Native HI/Other Pac Isl   126
    ## 5 NonHisp Asian                     950
    ## 6 NonHisp more than one race        906
    ## 7 Hispanic                         4281

# Factorial Invariance

## Configural Invariance

Two pairs of unique covariances (items 1 and 3, and items 2 and 4) were
included, based on preliminary model fit.

``` r
cfa_mod <- 'f =~ alc1 + alc2 + alc3 + alc4 + alc5 + alc6 + alc7
            alc1 ~~ 1 * alc1
            alc2 ~~ 1 * alc2
            alc3 ~~ 1 * alc3
            alc4 ~~ 1 * alc4
            alc5 ~~ 1 * alc5
            alc6 ~~ 1 * alc6
            alc7 ~~ 1 * alc7
            alc1 ~~ alc3
            alc2 ~~ alc4'
m1 <- cfa(cfa_mod, 
          data = sud_alc, 
          ordered = paste0("alc", 1:7),
          group = "race", parameterization = "theta")
summary(m1, fit.measures = TRUE, estimates = FALSE)
```

    ## lavaan 0.6-9 ended normally after 708 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                       112
    ##                                                       
    ##   Number of observations per group:               Used       Total
    ##     NonHisp White                                18853       19120
    ##     NonHisp Black/Afr Am                          3029        3113
    ##     NonHisp Native Am/AK Native                    415         426
    ##     NonHisp Native HI/Other Pac Isl                123         126
    ##     NonHisp Asian                                  922         950
    ##     NonHisp more than one race                     878         906
    ##     Hispanic                                      4113        4281
    ##                                                                   
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                               104.013     157.966
    ##   Degrees of freedom                                84          84
    ##   P-value (Chi-square)                           0.069       0.000
    ##   Scaling correction factor                                  0.701
    ##   Shift parameter for each group:                                 
    ##       NonHisp White                                          6.416
    ##       NonHisp Black/Afr Am                                   1.031
    ##       NonHisp Native Am/AK Native                            0.141
    ##       NonHisp Native HI/Other Pac Isl                        0.042
    ##       NonHisp Asian                                          0.314
    ##       NonHisp more than one race                             0.299
    ##       Hispanic                                               1.400
    ##        simple second-order correction                             
    ##   Test statistic for each group:
    ##     NonHisp White                               33.327      53.941
    ##     NonHisp Black/Afr Am                        18.676      27.663
    ##     NonHisp Native Am/AK Native                  6.212       8.999
    ##     NonHisp Native HI/Other Pac Isl              4.077       5.856
    ##     NonHisp Asian                               12.103      17.573
    ##     NonHisp more than one race                   9.085      13.254
    ##     Hispanic                                    20.533      30.680
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             40133.503   29334.472
    ##   Degrees of freedom                               147         147
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.370
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.999       0.997
    ##   Tucker-Lewis Index (TLI)                       0.999       0.996
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.008       0.015
    ##   90 Percent confidence interval - lower         0.000       0.011
    ##   90 Percent confidence interval - upper         0.012       0.018
    ##   P-value RMSEA <= 0.05                          1.000       1.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.028       0.028

The model fit was reasonable, so we did not reject configural
invariance.

## Metric Invariance

``` r
m2 <- update(m1, group.equal = "loadings")
# Model Comparison
anova(m1, m2)
```

    ## # A tibble: 2 × 7
    ##      Df AIC   BIC   Chisq `Chisq diff` `Df diff` `Pr(>Chisq)`
    ##   <int> <lgl> <lgl> <dbl>        <dbl>     <int>        <dbl>
    ## 1    84 NA    NA     104.         NA          NA       NA    
    ## 2   120 NA    NA     163.         37.1        36        0.418

The $\Delta \chi^2$ test was not significant, so we did not reject
metric invariance.

## Scalar Invariance

``` r
m3 <- update(m1, group.equal = c("loadings", "intercepts"))
# Model Comparison
anova(m2, m3)
```

    ## # A tibble: 2 × 7
    ##      Df AIC   BIC   Chisq `Chisq diff` `Df diff` `Pr(>Chisq)`
    ##   <int> <lgl> <lgl> <dbl>        <dbl>     <int>        <dbl>
    ## 1   120 NA    NA     163.         NA          NA     NA      
    ## 2   156 NA    NA     357.         64.0        36      0.00278

The $\Delta \chi^2$ test was significant, so we rejected scalar
invariance. The next step is to identify which item was not invariant.
We will use modification indices.

``` r
modindices(m3, sort. = TRUE, free.remove = FALSE, alpha = 0.05 / 7)
```

    ## # A tibble: 405 × 11
    ##    lhs   op    rhs    block group level    mi     epc sepc.lv sepc.all sepc.nox
    ##    <chr> <chr> <chr>  <int> <int> <int> <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
    ##  1 alc3  ~1    ""         1     1     1  74.3 -0.232  -0.232    -0.175   -0.175
    ##  2 alc3  ~*~   "alc3"     1     1     1  74.3  0.138   0.138     1        1    
    ##  3 alc5  ~~    "alc5"     1     1     1  54.4  0.685   1         0.271    0.271
    ##  4 alc5  ~1    ""         1     1     1  49.9  0.380   0.380     0.198    0.198
    ##  5 alc5  ~*~   "alc5"     1     1     1  49.9 -0.0623 -0.0623   -1       -1    
    ##  6 alc3  ~~    "alc3"     1     1     1  37.0 -0.413  -1        -0.571   -0.571
    ##  7 alc3  ~*~   "alc3"     2     2     1  33.4 -0.159  -0.159    -1       -1    
    ##  8 alc3  ~1    ""         2     2     1  33.4  0.233   0.233     0.178    0.178
    ##  9 alc4  ~1    ""         1     1     1  26.3  0.196   0.196     0.151    0.151
    ## 10 alc4  ~*~   "alc4"     1     1     1  26.3 -0.0651 -0.0651   -1       -1    
    ## # … with 395 more rows

This suggests that the threshold for item 3 was not invariant for Group
2 (Non-Hispanic White). We fitted a partial invariance model freeing
that constraint.

``` r
cfa_mod2 <- 'f =~ alc1 + alc2 + alc3 + alc4 + alc5 + alc6 + alc7
             alc1 ~~ 1 * alc1
             alc2 ~~ 1 * alc2
             alc3 ~~ 1 * alc3
             alc4 ~~ 1 * alc4
             alc5 ~~ 1 * alc5
             alc6 ~~ 1 * alc6
             alc7 ~~ 1 * alc7
             alc3 | c(i31, i3, i3, i3, i3, i3, i3) * t1
             alc1 ~~ alc3
             alc2 ~~ alc4'
m3p1 <- update(m1, model = cfa_mod2, group.equal = c("loadings", "intercepts"))
anova(m3p1, m2)  # significant
```

    ## # A tibble: 2 × 7
    ##      Df AIC   BIC   Chisq `Chisq diff` `Df diff` `Pr(>Chisq)`
    ##   <int> <lgl> <lgl> <dbl>        <dbl>     <int>        <dbl>
    ## 1   120 NA    NA     163.         NA          NA      NA     
    ## 2   155 NA    NA     283.         50.4        35       0.0440

``` r
modindices(m3p1, sort. = TRUE, free.remove = FALSE, alpha = 0.01 / 7)
```

    ## # A tibble: 403 × 11
    ##    lhs   op    rhs    block group level    mi     epc sepc.lv sepc.all sepc.nox
    ##    <chr> <chr> <chr>  <int> <int> <int> <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
    ##  1 alc5  ~*~   "alc5"     1     1     1  32.0 -0.0519 -0.0519   -1       -1    
    ##  2 alc5  ~1    ""         1     1     1  32.0  0.311   0.311     0.163    0.163
    ##  3 alc5  ~~    "alc5"     1     1     1  31.6  0.540   1         0.274    0.274
    ##  4 alc7  ~~    "alc7"     1     1     1  26.9 -0.374  -1        -0.391   -0.391
    ##  5 alc7  ~*~   "alc7"     1     1     1  21.1  0.0490  0.0490    1        1    
    ##  6 alc7  ~1    ""         1     1     1  21.1 -0.246  -0.246    -0.154   -0.154
    ##  7 alc5  ~*~   "alc5"     7     7     1  18.6  0.0613  0.0613    1        1    
    ##  8 alc5  ~1    ""         7     7     1  18.6 -0.310  -0.310    -0.171   -0.171
    ##  9 alc4  ~*~   "alc4"     1     1     1  16.6 -0.0528 -0.0528   -1       -1    
    ## 10 alc4  ~1    ""         1     1     1  16.6  0.157   0.157     0.122    0.122
    ## # … with 393 more rows

Continuing searching for non-invariant items, we found the threshold for
item 5 was not invariant for, again, Group 2 (Non-Hispanic White).

``` r
cfa_mod3 <- 'f =~ alc1 + alc2 + alc3 + alc4 + alc5 + alc6 + alc7
             alc1 ~~ 1 * alc1
             alc2 ~~ 1 * alc2
             alc3 ~~ 1 * alc3
             alc4 ~~ 1 * alc4
             alc5 ~~ 1 * alc5
             alc6 ~~ 1 * alc6
             alc7 ~~ 1 * alc7
             alc3 | c(i31, i3, i3, i3, i3, i3, i3) * t1
             alc5 | c(i51, i5, i5, i5, i5, i5, i5) * t1
             alc1 ~~ alc3
             alc2 ~~ alc4'
m3p2 <- update(m1, model = cfa_mod3, group.equal = c("loadings", "intercepts"))
anova(m3p2, m2)  # not significant
```

    ## # A tibble: 2 × 7
    ##      Df AIC   BIC   Chisq `Chisq diff` `Df diff` `Pr(>Chisq)`
    ##   <int> <lgl> <lgl> <dbl>        <dbl>     <int>        <dbl>
    ## 1   120 NA    NA     163.         NA          NA       NA    
    ## 2   154 NA    NA     251.         44.1        34        0.114

## (Partial) Strict Invariance

``` r
m4 <- update(m1, model = cfa_mod3, 
             group.equal = c("loadings", "intercepts", "residuals",
                             "residual.covariances"))
anova(m4, m3p2)  # not significant
```

    ## # A tibble: 2 × 7
    ##      Df AIC   BIC   Chisq `Chisq diff` `Df diff` `Pr(>Chisq)`
    ##   <int> <lgl> <lgl> <dbl>        <dbl>     <int>        <dbl>
    ## 1   154 NA    NA     251.         NA          NA       NA    
    ## 2   166 NA    NA     273.         13.4        12        0.341

## Difference in Thresholds

``` r
msummary(m4, group = term ~ model + group, output = "markdown")
```

|                  | Model 1 / 1.000 | Model 1 / 2.000 | Model 1 / 3.000 | Model 1 / 4.000 | Model 1 / 5.000 | Model 1 / 6.000 | Model 1 / 7.000 |
|:-----------------|:---------------:|:---------------:|:---------------:|:---------------:|:---------------:|:---------------:|:---------------:|
| f =\~ alc1       |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| f =\~ alc2       |      0.987      |      0.987      |      0.987      |      0.987      |      0.987      |      0.987      |      0.987      |
|                  |     (0.044)     |     (0.044)     |     (0.044)     |     (0.044)     |     (0.044)     |     (0.044)     |     (0.044)     |
| f =\~ alc3       |      0.719      |      0.719      |      0.719      |      0.719      |      0.719      |      0.719      |      0.719      |
|                  |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |
| f =\~ alc4       |      0.699      |      0.699      |      0.699      |      0.699      |      0.699      |      0.699      |      0.699      |
|                  |     (0.032)     |     (0.032)     |     (0.032)     |     (0.032)     |     (0.032)     |     (0.032)     |     (0.032)     |
| f =\~ alc5       |      1.394      |      1.394      |      1.394      |      1.394      |      1.394      |      1.394      |      1.394      |
|                  |     (0.063)     |     (0.063)     |     (0.063)     |     (0.063)     |     (0.063)     |     (0.063)     |     (0.063)     |
| f =\~ alc6       |      1.294      |      1.294      |      1.294      |      1.294      |      1.294      |      1.294      |      1.294      |
|                  |     (0.060)     |     (0.060)     |     (0.060)     |     (0.060)     |     (0.060)     |     (0.060)     |     (0.060)     |
| f =\~ alc7       |      1.062      |      1.062      |      1.062      |      1.062      |      1.062      |      1.062      |      1.062      |
|                  |     (0.052)     |     (0.052)     |     (0.052)     |     (0.052)     |     (0.052)     |     (0.052)     |     (0.052)     |
| alc1 \~\~ alc1   |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc2 \~\~ alc2   |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc3 \~\~ alc3   |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc4 \~\~ alc4   |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc5 \~\~ alc5   |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc6 \~\~ alc6   |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc7 \~\~ alc7   |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |      1.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc3 \| t1       |      1.319      |      1.119      |      1.119      |      1.119      |      1.119      |      1.119      |      1.119      |
|                  |     (0.021)     |     (0.028)     |     (0.028)     |     (0.028)     |     (0.028)     |     (0.028)     |     (0.028)     |
| alc5 \| t1       |      3.063      |      3.371      |      3.371      |      3.371      |      3.371      |      3.371      |      3.371      |
|                  |     (0.085)     |     (0.096)     |     (0.096)     |     (0.096)     |     (0.096)     |     (0.096)     |     (0.096)     |
| alc1 \~\~ alc3   |      0.180      |      0.180      |      0.180      |      0.180      |      0.180      |      0.180      |      0.180      |
|                  |     (0.020)     |     (0.020)     |     (0.020)     |     (0.020)     |     (0.020)     |     (0.020)     |     (0.020)     |
| alc2 \~\~ alc4   |      0.476      |      0.476      |      0.476      |      0.476      |      0.476      |      0.476      |      0.476      |
|                  |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |     (0.024)     |
| alc1 \| t1       |      1.453      |      1.453      |      1.453      |      1.453      |      1.453      |      1.453      |      1.453      |
|                  |     (0.029)     |     (0.029)     |     (0.029)     |     (0.029)     |     (0.029)     |     (0.029)     |     (0.029)     |
| alc2 \| t1       |      2.628      |      2.628      |      2.628      |      2.628      |      2.628      |      2.628      |      2.628      |
|                  |     (0.054)     |     (0.054)     |     (0.054)     |     (0.054)     |     (0.054)     |     (0.054)     |     (0.054)     |
| alc4 \| t1       |      2.310      |      2.310      |      2.310      |      2.310      |      2.310      |      2.310      |      2.310      |
|                  |     (0.037)     |     (0.037)     |     (0.037)     |     (0.037)     |     (0.037)     |     (0.037)     |     (0.037)     |
| alc6 \| t1       |      3.191      |      3.191      |      3.191      |      3.191      |      3.191      |      3.191      |      3.191      |
|                  |     (0.086)     |     (0.086)     |     (0.086)     |     (0.086)     |     (0.086)     |     (0.086)     |     (0.086)     |
| alc7 \| t1       |      3.157      |      3.157      |      3.157      |      3.157      |      3.157      |      3.157      |      3.157      |
|                  |     (0.081)     |     (0.081)     |     (0.081)     |     (0.081)     |     (0.081)     |     (0.081)     |     (0.081)     |
| f \~\~ f         |      1.382      |      1.362      |      2.015      |      1.160      |      1.219      |      1.643      |      1.180      |
|                  |     (0.078)     |     (0.119)     |     (0.306)     |     (0.305)     |     (0.153)     |     (0.247)     |     (0.090)     |
| alc3 \~\*\~ alc3 |      0.764      |      0.766      |      0.700      |      0.791      |      0.783      |      0.735      |      0.788      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc5 \~\*\~ alc5 |      0.521      |      0.524      |      0.451      |      0.554      |      0.545      |      0.488      |      0.551      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc1 \~\*\~ alc1 |      0.648      |      0.651      |      0.576      |      0.680      |      0.671      |      0.615      |      0.677      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc2 \~\*\~ alc2 |      0.653      |      0.656      |      0.581      |      0.685      |      0.676      |      0.620      |      0.682      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc4 \~\*\~ alc4 |      0.773      |      0.775      |      0.710      |      0.799      |      0.792      |      0.745      |      0.796      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc6 \~\*\~ alc6 |      0.549      |      0.552      |      0.478      |      0.583      |      0.573      |      0.516      |      0.580      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc7 \~\*\~ alc7 |      0.625      |      0.628      |      0.553      |      0.658      |      0.649      |      0.592      |      0.655      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc1 \~1         |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc2 \~1         |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc3 \~1         |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc4 \~1         |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc5 \~1         |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc6 \~1         |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| alc7 \~1         |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |      0.000      |
|                  |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |     (0.000)     |
| f \~1            |      0.000      |      0.151      |      0.696      |      0.561      |      0.141      |      0.140      |      0.287      |
|                  |     (0.000)     |     (0.051)     |     (0.111)     |     (0.167)     |     (0.079)     |     (0.094)     |     (0.043)     |

The results suggested that for item 3, Non-Hispanic White participants
tended to score lower, whereas for item 5, Non-Hispanic White
participants tended to score higher

## Fit Indices

``` r
fit_tab <- lapply(
  list("Configural" = m1,
       "Metric" = m2,
       "Scalar" = m3,
       "Partial Scalar 1" = m3p1,
       "Partial Scalar 2" = m3p2,
       "Partial Strict" = m4),
  fitmeasures,
  fit.measures = c(
    "chisq", "df", "rmsea",
    "rmsea.ci.lower", "rmsea.ci.upper",
    "cfi", "tli", "srmr"
  )
)
fit_tab <- do.call(rbind, fit_tab)
fit_tab
```

    ##                     chisq  df       rmsea rmsea.ci.lower rmsea.ci.upper
    ## Configural       104.0132  84 0.007673168    0.000000000     0.01213652
    ## Metric           163.0511 120 0.009415818    0.005298939     0.01287813
    ## Scalar           357.0628 156 0.017846783    0.015411254     0.02029159
    ## Partial Scalar 1 282.7512 155 0.014271615    0.011606609     0.01688609
    ## Partial Scalar 2 250.7769 154 0.012461829    0.009590640     0.01520884
    ## Partial Strict   272.6067 166 0.012597797    0.009853268     0.01523452
    ##                        cfi       tli       srmr
    ## Configural       0.9994995 0.9991241 0.02832143
    ## Metric           0.9989234 0.9986811 0.03302693
    ## Scalar           0.9949717 0.9952618 0.03279054
    ## Partial Scalar 1 0.9968051 0.9969700 0.03289688
    ## Partial Scalar 2 0.9975798 0.9976898 0.03303937
    ## Partial Strict   0.9973339 0.9976391 0.03439892

# Mean Comparisons Adjusting for Non-Invariance

``` r
names_lmeans <- grep("f~1", names(coef(m4)), value = TRUE)
names_lvars <- grep("f~~f", names(coef(m4)), value = TRUE)
lmeans <- c(0, coef(m4)[names_lmeans])
lvars <- coef(m4)[names_lvars]
nobs_m4 <- lavInspect(m4, "nobs")
grand_var <- weighted.mean(lvars, w = nobs_m4)
grand_mean <- weighted.mean(lmeans, w = nobs_m4)
# Standardized Mean Difference
sud_alc |>
  group_by(race) |>
  summarise(`# symptoms` = mean(alc_dep, na.rm = TRUE)) |>
  mutate(`latent means` = (lmeans - grand_mean) / sqrt(grand_var))
```

    ## # A tibble: 7 × 3
    ##   race                            `# symptoms` `latent means`
    ##   <fct>                                  <dbl>          <dbl>
    ## 1 NonHisp White                          0.530        -0.0681
    ## 2 NonHisp Black/Afr Am                   0.643         0.0615
    ## 3 NonHisp Native Am/AK Native            1.25          0.528 
    ## 4 NonHisp Native HI/Other Pac Isl        0.894         0.413 
    ## 5 NonHisp Asian                          0.593         0.0525
    ## 6 NonHisp more than one race             0.704         0.0516
    ## 7 Hispanic                               0.680         0.178

The latent means adjusted for both the violations of invariance, as well
as that some items are indicators of more severe AUD.
