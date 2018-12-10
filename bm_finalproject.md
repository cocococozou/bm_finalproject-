bm\_finalproject
================
Coco
12/7/2018

Read data
---------

stepwise
--------

``` r
full_model = lm(target_death_rate ~ ., data = num_df)
summary(full_model)

step_model <- stepAIC(full_model, direction = "both", 
                      trace = FALSE)
summary(step_model)
vif(step_model)
step_model<- update(step_model,.~.-avg_deaths_per_year)
step_model<- update(step_model,.~.-pop_est2015)
step_model<- update(step_model,.~.+black_high_ind*incidence_rate)
```

``` r
summary(step_model)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + percent_married + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + birth_rate + black_high_ind + incidence_rate:black_high_ind, 
    ##     data = num_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -90.191 -11.355  -0.401  10.833 136.807 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.209e+02  9.495e+00  12.728  < 2e-16 ***
    ## avg_ann_count                 -1.006e-03  3.064e-04  -3.285 0.001037 ** 
    ## incidence_rate                 1.957e-01  9.906e-03  19.755  < 2e-16 ***
    ## poverty_percent                3.592e-01  1.358e-01   2.644 0.008254 ** 
    ## percent_married               -2.428e-01  1.011e-01  -2.401 0.016435 *  
    ## pct_bach_deg25_over           -1.892e+00  1.058e-01 -17.884  < 2e-16 ***
    ## pct_unemployed16_over          6.693e-01  1.848e-01   3.622 0.000299 ***
    ## birth_rate                    -7.613e-01  2.181e-01  -3.491 0.000491 ***
    ## black_high_ind                 2.753e+01  9.161e+00   3.004 0.002692 ** 
    ## incidence_rate:black_high_ind -5.172e-02  1.998e-02  -2.588 0.009718 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.56 on 2082 degrees of freedom
    ## Multiple R-squared:  0.4182, Adjusted R-squared:  0.4157 
    ## F-statistic: 166.3 on 9 and 2082 DF,  p-value: < 2.2e-16

``` r
vif(step_model)
```

    ##                 avg_ann_count                incidence_rate 
    ##                      1.162779                      1.405796 
    ##               poverty_percent               percent_married 
    ##                      2.559924                      2.167521 
    ##           pct_bach_deg25_over         pct_unemployed16_over 
    ##                      1.696786                      1.681153 
    ##                    birth_rate                black_high_ind 
    ##                      1.060582                     86.027152 
    ## incidence_rate:black_high_ind 
    ##                     87.896436

Backward elimination
--------------------

``` r
back_model <- step(full_model, direction = "backward", trace=FALSE ) 
summary(back_model)
vif(back_model)
```

backward elimination suggest the same with stepwise

Forward elimination
-------------------

``` r
null<- lm(target_death_rate ~ 1, data = num_df)
for_model <- step(null, scope=list(lower=null, upper=full_model), direction="forward")
summary(for_model)
vif(for_model)
for_model<- update(for_model,.~.-avg_deaths_per_year)
for_model<- update(for_model,.~.-pop_est2015)
for_model<- update(for_model,.~.+black_high_ind*incidence_rate)
```

``` r
summary(for_model)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_bach_deg25_over + incidence_rate + 
    ##     percent_married + pct_unemployed16_over + black_high_ind + 
    ##     birth_rate + avg_ann_count + poverty_percent + incidence_rate:black_high_ind, 
    ##     data = num_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -90.191 -11.355  -0.401  10.833 136.807 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.209e+02  9.495e+00  12.728  < 2e-16 ***
    ## pct_bach_deg25_over           -1.892e+00  1.058e-01 -17.884  < 2e-16 ***
    ## incidence_rate                 1.957e-01  9.906e-03  19.755  < 2e-16 ***
    ## percent_married               -2.428e-01  1.011e-01  -2.401 0.016435 *  
    ## pct_unemployed16_over          6.693e-01  1.848e-01   3.622 0.000299 ***
    ## black_high_ind                 2.753e+01  9.161e+00   3.004 0.002692 ** 
    ## birth_rate                    -7.613e-01  2.181e-01  -3.491 0.000491 ***
    ## avg_ann_count                 -1.006e-03  3.064e-04  -3.285 0.001037 ** 
    ## poverty_percent                3.592e-01  1.358e-01   2.644 0.008254 ** 
    ## incidence_rate:black_high_ind -5.172e-02  1.998e-02  -2.588 0.009718 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.56 on 2082 degrees of freedom
    ## Multiple R-squared:  0.4182, Adjusted R-squared:  0.4157 
    ## F-statistic: 166.3 on 9 and 2082 DF,  p-value: < 2.2e-16

``` r
vif(for_model)
```

    ##           pct_bach_deg25_over                incidence_rate 
    ##                      1.696786                      1.405796 
    ##               percent_married         pct_unemployed16_over 
    ##                      2.167521                      1.681153 
    ##                black_high_ind                    birth_rate 
    ##                     86.027152                      1.060582 
    ##                 avg_ann_count               poverty_percent 
    ##                      1.162779                      2.559924 
    ## incidence_rate:black_high_ind 
    ##                     87.896436

<https://www.cancer.org/latest-news/facts-and-figures-2018-rate-of-deaths-from-cancer-continues-decline.html>
