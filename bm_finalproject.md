bm\_finalproject
================
Coco
12/7/2018

Read data
---------

``` r
 cancer_df <- read.csv(file = "./data/Cancer_Registry.csv") %>% 
   janitor::clean_names() %>% 
   na.omit() %>% 
   separate(geography, into = c("county", "state"),sep = ",") %>% 
   separate(binned_inc, into = c("binned_inc_low","binned_inc_high"), sep = ",") %>% 
   mutate(binned_inc_low = str_replace(binned_inc_low,"[(]",""),
          binned_inc_high = str_replace(binned_inc_high,"[]]",""),
          binned_inc_low = as.numeric(gsub(",","",binned_inc_low,fixed=TRUE)),
          binned_inc_high = as.numeric(gsub(",","",binned_inc_high,fixed=TRUE)))
```

    ## Warning in evalq(as.numeric(gsub(",", "", binned_inc_low, fixed = TRUE)), :
    ## NAs introduced by coercion

``` r
num_df <- cancer_df %>% 
  dplyr::select(-county, -state, -median_age_male, -median_age_female) %>% 
  dplyr::select(-contains("pct")) 
```

``` r
round(cor(num_df),3) %>% 
  knitr::kable(digits = 2)
```

|                        |  avg\_ann\_count|  avg\_deaths\_per\_year|  target\_death\_rate|  incidence\_rate|  med\_income|  pop\_est2015|  poverty\_percent|  study\_per\_cap|  binned\_inc\_low|  binned\_inc\_high|  median\_age|  avg\_household\_size|  percent\_married|  birth\_rate|
|------------------------|----------------:|-----------------------:|--------------------:|----------------:|------------:|-------------:|-----------------:|----------------:|-----------------:|------------------:|------------:|---------------------:|-----------------:|------------:|
| avg\_ann\_count        |             1.00|                    0.96|                -0.13|             0.01|         0.22|          0.96|             -0.11|             0.08|                NA|               0.16|        -0.03|                  0.08|             -0.10|        -0.04|
| avg\_deaths\_per\_year |             0.96|                    1.00|                -0.10|             0.01|         0.19|          0.99|             -0.05|             0.09|                NA|               0.14|        -0.02|                  0.10|             -0.16|        -0.08|
| target\_death\_rate    |            -0.13|                   -0.10|                 1.00|             0.38|        -0.38|         -0.11|              0.37|            -0.03|                NA|              -0.30|        -0.01|                 -0.06|             -0.23|        -0.05|
| incidence\_rate        |             0.01|                    0.01|                 0.38|             1.00|         0.06|         -0.02|             -0.02|             0.10|                NA|               0.05|         0.02|                 -0.12|             -0.13|        -0.10|
| med\_income            |             0.22|                    0.19|                -0.38|             0.06|         1.00|          0.18|             -0.80|             0.12|                NA|               0.88|         0.00|                  0.12|              0.34|        -0.08|
| pop\_est2015           |             0.96|                    0.99|                -0.11|            -0.02|         0.18|          1.00|             -0.04|             0.07|                NA|               0.14|        -0.02|                  0.11|             -0.14|        -0.06|
| poverty\_percent       |            -0.11|                   -0.05|                 0.37|            -0.02|        -0.80|         -0.04|              1.00|            -0.09|                NA|              -0.60|        -0.02|                  0.06|             -0.63|         0.02|
| study\_per\_cap        |             0.08|                    0.09|                -0.03|             0.10|         0.12|          0.07|             -0.09|             1.00|                NA|               0.07|        -0.03|                  0.01|             -0.03|        -0.02|
| binned\_inc\_low       |               NA|                      NA|                   NA|               NA|           NA|            NA|                NA|               NA|                 1|                 NA|           NA|                    NA|                NA|           NA|
| binned\_inc\_high      |             0.16|                    0.14|                -0.30|             0.05|         0.88|          0.14|             -0.60|             0.07|                NA|               1.00|         0.01|                  0.12|              0.27|        -0.08|
| median\_age            |            -0.03|                   -0.02|                -0.01|             0.02|         0.00|         -0.02|             -0.02|            -0.03|                NA|               0.01|         1.00|                 -0.04|              0.03|         0.00|
| avg\_household\_size   |             0.08|                    0.10|                -0.06|            -0.12|         0.12|          0.11|              0.06|             0.01|                NA|               0.12|        -0.04|                  1.00|             -0.10|         0.01|
| percent\_married       |            -0.10|                   -0.16|                -0.23|            -0.13|         0.34|         -0.14|             -0.63|            -0.03|                NA|               0.27|         0.03|                 -0.10|              1.00|         0.10|
| birth\_rate            |            -0.04|                   -0.08|                -0.05|            -0.10|        -0.08|         -0.06|              0.02|            -0.02|                NA|              -0.08|         0.00|                  0.01|              0.10|         1.00|

stepwise
--------

``` r
full_model = lm(target_death_rate ~ ., data = num_df)
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ ., data = num_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -119.448  -11.120   -0.663   11.351  109.730 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          1.643e+02  2.901e+01   5.663 2.47e-08 ***
    ## avg_ann_count       -3.383e-03  1.830e-03  -1.848  0.06513 .  
    ## avg_deaths_per_year  1.794e-02  9.486e-03   1.891  0.05921 .  
    ## incidence_rate       1.732e-01  1.893e-02   9.153  < 2e-16 ***
    ## med_income          -1.684e-04  3.275e-04  -0.514  0.60728    
    ## pop_est2015         -1.477e-05  1.224e-05  -1.206  0.22827    
    ## poverty_percent     -7.080e-02  4.817e-01  -0.147  0.88320    
    ## study_per_cap       -1.146e-03  2.628e-03  -0.436  0.66295    
    ## binned_inc_low      -1.114e-03  3.969e-04  -2.808  0.00518 ** 
    ## binned_inc_high      6.812e-05  9.218e-05   0.739  0.46024    
    ## median_age           6.849e-03  2.073e-02   0.330  0.74117    
    ## avg_household_size   2.094e+00  2.401e+00   0.872  0.38352    
    ## percent_married     -2.592e-01  2.308e-01  -1.123  0.26183    
    ## birth_rate          -2.926e-02  5.228e-01  -0.056  0.95538    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.95 on 520 degrees of freedom
    ##   (57 observations deleted due to missingness)
    ## Multiple R-squared:  0.2777, Adjusted R-squared:  0.2597 
    ## F-statistic: 15.38 on 13 and 520 DF,  p-value: < 2.2e-16

``` r
step_model <- stepAIC(full_model, direction = "both", 
                      trace = FALSE)
summary(step_model)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + avg_deaths_per_year + 
    ##     incidence_rate + binned_inc_low + percent_married, data = num_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -121.009  -10.848   -0.276   11.409  110.442 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          1.650e+02  1.365e+01  12.085   <2e-16 ***
    ## avg_ann_count       -3.845e-03  1.758e-03  -2.187   0.0292 *  
    ## avg_deaths_per_year  8.679e-03  4.960e-03   1.750   0.0807 .  
    ## incidence_rate       1.733e-01  1.840e-02   9.416   <2e-16 ***
    ## binned_inc_low      -1.107e-03  1.276e-04  -8.676   <2e-16 ***
    ## percent_married     -2.766e-01  1.824e-01  -1.517   0.1299    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.85 on 528 degrees of freedom
    ##   (57 observations deleted due to missingness)
    ## Multiple R-squared:  0.2736, Adjusted R-squared:  0.2667 
    ## F-statistic: 39.77 on 5 and 528 DF,  p-value: < 2.2e-16
