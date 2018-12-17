bm\_finalproject
================
Coco
12/7/2018

Read data
---------

``` r
 cancer_df <- read.csv(file = "./data/Cancer_Registry.csv") %>% 
   janitor::clean_names() %>% 
   separate(geography, into = c("county", "state"),sep = ",") %>% 
   separate(binned_inc, into = c("binned_inc_low","binned_inc_high"), sep = ",") %>% 
   mutate(binned_inc_low = str_replace(binned_inc_low,"[(]",""),
          binned_inc_high = str_replace(binned_inc_high,"[]]",""),
          binned_inc_low = as.numeric(gsub(",","",binned_inc_low,fixed=TRUE)),
          binned_inc_high = as.numeric(gsub(",","",binned_inc_high,fixed=TRUE)),
          median_inc = (binned_inc_high+binned_inc_low)/2,
          pct_no_cov = 100 - pct_public_coverage_alone - pct_private_coverage_alone,
          black_high_ind = case_when(pct_black > 9 ~ "1", TRUE ~ "0"),
          black_high_ind = as.numeric(black_high_ind)) %>% 
  dplyr::select(-binned_inc_high, -binned_inc_low, -contains("coverage"),-pct_no_hs18_24, -pct_hs18_24, -pct_some_col18_24,-pct_hs25_over,-pct_married_households)


num_df <- cancer_df %>% 
  dplyr::select(-county, -state, -median_age_male, -median_age_female,-pct_white,-pct_black,-pct_asian,-pct_other_race) %>% 
  na.omit()
```

Exploratory Analysis
--------------------

``` r
cor(num_df) %>% knitr::kable()

bach = num_df %>% 
  ggplot(aes(x  = pct_bach_deg25_over, y = target_death_rate)) + 
  geom_point()

inc = num_df %>% 
  ggplot(aes(x  = incidence_rate, y = target_death_rate)) + 
  geom_point()

income = num_df %>% 
  ggplot(aes(x  = med_income, y = target_death_rate)) + 
  geom_point()

employ = num_df %>% 
  ggplot(aes(x  = pct_employed16_over, y = target_death_rate)) + 
  geom_point()

(bach + inc) / (income + employ)
```

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
step_model_ours<- update(step_model,.~.-pop_est2015)
step_model_plus<- update(step_model_ours,.~.+black_high_ind*incidence_rate)
step_model_plus<- update(step_model_plus,.~.+pct_no_cov)
```

``` r
summary(step_model_ours)
summary(step_model_plus)
vif(step_model)
vif(step_model_plus)
```

### Cp&AIC&adjustedr2

``` r
ours = glance(step_model_ours) %>% 
  as.data.frame() %>% 
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value) %>% 
  mutate(cp = ols_mallows_cp(step_model_ours, full_model)) %>% 
  rename(RSE = sigma)


plus = glance(step_model_plus) %>% 
  as.data.frame() %>% 
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value) %>% 
  mutate(cp = ols_mallows_cp(step_model_plus, full_model)) %>% 
  rename(RSE = sigma)
```

We decide to use plus as the final model

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
vif(for_model)
```

<https://www.cancer.org/latest-news/facts-and-figures-2018-rate-of-deaths-from-cancer-continues-decline.html>

### Cross validation

``` r
cv_df <-  crossv_mc(num_df, n=100, test = .2)

step_lm <- lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + poverty_percent + percent_married + pct_bach_deg25_over + 
pct_unemployed16_over + birth_rate + black_high_ind, data = num_df)

plus_lm <- lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
 poverty_percent + percent_married + pct_bach_deg25_over + 
pct_unemployed16_over + birth_rate + black_high_ind + pct_no_cov + 
incidence_rate:black_high_ind, data = num_df)



cv_result <- cv_df %>% 
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>% 
  mutate(step_mod   = map(train, ~step_lm),
         plus_mod = map(train, ~plus_lm)) %>% 
  mutate(rmse_step    = map2_dbl(step_mod, test, ~rmse(model = .x, data = .y)),
         rmse_plus = map2_dbl(plus_mod, test, ~rmse(model = .x, data = .y)))

cv_result %>% 
  dplyr::select(rmse_step,rmse_plus) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()

rmse_result <- cv_result %>% 
  dplyr::select(rmse_step,rmse_plus) %>%
  lapply(mean) %>% 
  as.data.frame(digit=3) %>%
  gather(key=model, value=rmse, rmse_step:rmse_plus)
```

### Criterion Method Summary

``` r
rbind(ours, plus) %>% 
  cbind(.,rmse_result) %>% 
  dplyr::select(model, everything()) %>% 
  knitr::kable(digits = 2)
```

<<<<<<< HEAD
| model      |  adj.r.squared|    RSE|       AIC|       BIC|  p.value|     cp|   rmse|
|:-----------|--------------:|------:|---------:|---------:|--------:|------:|------:|
| rmse\_step |           0.41|  19.59|  18395.16|  18451.62|        0|  20.82|  19.65|
| rmse\_plus |           0.42|  19.56|  18391.85|  18459.60|        0|  17.52|  19.59|

=======
>>>>>>> e27b50eab3aeac86721be481682e4e90bff9f462
### Ridge Regression

``` r
# Try a grid of values for lambda: from 10^-2 to 10^5

grid <- 10^seq(5,-2, length=100)

Y <- num_df[,3]

X <- model.matrix(target_death_rate ~ avg_ann_count + incidence_rate + 
 poverty_percent + percent_married + pct_bach_deg25_over + 
pct_unemployed16_over + birth_rate + black_high_ind + pct_no_cov + 
incidence_rate:black_high_ind, data = num_df)

set.seed(1)

test<-sample(1:nrow(X),nrow(X)/5)

train<-(-test)

Y.test<-Y[test]


# Cross-validation
set.seed(2)
cv.out<-cv.glmnet(X[train,],Y[train])

# Fit a Ridge model with all observations with the best lambda

best.lambda<- cv.out$lambda.min

ridge2<- glmnet(X[train ,],Y[train], alpha =0, lambda=best.lambda)
ridge2.cv<- cv.glmnet(X[train ,],Y[train], alpha = 0)

## Generating the coef
coef_ridge <- broom::tidy(coef(ridge2.cv))
coef_lm <- broom::tidy(coef(step_model_plus))
coef <- cbind(coef_ridge[,-2],coef_lm[,-1])
colnames(coef) <- c('coef','ridge','lm')
knitr::kable(coef, digits = 3)

ridge.pred <- predict(ridge2.cv,s=best.lambda,newx=X[test,])

rmse_ridge <- sqrt(mean((ridge.pred-Y.test)^2))

cv_result %>% 
  dplyr::select(rmse_step,rmse_plus) %>%
  lapply(mean) %>% 
  as.data.frame(digit=3) %>% 
  mutate(rmse_ridge = rmse_ridge)
```

<<<<<<< HEAD
    ##   rmse_step rmse_plus rmse_ridge
    ## 1   19.6473  19.58822   19.14778

=======
>>>>>>> e27b50eab3aeac86721be481682e4e90bff9f462
Leverages
---------

``` r
lev_step = hat(model.matrix(step_model_ours))
lev_step_plus = hat(model.matrix(step_model_plus))

# plot leverages
plot(lev_step)
plot(lev_step_plus)

# show observation with high leverage
num_df[lev_step > 0.30,] #observation 1000
num_df[lev_step_plus > 0.20,] #observation 282 and 1000
```

Outliers Using Studentized Residuals
------------------------------------

``` r
stu_res_step = rstandard(step_model_ours)
outliers_y_step = stu_res_step[abs(stu_res_step) > 2.5]
outliers_y_step

stu_res_plus = rstandard(step_model_plus)
outliers_y_plus = stu_res_plus[abs(stu_res_plus) > 2.5]
outliers_y_plus

stu_res_back = rstandard(back_model)
outliers_y_back = stu_res_back[abs(stu_res_back) > 2.5]
outliers_y_back

# 282, 1221, 1366
```

Cook's Distance
---------------

``` r
# step model ours
plot(step_model_ours)

# step model plus
plot(step_model_plus)


# Observation 282, 1221, 1366 should be looked at 
```

Various Measures of Influence
-----------------------------

``` r
influence.measures(step_model)
summary(step_model)

# 282, 1221, 1366 noted as potentially influential 

plot(num_df$incidence_rate, num_df$target_death_rate)
```

Remove Outliers for First Model
-------------------------------

``` r
# remove observation 282 only
num_df_no_282 = cancer_df %>% 
  dplyr::select(-county, -state, -median_age_male, -median_age_female,-pct_white,-pct_black,-pct_asian,-pct_other_race) %>% 
  tibble::rowid_to_column() %>% 
  filter(rowid != 282) %>% 
  na.omit()

num_df_no_1000 = cancer_df %>% 
  dplyr::select(-county, -state, -median_age_male, -median_age_female,-pct_white,-pct_black,-pct_asian,-pct_other_race) %>% 
  tibble::rowid_to_column() %>% 
  filter(rowid != 1000) %>% 
  na.omit()

num_df_no_1221 = cancer_df %>% 
  dplyr::select(-county, -state, -median_age_male, -median_age_female,-pct_white,-pct_black,-pct_asian,-pct_other_race) %>% 
  tibble::rowid_to_column() %>% 
  filter(rowid != 1221) %>% 
  na.omit()

num_df_no_1366 = cancer_df %>% 
  dplyr::select(-county, -state, -median_age_male, -median_age_female,-pct_white,-pct_black,-pct_asian,-pct_other_race) %>% 
  tibble::rowid_to_column() %>% 
  filter(rowid != 1366) %>% 
  na.omit()

step_no_282 = update(step_model_ours, . ~ ., data = num_df_no_282)
summary(step_no_282)
summary(step_model_ours)

step_no_1000 = update(step_model_ours, . ~ ., data = num_df_no_1000)
summary(step_no_1000)
summary(step_model_ours)

step_no_1221 = update(step_model_ours, . ~ ., data = num_df_no_1221)
summary(step_no_1221)
summary(step_model_ours)

step_no_1366 = update(step_model_ours, . ~ ., data = num_df_no_1366)
summary(step_no_1366)
summary(step_model_ours)
```

Remove influential points for Second Model
------------------------------------------

``` r
step_no_282_plus = update(step_model_plus, . ~ ., data = num_df_no_282)
summary(step_no_282_plus)
<<<<<<< HEAD
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + percent_married + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + birth_rate + black_high_ind + pct_no_cov + 
    ##     incidence_rate:black_high_ind, data = num_df_no_282)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -89.936 -11.344  -0.462  10.635 136.238 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.228e+02  9.651e+00  12.724  < 2e-16 ***
    ## avg_ann_count                 -1.092e-03  3.056e-04  -3.572 0.000362 ***
    ## incidence_rate                 1.946e-01  1.007e-02  19.320  < 2e-16 ***
    ## poverty_percent                3.994e-01  1.443e-01   2.767 0.005702 ** 
    ## percent_married               -2.451e-01  1.039e-01  -2.359 0.018408 *  
    ## pct_bach_deg25_over           -1.891e+00  1.075e-01 -17.594  < 2e-16 ***
    ## pct_unemployed16_over          6.648e-01  1.849e-01   3.596 0.000331 ***
    ## birth_rate                    -7.651e-01  2.170e-01  -3.526 0.000432 ***
    ## black_high_ind                 5.137e+00  1.011e+01   0.508 0.611405    
    ## pct_no_cov                    -5.807e-02  9.760e-02  -0.595 0.551899    
    ## incidence_rate:black_high_ind -2.745e-03  2.206e-02  -0.124 0.900998    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.45 on 2080 degrees of freedom
    ## Multiple R-squared:  0.4253, Adjusted R-squared:  0.4225 
    ## F-statistic: 153.9 on 10 and 2080 DF,  p-value: < 2.2e-16

``` r
summary(step_model_plus)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + percent_married + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + birth_rate + black_high_ind + pct_no_cov + 
    ##     incidence_rate:black_high_ind, data = num_df)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -89.73 -11.36  -0.49  10.78 136.59 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.224e+02  9.707e+00  12.609  < 2e-16 ***
    ## avg_ann_count                 -1.023e-03  3.071e-04  -3.330 0.000885 ***
    ## incidence_rate                 1.941e-01  1.013e-02  19.155  < 2e-16 ***
    ## poverty_percent                3.984e-01  1.452e-01   2.745 0.006112 ** 
    ## percent_married               -2.228e-01  1.044e-01  -2.134 0.032964 *  
    ## pct_bach_deg25_over           -1.909e+00  1.081e-01 -17.666  < 2e-16 ***
    ## pct_unemployed16_over          6.850e-01  1.859e-01   3.684 0.000235 ***
    ## birth_rate                    -7.546e-01  2.183e-01  -3.457 0.000557 ***
    ## black_high_ind                 2.720e+01  9.172e+00   2.965 0.003060 ** 
    ## pct_no_cov                    -7.527e-02  9.811e-02  -0.767 0.443086    
    ## incidence_rate:black_high_ind -5.099e-02  2.001e-02  -2.548 0.010900 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.56 on 2081 degrees of freedom
    ## Multiple R-squared:  0.4183, Adjusted R-squared:  0.4155 
    ## F-statistic: 149.7 on 10 and 2081 DF,  p-value: < 2.2e-16
=======
summary(step_model_plus)
>>>>>>> e27b50eab3aeac86721be481682e4e90bff9f462

step_no_1000_plus = update(step_model_plus, . ~ ., data = num_df_no_1000)
summary(step_no_1000_plus)
summary(step_model_plus)

step_no_1221_plus = update(step_model_plus, . ~ ., data = num_df_no_1221)
summary(step_no_1221_plus)
summary(step_model_plus)

step_no_1366_plus = update(step_model_plus, . ~ ., data = num_df_no_1366)
summary(step_no_1366_plus)
summary(step_model_plus)
```
