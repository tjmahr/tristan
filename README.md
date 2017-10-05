
<!-- README.md is generated from README.Rmd. Please edit that file -->
tristan [![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
====================================================================================================================================================================================================================================

This package contains my helper functions for working with models fit with RStanARM. The package is named tristan because I'm working with Stan samples and my name is Tristan.

I plan to incrementally update this package whenever I find myself solving the same old problems from an RStanARM model.

Installation
------------

You can install tristan from github with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/tristan")
```

Overview of helpers
-------------------

`augment_posterior_predict()` and `augment_posterior_linpred()` generate new data predictions and fitted means for new datasets using RStanARM's `posterior_predict()` and `posterior_linpred()`. The RStanARM functions return giant matrices of predicted values, but these functions return a long dataframe of predicted values along with the values of the predictor variables. The name *augment* follows the convention of the broom package where `augment()` refers to augmenting a data-set with model predictions.

`stan_to_lm()` and `stan_to_glm()` provide a quick way to refit an RStanARM model with its classical counterpart.

`tidy_etdi()`, `tidy_hdpi()`, `tidy_median()` and `double_etdi()` provide helpers for interval calclulation.

`draw_coef()`, `draw_fixef()`, `draw_ranef()` and `draw_var_corr()` work like the `coef()`, `fixef()`, `ranef()`, and `VarCorr()` functions for rstanarm mixed effects models, but they return a tidy dataframe of posterior samples instead of point estimates.

Example
-------

Fit a simple linear model.

``` r
library(tidyverse)
#> Warning: package 'dplyr' was built under R version 3.4.2
library(rstanarm)
library(tristan)

# Scaling makes the model run much faster
scale_v <- function(...) as.vector(scale(...))
iris$z.Sepal.Length <- scale_v(iris$Sepal.Length)
iris$z.Petal.Length <- scale_v(iris$Petal.Length)

# Just to ensure that NA values don't break the prediction function
iris[2:6, "Species"] <- NA

model <- stan_glm(
  z.Sepal.Length ~ z.Petal.Length * Species,
  data = iris,
  family = gaussian(),
  prior = normal(0, 2))
```

``` r
print(model)
#> stan_glm
#>  family:  gaussian [identity]
#>  formula: z.Sepal.Length ~ z.Petal.Length * Species
#> ------
#> 
#> Estimates:
#>                                  Median MAD_SD
#> (Intercept)                       0.0    0.7  
#> z.Petal.Length                    0.8    0.5  
#> Speciesversicolor                -0.4    0.7  
#> Speciesvirginica                 -1.3    0.7  
#> z.Petal.Length:Speciesversicolor  1.0    0.6  
#> z.Petal.Length:Speciesvirginica   1.3    0.6  
#> sigma                             0.4    0.0  
#> 
#> Sample avg. posterior predictive 
#> distribution of y (X = xbar):
#>          Median MAD_SD
#> mean_PPD 0.0    0.0   
#> 
#> ------
#> For info on the priors used see help('prior_summary.stanreg').
```

### Posterior fitted values (linear predictions)

Let's plot some samples of the model's linear prediction for the mean. If classical model provide a single "line of best fit", Bayesian models provide a distribution "lines of plausible fit". We'd like to visualize 100 of these lines alongside the raw data.

In classical models, getting the fitted values is easily done by adding a column of `fitted()` values to dataframe or using `predict()` on some new observations.

Because the posterior of this model contains 4000 such fitted or predicted values, more data wrangling and reshaping is required. `augment_posterior_linpred()` automates this task by producing a long dataframe with one row per posterior fitted value.

Here, we tell the model that we want just 100 of those lines (i.e., 100 samples from the posterior distribution).

``` r
# Get the fitted means of the data for 100 samples of the posterior distribution
linear_preds <- augment_posterior_linpred(
  model = model, 
  newdata = iris, 
  nsamples = 100)
linear_preds
#> # A tibble: 14,500 x 10
#>    .observation .draw .posterior_value Sepal.Length Sepal.Width
#>           <int> <int>            <dbl>        <dbl>       <dbl>
#>  1            1     1       -0.9450143          5.1         3.5
#>  2            1     2       -1.0788779          5.1         3.5
#>  3            1     3       -1.0356139          5.1         3.5
#>  4            1     4       -1.0491301          5.1         3.5
#>  5            1     5       -0.9396638          5.1         3.5
#>  6            1     6       -1.1621977          5.1         3.5
#>  7            1     7       -1.0184043          5.1         3.5
#>  8            1     8       -1.0316247          5.1         3.5
#>  9            1     9       -1.0556448          5.1         3.5
#> 10            1    10       -1.0218903          5.1         3.5
#> # ... with 14,490 more rows, and 5 more variables: Petal.Length <dbl>,
#> #   Petal.Width <dbl>, Species <fctr>, z.Sepal.Length <dbl>,
#> #   z.Petal.Length <dbl>
```

To plot the lines, we have to unscale the model's fitted values.

``` r
unscale <- function(scaled, original) {
  (scaled * sd(original, na.rm = TRUE)) + mean(original, na.rm = TRUE)
}

linear_preds$.posterior_value <- unscale(
  scaled = linear_preds$.posterior_value, 
  original = iris$Sepal.Length)
```

Now, we can do a spaghetti plot of linear predictions.

``` r
ggplot(iris) + 
  aes(x = Petal.Length, y = Sepal.Length, color = Species) + 
  geom_point() + 
  geom_line(aes(y = .posterior_value, group = interaction(Species, .draw)), 
            data = linear_preds, alpha = .20)
```

![](fig/README-many-lines-of-best-fit-1.png)

### Posterior predictions (simulated new data)

`augment_posterior_predict()` similarly tidies values from the `posterior_predict()` function. `posterior_predict()` incorporates the error terms from the model, so it can be used predict new fake data from the model.

Let's create a range of values within each species, and get posterior predictions for those values.

``` r
library(modelr)

# Within each species, generate a sequence of z.Petal.Length values
newdata <- iris %>% 
  group_by(Species) %>% 
  # Expand the range x value a little bit so that the points do not bulge out
  # left/right sides of the uncertainty ribbon in the plot
  data_grid(z.Petal.Length = z.Petal.Length %>% 
              seq_range(n = 80, expand = .10)) %>% 
  ungroup()

newdata$Petal.Length <- unscale(newdata$z.Petal.Length, iris$Petal.Length)

# Get posterior predictions
posterior_preds <- augment_posterior_predict(model, newdata)
posterior_preds
#> # A tibble: 960,000 x 6
#>    .observation .draw .posterior_value Species z.Petal.Length Petal.Length
#>           <int> <int>            <dbl>  <fctr>          <dbl>        <dbl>
#>  1            1     1       -1.2846158  setosa      -1.587834        0.955
#>  2            1     2       -0.8286355  setosa      -1.587834        0.955
#>  3            1     3       -1.9339924  setosa      -1.587834        0.955
#>  4            1     4       -1.2500540  setosa      -1.587834        0.955
#>  5            1     5       -1.5456511  setosa      -1.587834        0.955
#>  6            1     6       -0.8536395  setosa      -1.587834        0.955
#>  7            1     7       -0.9103809  setosa      -1.587834        0.955
#>  8            1     8       -2.0104667  setosa      -1.587834        0.955
#>  9            1     9       -1.6707626  setosa      -1.587834        0.955
#> 10            1    10       -1.6492947  setosa      -1.587834        0.955
#> # ... with 959,990 more rows

posterior_preds$.posterior_value <- unscale(
  scaled = posterior_preds$.posterior_value, 
  original = iris$Sepal.Length)
```

Take a second to appreciate the size of that table. It has 4000 predictions for each the 320 observations in `newdata`.

Now, we might inspect whether 95% of the data falls inside the 95% interval of posterior-predicted values (among other questions we could ask the model.)

``` r
ggplot(iris) + 
  aes(x = Petal.Length, y = Sepal.Length, color = Species) + 
  geom_point() + 
  stat_summary(aes(y = .posterior_value, group = Species, color = NULL), 
               data = posterior_preds, alpha = 0.4, fill = "grey60", 
               geom = "ribbon", 
               fun.data = median_hilow, fun.args = list(conf.int = .95))
```

![](fig/README-95-percent-intervals-1.png)

### Refit RStanARM models with classical counterparts

There are some quick functions for refitting RStanARM models using classical versions. These functions basically inject the values of `model$formula` and `model$data` into `lm()` or `glm()`. (Seriously, see the call sections in the two outputs below.) Therefore, don't use these functions for serious comparisons of classical versus Bayesian models.

Refit with a linear model:

``` r
arm::display(stan_to_lm(model))
#> Please manually fit model if original model used any 
#> arguments besides `formula` and `data`.
#> stats::lm(formula = stats::formula(model), data = model$data, 
#>     weights = if (length(model$weights) == 0) NULL else model$weights, 
#>     offset = model$offset)
#>                                  coef.est coef.se
#> (Intercept)                       0.33     0.80  
#> z.Petal.Length                    1.03     0.61  
#> Speciesversicolor                -0.72     0.81  
#> Speciesvirginica                 -1.59     0.83  
#> z.Petal.Length:Speciesversicolor  0.74     0.65  
#> z.Petal.Length:Speciesvirginica   1.10     0.64  
#> ---
#> n = 145, k = 6
#> residual sd = 0.41, R-Squared = 0.84
```

Refit with a generalized linear model:

``` r
arm::display(stan_to_glm(model))
#> Please manually fit model if original model used any 
#> arguments besides `formula`, `family`, and `data`.
#> stats::glm(formula = stats::formula(model), family = model$family, 
#>     data = model$data, weights = if (length(model$weights) == 
#>         0) NULL else model$weights, offset = model$offset)
#>                                  coef.est coef.se
#> (Intercept)                       0.33     0.80  
#> z.Petal.Length                    1.03     0.61  
#> Speciesversicolor                -0.72     0.81  
#> Speciesvirginica                 -1.59     0.83  
#> z.Petal.Length:Speciesversicolor  0.74     0.65  
#> z.Petal.Length:Speciesvirginica   1.10     0.64  
#> ---
#>   n = 145, k = 6
#>   residual deviance = 23.3, null deviance = 142.0 (difference = 118.7)
#>   overdispersion parameter = 0.2
#>   residual sd is sqrt(overdispersion) = 0.41
```

### Calculate (classical) *R*<sup>2</sup>

`calculate_classical_r2()` returns the unadjusted *R*<sup>2</sup> for each draw of the posterior distribution.

``` r
df_r2 <- data_frame(
  R2 = calculate_model_r2(model)
)
df_r2
#> # A tibble: 4,000 x 1
#>           R2
#>        <dbl>
#>  1 0.8328123
#>  2 0.8317308
#>  3 0.8328224
#>  4 0.8294250
#>  5 0.8312924
#>  6 0.8272437
#>  7 0.8233947
#>  8 0.8322048
#>  9 0.8312965
#> 10 0.8218496
#> # ... with 3,990 more rows
```

### Interval calculation

Two more helper functions compute tidy data-frames of posterior density intervals. `tidy_hpdi()` provides the highest-density posterior interval for model parameters, while `tidy_etdi()` computes the equal-tailed density intervals (the typical sort of intervals used for uncertain intervals.)

``` r
tidy_hpdi(model)
#> # A tibble: 7 x 5
#>                               term interval density       lower      upper
#>                              <chr>    <chr>   <dbl>       <dbl>      <dbl>
#> 1                      (Intercept)     HPDI     0.9 -0.96780513  1.2744215
#> 2                   z.Petal.Length     HPDI     0.9 -0.02275323  1.7009889
#> 3                Speciesversicolor     HPDI     0.9 -1.58955554  0.6581076
#> 4                 Speciesvirginica     HPDI     0.9 -2.46028962 -0.1653479
#> 5 z.Petal.Length:Speciesversicolor     HPDI     0.9  0.04041798  1.9201696
#> 6  z.Petal.Length:Speciesvirginica     HPDI     0.9  0.40728541  2.2280055
#> 7                            sigma     HPDI     0.9  0.36923533  0.4517970
tidy_etdi(model)
#> # A tibble: 7 x 5
#>                               term interval density       lower
#>                              <chr>    <chr>   <dbl>       <dbl>
#> 1                      (Intercept)     ETDI     0.9 -1.11840092
#> 2                   z.Petal.Length     ETDI     0.9 -0.09340855
#> 3                Speciesversicolor     ETDI     0.9 -1.54307619
#> 4                 Speciesvirginica     ETDI     0.9 -2.41659778
#> 5 z.Petal.Length:Speciesversicolor     ETDI     0.9  0.02942054
#> 6  z.Petal.Length:Speciesvirginica     ETDI     0.9  0.40763669
#> 7                            sigma     ETDI     0.9  0.37389087
#> # ... with 1 more variables: upper <dbl>
```

The functions also work on single vectors of numbers, for quick one-off calculations.

``` r
r2_intervals <- bind_rows(
  tidy_hpdi(calculate_model_r2(model)),
  tidy_etdi(calculate_model_r2(model))
)
r2_intervals
#> # A tibble: 2 x 5
#>                        term interval density     lower     upper
#>                       <chr>    <chr>   <dbl>     <dbl>     <dbl>
#> 1 calculate_model_r2(model)     HPDI     0.9 0.8226714 0.8348560
#> 2 calculate_model_r2(model)     ETDI     0.9 0.8203498 0.8340235
```

We can compare the difference between highest-posterior density intervals and equal-tailed intervals.

``` r
ggplot(df_r2) + 
  aes(x = R2) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = lower, color = interval), 
             data = r2_intervals, size = 1, linetype = "dashed") + 
  geom_vline(aes(xintercept = upper, color = interval), 
             data = r2_intervals, size = 1, linetype = "dashed") +
  labs(color = "90% interval",
       x = expression(R^2),
       y = "Num. posterior samples")
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](fig/README-r2-histogram-1.png)

Pure sugar
----------

`double_etdi()` that provides all the values needed to make a double interval (caterpillar) plot.

``` r
df1 <- double_etdi(calculate_model_r2(model), .95, .90)
df2 <- double_etdi(model, .95, .90)
df <- bind_rows(df1, df2)
df
#> # A tibble: 8 x 9
#>                               term outer_lower inner_lower    estimate
#>                              <chr>       <dbl>       <dbl>       <dbl>
#> 1        calculate_model_r2(model)   0.8180068  0.82034976  0.82963306
#> 2                      (Intercept)  -1.3804081 -1.11840092  0.03694444
#> 3                   z.Petal.Length  -0.2837505 -0.09340855  0.79797208
#> 4                Speciesversicolor  -1.7564642 -1.54307619 -0.42134720
#> 5                 Speciesvirginica  -2.6198936 -2.41659778 -1.27733443
#> 6 z.Petal.Length:Speciesversicolor  -0.1378244  0.02942054  0.97221508
#> 7  z.Petal.Length:Speciesvirginica   0.2497426  0.40763669  1.31514103
#> 8                            sigma   0.3676592  0.37389087  0.41193255
#> # ... with 5 more variables: inner_upper <dbl>, outer_upper <dbl>,
#> #   est_type <chr>, inner_density <dbl>, outer_density <dbl>

ggplot(df) + 
  aes(x = estimate, y = term, yend = term) + 
  geom_vline(color = "white", size = 2, xintercept = 0) + 
  geom_segment(aes(x = outer_lower, xend = outer_upper)) + 
  geom_segment(aes(x = inner_lower, xend = inner_upper), size = 2) + 
  geom_point(size = 4)
```

![](fig/README-double-interval-1.png)

Predictions for mixed effects models
------------------------------------

This is such a finicky area that it will never 100% of the time but it works well enough, especially if the new observations are subsets of the original dataset.

``` r
cbpp <- lme4::cbpp
m2 <- stan_glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp, family = binomial,
  prior_covariance = decov(4, 1, 1), 
  prior = normal(0, 1))
#> trying deprecated constructor; please alert package maintainer
```

Add a new herd to the data.

``` r
# Add a new herd
newdata <- cbpp %>% 
  tibble::add_row(herd = "16", incidence = 0, size = 20, period = 1) %>% 
  tibble::add_row(herd = "16", incidence = 0, size = 20, period = 2) %>% 
  tibble::add_row(herd = "16", incidence = 0, size = 20, period = 3) %>% 
  tibble::add_row(herd = "16", incidence = 0, size = 20, period = NA)

d <- augment_posterior_predict(m2, newdata) %>% 
  dplyr::filter(herd == "16")
d
#> # A tibble: 12,000 x 7
#>    .observation .draw .posterior_value   herd incidence  size period
#>           <int> <int>            <int> <fctr>     <dbl> <dbl> <fctr>
#>  1           57     1                8     16         0    20      1
#>  2           57     2                4     16         0    20      1
#>  3           57     3                5     16         0    20      1
#>  4           57     4                7     16         0    20      1
#>  5           57     5                6     16         0    20      1
#>  6           57     6                3     16         0    20      1
#>  7           57     7                3     16         0    20      1
#>  8           57     8                6     16         0    20      1
#>  9           57     9                6     16         0    20      1
#> 10           57    10                5     16         0    20      1
#> # ... with 11,990 more rows
```

Plot the predictions.

``` r
ggplot(d, aes(x = interaction(herd, period), y = .posterior_value / size)) + 
  stat_summary(fun.data = median_hilow) + 
  theme_grey(base_size = 8)
```

![](fig/README-mixed-pred-1.png)

Samples for mixed effects models
--------------------------------

`coef`, `fixef()` and `ranef()` only give the point estimates for model effects. `draw_coef()`, `draw_fixef()` and `draw_ranef()` will sample them from the posterior distribution.

``` r
ranef(m2)
#> $herd
#>     (Intercept)
#> 1   0.585726541
#> 2  -0.315578819
#> 3   0.386807105
#> 4   0.002520164
#> 5  -0.231718913
#> 6  -0.445537342
#> 7   0.894365123
#> 8   0.668274806
#> 9  -0.281648583
#> 10 -0.588745893
#> 11 -0.119874277
#> 12 -0.083545977
#> 13 -0.768323937
#> 14  1.031629112
#> 15 -0.591667479
draw_ranef(m2, 100)
#> # A tibble: 1,500 x 6
#>    .draw .group_var .group       .term            .parameter
#>    <int>      <chr>  <chr>       <chr>                 <chr>
#>  1     1       herd      1 (Intercept) b[(Intercept) herd:1]
#>  2     2       herd      1 (Intercept) b[(Intercept) herd:1]
#>  3     3       herd      1 (Intercept) b[(Intercept) herd:1]
#>  4     4       herd      1 (Intercept) b[(Intercept) herd:1]
#>  5     5       herd      1 (Intercept) b[(Intercept) herd:1]
#>  6     6       herd      1 (Intercept) b[(Intercept) herd:1]
#>  7     7       herd      1 (Intercept) b[(Intercept) herd:1]
#>  8     8       herd      1 (Intercept) b[(Intercept) herd:1]
#>  9     9       herd      1 (Intercept) b[(Intercept) herd:1]
#> 10    10       herd      1 (Intercept) b[(Intercept) herd:1]
#> # ... with 1,490 more rows, and 1 more variables: .posterior_value <dbl>

fixef(m2)
#> (Intercept)     period2     period3     period4 
#>  -1.4884613  -0.8502120  -0.9655542  -1.3250250
draw_fixef(m2, 100)
#> # A tibble: 400 x 3
#>    .draw  .parameter .posterior_value
#>    <int>       <chr>            <dbl>
#>  1     1 (Intercept)       -1.2968494
#>  2     2 (Intercept)       -1.5348199
#>  3     3 (Intercept)       -1.1787171
#>  4     4 (Intercept)       -1.4681809
#>  5     5 (Intercept)       -1.4974331
#>  6     6 (Intercept)       -0.8424193
#>  7     7 (Intercept)       -1.4127342
#>  8     8 (Intercept)       -1.5275066
#>  9     9 (Intercept)       -1.5800322
#> 10    10 (Intercept)       -1.1636854
#> # ... with 390 more rows

coef(m2)
#> $herd
#>    (Intercept)   period2    period3   period4
#> 1   -0.9027348 -0.850212 -0.9655542 -1.325025
#> 2   -1.8040401 -0.850212 -0.9655542 -1.325025
#> 3   -1.1016542 -0.850212 -0.9655542 -1.325025
#> 4   -1.4859411 -0.850212 -0.9655542 -1.325025
#> 5   -1.7201802 -0.850212 -0.9655542 -1.325025
#> 6   -1.9339986 -0.850212 -0.9655542 -1.325025
#> 7   -0.5940962 -0.850212 -0.9655542 -1.325025
#> 8   -0.8201865 -0.850212 -0.9655542 -1.325025
#> 9   -1.7701099 -0.850212 -0.9655542 -1.325025
#> 10  -2.0772072 -0.850212 -0.9655542 -1.325025
#> 11  -1.6083356 -0.850212 -0.9655542 -1.325025
#> 12  -1.5720073 -0.850212 -0.9655542 -1.325025
#> 13  -2.2567852 -0.850212 -0.9655542 -1.325025
#> 14  -0.4568322 -0.850212 -0.9655542 -1.325025
#> 15  -2.0801288 -0.850212 -0.9655542 -1.325025
#> 
#> attr(,"class")
#> [1] "coef.mer"
draw_coef(m2, 100)
#> # A tibble: 6,000 x 9
#>    .draw .group_var .group       .term .fixef_parameter
#>    <int>      <chr>  <chr>       <chr>            <chr>
#>  1     1       herd      1 (Intercept)      (Intercept)
#>  2     1       herd      1     period2          period2
#>  3     1       herd      1     period3          period3
#>  4     1       herd      1     period4          period4
#>  5     1       herd     10 (Intercept)      (Intercept)
#>  6     1       herd     10     period2          period2
#>  7     1       herd     10     period3          period3
#>  8     1       herd     10     period4          period4
#>  9     1       herd     11 (Intercept)      (Intercept)
#> 10     1       herd     11     period2          period2
#> # ... with 5,990 more rows, and 4 more variables: .ranef_parameter <chr>,
#> #   .fixef_part <dbl>, .ranef_part <dbl>, .total <dbl>
draw_coef(m2, 100) %>% 
  select(.draw, .group_var, .group, .fixef_parameter, .total) %>% 
  tidyr::spread(.fixef_parameter, .total)
#> # A tibble: 1,500 x 7
#>    .draw .group_var .group `(Intercept)`   period2   period3   period4
#>  * <int>      <chr>  <chr>         <dbl>     <dbl>     <dbl>     <dbl>
#>  1     1       herd      1    -0.7951965 -1.142092 -1.033629 -1.900248
#>  2     1       herd     10    -2.0308748 -1.142092 -1.033629 -1.900248
#>  3     1       herd     11    -0.9627433 -1.142092 -1.033629 -1.900248
#>  4     1       herd     12    -2.3796051 -1.142092 -1.033629 -1.900248
#>  5     1       herd     13    -2.1757593 -1.142092 -1.033629 -1.900248
#>  6     1       herd     14    -0.3619617 -1.142092 -1.033629 -1.900248
#>  7     1       herd     15    -1.4694316 -1.142092 -1.033629 -1.900248
#>  8     1       herd      2    -1.6140949 -1.142092 -1.033629 -1.900248
#>  9     1       herd      3    -1.3508801 -1.142092 -1.033629 -1.900248
#> 10     1       herd      4    -0.9866125 -1.142092 -1.033629 -1.900248
#> # ... with 1,490 more rows
```

We can visually confirm that `coef()` uses the posterior median values.

``` r
all_coefs <- draw_coef(m2)

medians <- coef(m2) %>% 
  lapply(tibble::rownames_to_column, ".group") %>% 
  bind_rows(.id = ".group_var") %>% 
  tidyr::gather(.term, .posterior_median, -.group_var, -.group)
  
ggplot(all_coefs) + 
  aes(x = interaction(.group_var, .group), y = .total) + 
  stat_summary(fun.data = median_hilow) +
  facet_wrap(".term") + 
  geom_point(aes(y = .posterior_median), data = medians, color = "red")
```

![](fig/README-test-coef-1.png)

`draw_var_corr()` provides an analogue to `VarCorr()`. Let's compare the prior distribution of correlation terms using different settings for `decov()` prior.

``` r
sleep <- lme4::sleepstudy

m0 <- stan_glmer(
  Reaction ~ Days + (Days | Subject),
  data = sleep,
  prior_covariance = decov(.25, 1, 1), 
  prior = normal(0, 1),
  prior_PD = TRUE,
  chains = 1)
#> trying deprecated constructor; please alert package maintainer

m1 <- stan_glmer(
  Reaction ~ Days + (Days | Subject),
  data = sleep,
  prior_covariance = decov(1, 1, 1), 
  prior = normal(0, 1),
  prior_PD = TRUE,
  chains = 1)
#> trying deprecated constructor; please alert package maintainer

m2 <- stan_glmer(
  Reaction ~ Days + (Days | Subject),
  data = sleep,
  prior_covariance = decov(2, 1, 1), 
  prior = normal(0, 1),
  prior_PD = TRUE,
  chains = 1)
#> trying deprecated constructor; please alert package maintainer

m3 <- stan_glmer(
  Reaction ~ Days + (Days | Subject),
  data = sleep,
  prior_covariance = decov(4, 1, 1), 
  prior = normal(0, 1),
  prior_PD = TRUE,
  chains = 1)
#> trying deprecated constructor; please alert package maintainer

m4 <- stan_glmer(
  Reaction ~ Days + (Days | Subject),
  data = sleep,
  prior_covariance = decov(8, 1, 1), 
  prior = normal(0, 1),
  prior_PD = TRUE,
  chains = 1)
#> trying deprecated constructor; please alert package maintainer
```

`VarCorr()` returns the mean variance-covariance/standard-deviation-correlation values.

``` r
VarCorr(m3)
#>  Groups   Name        Std.Dev. Corr 
#>  Subject  (Intercept) 85793.6       
#>           Days        67580.4  0.551
#>  Residual              4494.7

as.data.frame(VarCorr(m3))
#>        grp        var1 var2       vcov        sdcor
#> 1  Subject (Intercept) <NA> 7360535973 8.579357e+04
#> 2  Subject        Days <NA> 4567104806 6.758036e+04
#> 3  Subject (Intercept) Days 3194777937 5.510176e-01
#> 4 Residual        <NA> <NA>   20202148 4.494680e+03
```

In this function we compute the sd-cor matrix for each posterior sample.

``` r
vcs0 <- draw_var_corr(m0)
vcs1 <- draw_var_corr(m1)
vcs2 <- draw_var_corr(m2)
vcs3 <- draw_var_corr(m3)
vcs4 <- draw_var_corr(m4)

vcs3
#> # A tibble: 3,000 x 7
#>    .draw                             .parameter     grp        var1  var2
#>    <int>                                  <chr>   <chr>       <chr> <chr>
#>  1    73 Sigma[Subject:(Intercept),(Intercept)] Subject (Intercept)  <NA>
#>  2    73               Sigma[Subject:Days,Days] Subject        Days  <NA>
#>  3    73        Sigma[Subject:Days,(Intercept)] Subject (Intercept)  Days
#>  4   243 Sigma[Subject:(Intercept),(Intercept)] Subject (Intercept)  <NA>
#>  5   243               Sigma[Subject:Days,Days] Subject        Days  <NA>
#>  6   243        Sigma[Subject:Days,(Intercept)] Subject (Intercept)  Days
#>  7   600 Sigma[Subject:(Intercept),(Intercept)] Subject (Intercept)  <NA>
#>  8   600               Sigma[Subject:Days,Days] Subject        Days  <NA>
#>  9   600        Sigma[Subject:Days,(Intercept)] Subject (Intercept)  Days
#> 10   310 Sigma[Subject:(Intercept),(Intercept)] Subject (Intercept)  <NA>
#> # ... with 2,990 more rows, and 2 more variables: vcov <dbl>, sdcor <dbl>
```

Filter to just the correlations.

``` r
cors <- bind_rows(
  vcs0 %>% mutate(model = "decov(.25, 1, 1)"),
  vcs1 %>% mutate(model = "decov(1, 1, 1)"),
  vcs2 %>% mutate(model = "decov(2, 1, 1)"),
  vcs3 %>% mutate(model = "decov(4, 1, 1)"),
  vcs4 %>% mutate(model = "decov(8, 1, 1)")) %>% 
  filter(!is.na(var2))

ggplot(cors) + 
  aes(y = sdcor, x = .parameter, color = model) + 
  stat_summary(fun.data = median_hilow, geom = "linerange", 
               fun.args = list(conf.int = .9), 
               position = position_dodge(-.8)) + 
  stat_summary(fun.data = median_hilow, size = 1.5, geom = "linerange", 
               fun.args = list(conf.int = .7), show.legend = FALSE,
               position = position_dodge(-.8)) + 
  stat_summary(fun.data = median_hilow, size = 2.5, geom = "linerange", 
               fun.args = list(conf.int = .5), show.legend = FALSE,
               position = position_dodge(-.8)) + 
  coord_flip() + 
  labs(x = NULL, y = "correlation", color = "prior") + 
  ggtitle("Degrees of regularization with LKJ prior")
#> Warning: position_dodge requires non-overlapping x intervals

#> Warning: position_dodge requires non-overlapping x intervals

#> Warning: position_dodge requires non-overlapping x intervals
```

![](fig/README-var-corr-1.png)

``` r

ggplot(cors) + 
  aes(x = sdcor, color = model) + 
  stat_density(geom = "line", adjust = .2) + 
  facet_wrap(".parameter")
```

![](fig/README-var-corr-2.png)
