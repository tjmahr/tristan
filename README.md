
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

The `ggs()` function in the ggmcmc package produces a tidy dataframe of MCMC samples. That function doesn't return the original parameter names for RStanARM models. `ggs_rstanarm()` rectifies this problem.

Example
-------

Fit a simple linear model.

``` r
library(tidyverse)
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
#> (Intercept)                       0.0    0.6  
#> z.Petal.Length                    0.8    0.5  
#> Speciesversicolor                -0.4    0.6  
#> Speciesvirginica                 -1.3    0.7  
#> z.Petal.Length:Speciesversicolor  1.0    0.5  
#> z.Petal.Length:Speciesvirginica   1.3    0.5  
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
#>  1            1     1       -1.0003247          5.1         3.5
#>  2            1     2       -0.9800408          5.1         3.5
#>  3            1     3       -1.0782613          5.1         3.5
#>  4            1     4       -1.1555150          5.1         3.5
#>  5            1     5       -1.0990496          5.1         3.5
#>  6            1     6       -0.9977267          5.1         3.5
#>  7            1     7       -1.1127216          5.1         3.5
#>  8            1     8       -0.9554037          5.1         3.5
#>  9            1     9       -1.0018127          5.1         3.5
#> 10            1    10       -1.0210712          5.1         3.5
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
#>  1            1     1       -1.5160539  setosa      -1.587834        0.955
#>  2            1     2       -1.1513862  setosa      -1.587834        0.955
#>  3            1     3       -1.2457199  setosa      -1.587834        0.955
#>  4            1     4       -1.9700178  setosa      -1.587834        0.955
#>  5            1     5       -1.4067362  setosa      -1.587834        0.955
#>  6            1     6       -1.3449140  setosa      -1.587834        0.955
#>  7            1     7       -1.2115145  setosa      -1.587834        0.955
#>  8            1     8       -0.8762633  setosa      -1.587834        0.955
#>  9            1     9       -1.5883634  setosa      -1.587834        0.955
#> 10            1    10       -1.3116756  setosa      -1.587834        0.955
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

### ggmc support

ggmc provides [a lot of magic](http://xavier-fim.net/packages/ggmcmc/#importing-mcmc-samples-into-ggmcmc-using-ggs). The general ggmcmc workflow is to create a tidy dataframe using `ggs()` and plug it that into the package's plotting functions. For example, here is how we can inspect the each parameter value using histograms and interval plots.

``` r
library(ggmcmc)
gg_model <- ggs(model)
  
# Facet wrap so that values are in a grid, not a single column.
ggs_histogram(gg_model) + 
  facet_wrap("Parameter", scales = "free_x")

# Remap y-aesthetic so that the parameters read from top-to-bottom in their
# original factor ordering.
ggs_caterpillar(gg_model, line = 0) + 
  aes(y = forcats::fct_rev(Parameter)) + 
  ylab(NULL)
```

![](fig/README-ggmc-no-name-1.png)![](fig/README-ggmc-no-name-2.png)

The package is magically convenient. But look, the plot lost the names of parameters from the model!

`ggs_rstanarm()` is a small function that imitates the output of `ggs()` but tries to keep the original parameter names. It also drops the non-parameter `"mean_PPD"` (the model's prediction for a completely average observation.)

``` r
gg_model2 <- ggs_rstanarm(model)

ggs_histogram(gg_model2) + 
  facet_wrap("Parameter", scales = "free_x")

ggs_caterpillar(gg_model2, line = 0) + 
  aes(y = forcats::fct_rev(Parameter)) + 
  ylab(NULL)
```

![](fig/README-ggmc-yes-name-1.png)![](fig/README-ggmc-yes-name-2.png)

### Calculate *R*<sup>2</sup>

`calculate_model_r2()` returns the unadjusted *R*<sup>2</sup> for each draw of the posterior distribution.

``` r
df_r2 <- data_frame(
  R2 = calculate_model_r2(model)
)
df_r2
#> # A tibble: 4,000 x 1
#>           R2
#>        <dbl>
#>  1 0.8269203
#>  2 0.8260345
#>  3 0.8221425
#>  4 0.8270270
#>  5 0.8185265
#>  6 0.8261973
#>  7 0.8279846
#>  8 0.8254305
#>  9 0.8270019
#> 10 0.8320477
#> # ... with 3,990 more rows
```

### Interval calculation

Two more helper functions compute tidy data-frames of posterior density intervals. `tidy_hpdi()` provides the highest-density posterior interval for model parameters, while `tidy_etdi()` computes the equal-tailed density intervals (the typical sort of intervals used for uncertain intervals.)

``` r
tidy_hpdi(model)
#> # A tibble: 7 x 5
#>                               term interval density       lower      upper
#>                              <chr>    <chr>   <dbl>       <dbl>      <dbl>
#> 1                      (Intercept)     HPDI     0.9 -0.96662031  1.1132883
#> 2                   z.Petal.Length     HPDI     0.9  0.04365789  1.6235364
#> 3                Speciesversicolor     HPDI     0.9 -1.43121311  0.6811740
#> 4                 Speciesvirginica     HPDI     0.9 -2.32824507 -0.1798205
#> 5 z.Petal.Length:Speciesversicolor     HPDI     0.9  0.14606964  1.8282612
#> 6  z.Petal.Length:Speciesvirginica     HPDI     0.9  0.49959901  2.2286074
#> 7                            sigma     HPDI     0.9  0.37047300  0.4492886
tidy_etdi(model)
#> # A tibble: 7 x 5
#>                               term interval density       lower      upper
#>                              <chr>    <chr>   <dbl>       <dbl>      <dbl>
#> 1                      (Intercept)     ETDI     0.9 -1.04985921  1.0485166
#> 2                   z.Petal.Length     ETDI     0.9 -0.03029808  1.5747382
#> 3                Speciesversicolor     ETDI     0.9 -1.45587558  0.6632600
#> 4                 Speciesvirginica     ETDI     0.9 -2.33831759 -0.1846349
#> 5 z.Petal.Length:Speciesversicolor     ETDI     0.9  0.14595095  1.8280213
#> 6  z.Petal.Length:Speciesvirginica     ETDI     0.9  0.47150891  2.2089550
#> 7                            sigma     ETDI     0.9  0.37497544  0.4553001
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
#> 1 calculate_model_r2(model)     HPDI     0.9 0.8231292 0.8348043
#> 2 calculate_model_r2(model)     ETDI     0.9 0.8208520 0.8339865
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
#>                               term outer_lower inner_lower     estimate
#>                              <chr>       <dbl>       <dbl>        <dbl>
#> 1        calculate_model_r2(model)  0.81820677  0.82085205  0.829644851
#> 2                      (Intercept) -1.25373326 -1.04985921  0.006368439
#> 3                   z.Petal.Length -0.19771497 -0.03029808  0.776595364
#> 4                Speciesversicolor -1.62467859 -1.45587558 -0.397443230
#> 5                 Speciesvirginica -2.53694541 -2.33831759 -1.257934858
#> 6 z.Petal.Length:Speciesversicolor -0.03191119  0.14595095  0.981728606
#> 7  z.Petal.Length:Speciesvirginica  0.31518660  0.47150891  1.326612423
#> 8                            sigma  0.36959972  0.37497544  0.411405525
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
