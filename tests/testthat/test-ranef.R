context("mixed effects")
library(rstanarm)
library(dplyr)

stan_glmer <- function(...) {
  purrr::quietly(rstanarm::stan_glmer)(...)[["result"]]
}
stan_glm <- function(...) {
  purrr::quietly(rstanarm::stan_glm)(...)[["result"]]
}
cbpp <- lme4::cbpp

expect_string <- function(string, pattern) {
  stringr::str_detect(string, pattern) %>%
    all() %>%
    testthat::expect_true()
  invisible(string)
}

model0 <- stan_glm(
  Sepal.Length ~ .^2,
  data = iris,
  family = gaussian,
  prior = normal(0, 1),
  chains = 4,
  iter = 100
)

model1 <- stan_glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp, family = binomial,
  prior_covariance = decov(4, 1, 1),
  prior = normal(0, 1),
  chains = 1, iter = 100
)

model2 <- stan_glmer(
  cbind(incidence, size - incidence) ~ 1 + (period | herd),
  data = cbpp, family = binomial,
  prior_covariance = decov(4, 1, 1),
  prior = normal(0, 1),
  chains = 2, iter = 100
)

model3 <- stan_glmer(
  cbind(incidence, size - incidence) ~ 1 + (period | herd) + (1 | period),
  data = cbpp, family = binomial,
  prior_covariance = decov(4, 1, 1),
  prior = normal(0, 1),
  chains = 1, iter = 100
)

model4 <- stan_glmer(
  cbind(incidence, size - incidence) ~ 1 + (1 | period/herd),
  data = cbpp, family = binomial,
  prior_covariance = decov(4, 1, 1),
  prior = normal(0, 1),
  chains = 1, iter = 100
)

model <- stan_glmer(
  cbind(incidence, size - incidence) ~ period + (period | herd) + (1 | period),
  data = cbpp, family = binomial,
  prior_covariance = decov(4, 1, 1),
  prior = normal(0, 1),
  chains = 1, iter = 100
)


test_that("groups are correctly inferred", {
  from_ranef <- draw_these_ranef(model3, 1:50) %>%
    dplyr::filter(.term == "(Intercept)",
                  .group_var == "herd",
                  .group == 1) %>%
    dplyr::pull(.posterior_value)

  by_hand <- as.data.frame(model3)[1:50, "b[(Intercept) herd:1]"] %>% unlist()
  expect_equal(from_ranef, by_hand)

  from_ranef <- draw_these_ranef(model4, 1:50) %>%
    dplyr::filter(.term == "(Intercept)",
                  .group_var == "herd:period",
                  .group == "1:1") %>%
    dplyr::pull(.posterior_value)

  by_hand <- as.data.frame(model4)[1:50, "b[(Intercept) herd:period:1:1]"] %>%
    unlist()
  expect_equal(from_ranef, by_hand)

  results <- draw_these_ranef(model4, 1:10)

  results$.parameter %>%
    expect_string(results$.group_var) %>%
    expect_string(results$.group) %>%
    expect_string(results$.term)
})


test_that("median draw_fixef matches fixef", {
  pkg_fixef <- . %>%
    draw_fixef() %>%
    group_by(.parameter) %>%
    summarise(median = median(.posterior_value)) %>%
    arrange(.parameter) %>%
    {set_names(.$median, .$.parameter)}

  testthat::expect_equal(pkg_fixef(model1), fixef(model1))
  testthat::expect_equal(pkg_fixef(model2), fixef(model2))
  testthat::expect_equal(pkg_fixef(model3), fixef(model3))
  testthat::expect_equal(pkg_fixef(model4), fixef(model4))
})

test_that("median draw_coef matches coef", {
  tidy_coef <- . %>%
    coef() %>%
    lapply(tibble::rownames_to_column, ".group") %>%
    bind_rows(.id = ".group_var") %>%
    tidyr::gather(.term, .posterior_median, -.group_var, -.group)

  from_tristan <- . %>%
    draw_coef() %>%
    group_by(.group_var, .group, .term) %>%
    summarise_at(vars(.fixef_part, .ranef_part), median, na.rm = TRUE) %>%
    mutate(sum_medians = .fixef_part + .ranef_part)

  compare_sets <- function(m) {
    both <- tidy_coef(m) %>%
      left_join(from_tristan(m), by = c(".group_var", ".group", ".term"))
    testthat::expect_equal(both$sum_medians, both$.posterior_median)
  }

  compare_sets(model1)
  compare_sets(model2)
  compare_sets(model3)
  compare_sets(model4)
})

test_that("median draw_coef matches coef", {
  tidy_coef <- . %>%
    coef() %>%
    lapply(tibble::rownames_to_column, ".group") %>%
    bind_rows(.id = ".group_var") %>%
    tidyr::gather(.term, .posterior_median, -.group_var, -.group)

  from_tristan <- . %>%
    draw_coef() %>%
    group_by(.group_var, .group, .term) %>%
    summarise_at(vars(.fixef_part, .ranef_part), median, na.rm = TRUE) %>%
    mutate(sum_medians = .fixef_part + .ranef_part)

  compare_sets <- function(m) {
    both <- tidy_coef(m) %>%
      left_join(from_tristan(m), by = c(".group_var", ".group", ".term"))
    testthat::expect_equal(both$sum_medians, both$.posterior_median)
  }

  compare_sets(model1)
  compare_sets(model2)
  compare_sets(model3)
  compare_sets(model4)
})


test_that("get_par_names returns column names of posterior matrix", {
  for (m in list(model0, model1, model2, model3, model4)) {
    expect_equal(colnames(as.matrix(m)), get_par_names(m))
  }
})

test_that("has_sigma_term detects 'sigma' term", {
  expect_true(has_sigma_term(model0))
  expect_false(has_sigma_term(model1))
})

test_that("get_nsamples returns number of post-warmup draws", {
  for (m in list(model0, model1, model2, model3, model4)) {
    avail_draws <- attr(summary(m), "posterior_sample_size")
    testthat::expect_equal(get_posterior_sample_size(m), avail_draws)
  }
})

test_that("get_sample_nums defaults to all samples", {
  s1 <- get_sample_nums(model0)
  s2 <- get_sample_nums(model0, NULL)
  s3 <- get_sample_nums(model0, 10)

  testthat::expect_length(s1, get_posterior_sample_size(model0))
  testthat::expect_length(s2, get_posterior_sample_size(model0))
  testthat::expect_length(s3, 10)

  testthat::expect_length(unique(s1), get_posterior_sample_size(model0))
  testthat::expect_length(unique(s2), get_posterior_sample_size(model0))
  testthat::expect_length(unique(s3), 10)

})

test_that("compute_var_corr matches VarCorr output", {
  for (m in list(model1, model2, model3, model4)) {
    sigma <- as.matrix(m, pars = get_var_corr_names(m)) %>% colMeans()
    info <- get_var_corr_info(m)

    mean_var_corr <- compute_var_corr(sigma, info) %>%
      convert_to_varcorr_object(m)

    testthat::expect_equal(as.data.frame(mean_var_corr),
                           as.data.frame(VarCorr(m)))
  }

})

test_that("mean of draw_var_corr matches VarCorr output", {
  for (m in list(model1, model2, model3, model4)) {
    means <- draw_var_corr(m) %>%
      group_by(grp, var1, var2) %>%
      summarise(vcov = mean(vcov)) %>%
      ungroup()
    testthat::expect_equal(means,
                           as.data.frame(VarCorr(m)) %>% select(-sdcor))
  }
})

test_that("draw_var_corr samples variance values", {
  for (m in list(model1, model2, model3, model4)) {
    draws <- draw_var_corr(m, 5)
    df <- as.data.frame(m)

    for (i in seq_len(nrow(draws))) {
      row <- draws[[i, ".draw"]]
      col <- draws[[i, ".parameter"]]
      value <- draws[[i, "vcov"]]
      testthat::expect_equal(value, df[row, col])
    }
  }
})

