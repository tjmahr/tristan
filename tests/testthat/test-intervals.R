context("intervals")

test_that("ETDIs match ggmcmc results", {
  data(line, package = "coda")

  my_ci <- tidy_etdi(line)
  ggmcmc_ci <- ggmcmc::ci(ggmcmc::ggs(line), thick_ci = c(0.05, 0.95))

  expect_equal(my_ci$lower, ggmcmc_ci$Low)
  expect_equal(my_ci$upper, ggmcmc_ci$High)

  library(rstanarm)
  fit <- purrr::quietly(stan_lm)(kid_score ~ mom_hs * mom_iq, data = kidiq,
                 prior = R2(location = 0.30, what = "mean"),
                 # the next line is only to make the example go fast enough
                 chains = 1, iter = 500, seed = 12345, show_messages = FALSE)
  fit <- fit$result

  my_ci <- tidy_etdi(fit)
  ggmcmc_ci <- ggmcmc::ci(ggs_rstanarm(fit), thick_ci = c(0.05, 0.95))

  expect_equal(my_ci$lower, ggmcmc_ci$Low)
  expect_equal(my_ci$upper, ggmcmc_ci$High)
})
