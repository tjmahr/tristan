context("augment")

stan_glmer <- function(...) {
  purrr::quietly(rstanarm::stan_glmer)(...)[["result"]]
}

stan_glm <- function(...) {
  purrr::quietly(rstanarm::stan_glm)(...)[["result"]]
}

m1 <- stan_glm(Sepal.Length ~ Species, family = gaussian, iris,
               chains = 1, iter = 200)

m2 <- stan_glmer(Sepal.Length ~ 1 | Species, family = gaussian, iris,
                 chains = 1, iter = 200)

test_that("augment functions numbers observations in newdata", {
  newdata <- data.frame(Species = c("virginica", "setosa", "versicolor"))
  newdata$obs <- 1:3
  nsamples <- 20

  lp <- augment_posterior_linpred(m1, newdata, nsamples = nsamples)
  pp <- augment_posterior_predict(m1, newdata, nsamples = nsamples)

  mer_lp <- augment_posterior_linpred(m2, newdata, nsamples = nsamples)
  mer_pp <- augment_posterior_predict(m2, newdata, nsamples = nsamples)

  # .observation matches original obs numbers
  expect_equal(lp$.observation, lp$obs)
  expect_equal(pp$.observation, pp$obs)

  expect_equal(mer_lp$.observation, lp$obs)
  expect_equal(mer_pp$.observation, pp$obs)

  # each observation is repeated in each MCMC sample
  expect_equal(nrow(lp), nrow(newdata) * nsamples)
  expect_equal(nrow(pp), nrow(newdata) * nsamples)
  expect_equal(nrow(mer_lp), nrow(newdata) * nsamples)
  expect_equal(nrow(mer_pp), nrow(newdata) * nsamples)
})

test_that("augment functions default to original dataset", {
  nsamples <- 1

  lp <- augment_posterior_linpred(m1, nsamples = nsamples)
  pp <- augment_posterior_predict(m1, nsamples = nsamples)

  mer_lp <- augment_posterior_linpred(m2, nsamples = nsamples)
  mer_pp <- augment_posterior_predict(m2, nsamples = nsamples)

  # With a single sample, the columns of augmented posterior sample should match
  # columns of original dataset
  expect_equal(lp$Sepal.Length, iris$Sepal.Length)
  expect_equal(pp$Sepal.Length, iris$Sepal.Length)
  expect_equal(mer_lp$Sepal.Length, iris$Sepal.Length)
  expect_equal(mer_pp$Sepal.Length, iris$Sepal.Length)
})

test_that("augment functions ignore observations with missing predictors", {
  nsamples <- 1

  newdata <- data.frame(
    Species = c(NA, "virginica", NA, "setosa", NA, "versicolor"),
    obs = 1:6)
  can_predict <- c(2, 4, 6)

  lp <- augment_posterior_linpred(m1, newdata, nsamples = nsamples)
  pp <- augment_posterior_predict(m1, newdata, nsamples = nsamples)

  mer_lp <- augment_posterior_linpred(m2, newdata, nsamples = nsamples)
  mer_pp <- augment_posterior_predict(m2, newdata, nsamples = nsamples)

  # .observation matches original obs numbers
  expect_equal(lp$.observation, can_predict)
  expect_equal(pp$.observation, can_predict)

  expect_equal(mer_lp$.observation, can_predict)
  expect_equal(mer_pp$.observation, can_predict)
})
