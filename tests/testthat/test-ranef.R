context("random effects")
library(rstanarm)

stan_glmer <- function(...) {
  purrr::quietly(rstanarm::stan_glmer)(...)[["result"]]
}
cbpp <- lme4::cbpp

# model <- stan_glmer(
#   cbind(incidence, size - incidence) ~ period + (1 | herd),
#   data = cbpp, family = binomial,
#   prior_covariance = decov(4, 1, 1),
#   prior = normal(0, 1),
#   chains = 1, iter = 100
# )
#
# model2 <- stan_glmer(
#   cbind(incidence, size - incidence) ~ 1 + (period | herd),
#   data = cbpp, family = binomial,
#   prior_covariance = decov(4, 1, 1),
#   prior = normal(0, 1),
#   chains = 2, iter = 100
# )

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
    dplyr::filter(.term == "(Intercept)", .group_var == "herd", .group == 1) %>%
    dplyr::pull(.posterior_value)

  by_hand <- as.data.frame(model3)[1:50, "b[(Intercept) herd:1]"] %>% unlist()
  expect_equal(from_ranef, by_hand)

  from_ranef <- draw_these_ranef(model4, 1:50) %>%
    dplyr::filter(.term == "(Intercept)", .group_var == "herd:period", .group == "1:1") %>%
    dplyr::pull(.posterior_value)

  by_hand <- as.data.frame(model4)[1:50, "b[(Intercept) herd:period:1:1]"] %>% unlist()
  expect_equal(from_ranef, by_hand)

  results <- draw_these_ranef(model4, 1:10)

  stringr::str_detect(results$.parameter, results$.group_var) %>%
    all() %>%
    testthat::expect_true()

  stringr::str_detect(results$.parameter, results$.term) %>%
    all() %>%
    testthat::expect_true()

  stringr::str_detect(results$.parameter, results$.group) %>%
    all() %>%
    testthat::expect_true()
})
