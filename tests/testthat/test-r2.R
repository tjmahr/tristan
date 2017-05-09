context("R2")

test_that("calculate_r2() matches summary.lm()", {
  m <- lm(mpg ~ wt * cyl, mtcars)
  from_lm <- summary(m)[["r.squared"]]
  from_me <- calculate_r2(mtcars$mpg, m$fitted.values)
  expect_equal(from_lm, from_me)
})
