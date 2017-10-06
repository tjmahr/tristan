

#' Calculate classical r-squared from a model
#' @param model an RStanARM model
#' @return the rsquared value for each posterior sample
#' @export
calculate_model_r2 <- function(model) {
  fits <- rstanarm::posterior_linpred(model, transform = FALSE)
  ys <- model$y

  r2 <- apply(fits, 1, function(y_fits) calculate_r2(ys, y_fits))
  r2
}

#' Calculate classical r-squared from observations and predictions
#' @param y_obs observed y values
#' @param y_fits fitted y values
#' @return the rsquared value
#' @export
calculate_r2 <- function(y_obs, y_fits) {
  stopifnot(length(y_obs) == length(y_fits))
  # (z <- crossprod(1:4))    # = sum(1 + 2^2 + 3^2 + 4^2)
  resid_ss <- as.vector(crossprod(y_obs - y_fits))
  total_ss <- as.vector(crossprod(y_obs - mean(y_obs)))
  1 - (resid_ss / total_ss)
}


# Old versions of these functions I had developed. Checking them in case I need
# to revert to code that cannot rely on posterior_linpred()

# get_r2 <- function(model) {
#   no_sigmas <- model %>% coef() %>% names()
#   posterior_beta <- model %>% as.matrix(pars = no_sigmas)
#   stopifnot(all(no_sigmas == colnames(posterior_beta)))
#
#   xs <- model.matrix(model)
#   ys <- model$y
#
#   r2 <- apply(posterior_beta, 1, get_one_r2, xs, ys)
#   r2
# }
#
# get_one_r2 <- function(betas, xs, ys) {
#   stopifnot(colnames(xs) == names(betas))
#
#   mus <- xs %*% betas
#   resid_ss <- as.vector(crossprod(ys - mus))
#   total_ss <- as.vector(crossprod(ys - mean(ys)))
#   1 - (resid_ss / total_ss)
# }
#
