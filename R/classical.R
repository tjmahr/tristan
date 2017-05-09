

#' Refit a simple RStanARM model with a classical version
#' @name stan_to_classical
#' @param model a model fit with [rstanarm::stan_glm()] or
#' [rstanarm::stan_lm()].
#' @return the corresponding model fit with [stats::glm()] or
#' [stats::lm()].
NULL


#' @rdname stan_to_classical
#' @export
stan_to_glm <- function(model) {
  message("Please manually fit model if original model used any ",
          "\narguments besides `formula`, `family`, and `data`.")
  stopifnot(model$modeling_function == "stan_glm")

  stats::glm(
    stats::formula(model),
    family = model$family,
    data = model$data,
    weights = if(length(model$weights) == 0) NULL else model$weights,
    offset = model$offset
  )
}


#' @rdname stan_to_classical
#' @export
stan_to_lm <- function(model) {
  message("Please manually fit model if original model used any ",
          "\narguments besides `formula` and `data`.")

  stopifnot(model$modeling_function %in% c("stan_glm", "stan_lm"))

  if (model$modeling_function == "stan_glm") {
    stopifnot(
      model$family$link == "identity",
      model$family$family == "gaussian")
  }

  stats::lm(
    stats::formula(model),
    data = model$data,
    weights = if(length(model$weights) == 0) NULL else model$weights,
    offset = model$offset
  )
}
