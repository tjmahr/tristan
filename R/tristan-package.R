#' Tristan's helper functions for RStanARM models and MCMC samples
#'
#' @name tristan
#' @import rlang dplyr
#' @importFrom stats cov2cor median model.frame quantile
#' @docType package
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Create a long data-frame of posterior predictions
#'
#' These functions are wrapper around RStanARM's prediction functions that
#' return a long tidy dataframe of model predictions.
#' `augment_posterior_predict()` wraps [rstanarm::posterior_predict()] and
#' `augment_posterior_linpred()` wraps [rstanarm::posterior_predict()].
#'
#' @param model a model fit with [RStanARM][rstanarm::rstanarm]
#' @param newdata a dataframe of new observations to generate predictions. If
#'   omitted or null, the original data-set used to fit the model is used for
#'   the predictions.
#' @param ... other arguments passed to [rstanarm::posterior_predict()] or
#' [rstanarm::posterior_linpred()].
#' @param nsamples number of samples to draw from the posterior. Defaults to the
#'   full number of samples.
#' @return a data-frame (a [tibble::tibble()]) of posterior predictions. The
#'   dataset used to generate the predictions is augmented with posterior
#'   predictions, so that there is row per posterior prediction per observation.
#'   Additionaly columns are included: `.observation` uniquely identifies rows
#'   from the prediction data-set, `.draw` is the posterior sample number, and
#'   `.posterior_value` is the predicted value for that `.observation` in that
#'   `.draw`.
#' @name augment_posterior
NULL



#' @export
#' @rdname augment_posterior
augment_posterior_predict <- function(model, newdata = NULL, ...,
                                      nsamples = NULL) {
  preddata <- prepare_pred_data(model, newdata)
  preds <- rstanarm::posterior_predict(model, newdata = preddata,
                                       draws = nsamples, ...)
  preds %>%
    reshape2::melt(varnames = c(".draw", ".observation"),
                   value.name = ".posterior_value") %>%
    select(one_of(c(".observation", ".draw", ".posterior_value"))) %>%
    inner_join(preddata, by = ".observation") %>%
    arrange(.data$.observation, .data$.draw) %>%
    tibble::as.tibble()
}

#' @export
#' @rdname augment_posterior
augment_posterior_linpred <- function(model, newdata = NULL, ...,
                                      nsamples = NULL) {
  preddata <- prepare_pred_data(model, newdata)
  preds <- rstanarm::posterior_linpred(model, newdata = preddata, ...)
  long_preds <- preds %>%
    reshape2::melt(varnames = c(".draw", ".observation"),
                   value.name = ".posterior_value") %>%
    select(one_of(c(".observation", ".draw", ".posterior_value")))

  if (!is.null(nsamples)) {
    samples <- sample(unique(long_preds$.draw), size = nsamples)
    long_preds <- long_preds[long_preds$.draw %in% samples, ]
    # Renumber the draws
    long_preds$.draw <- match(long_preds$.draw, samples)
  }

  long_preds %>%
    inner_join(preddata, by = ".observation") %>%
    arrange(.data$.observation, .data$.draw) %>%
    tibble::as.tibble()
}


prepare_pred_data <- function(model, newdata = NULL) {
  if (is.null(newdata)) {
    newdata <- model$data
  }

  # Number the observations in the data
  newdata[[".observation"]] <- seq_len(nrow(newdata))

  # Figure out which observations the model will handle by adding the
  # observation ID to the model formula
  temp <- modelr::add_predictors(stats::formula(model), ~ .observation)

  # Simplify any random-effect subformulas
  temp <- lme4::subbars(temp)

  # Don't need y values
  temp[2] <- NULL
  model_ready_data <- model.frame(temp, newdata)

  # Keep only obersvations that the model can generate new predictions for
  newdata[newdata$.observation %in% model_ready_data$.observation, ]
}





#' Create tidy dataframe for ggmcmc from an RStanARM model
#'
#' @param model a model fit with [RStanARM][rstanarm::rstanarm]
#' @param r2 whether to include R2 as a parameter value
#' @return a data-frame (a [tibble::tibble()]) of MCMC sampling information
#'   suitable for use with the [ggmcmc package][ggmcmc::ggmcmc].
#' @export
ggs_rstanarm <- function(model, r2 = FALSE) {
  stopifnot(inherits(model, "stanreg"))
  chains <- as.array(model)
  dimnames(chains)[["chains"]] <- seq_along(dimnames(chains)[["chains"]])
  long <- reshape2::melt(chains, c("Iteration", "Chain", "Parameter"), "value")
  long <- tibble::as.tibble(long)
  if (r2) {
    r2_steps <- dplyr::distinct_(long, ~ Iteration, ~ Chain)
    r2_steps <- dplyr::arrange_(r2_steps, ~ Chain, ~ Iteration)
    r2_steps$Parameter <- "R2"
    r2_steps$value <- calculate_model_r2(model)
    long <- rbind(long, r2_steps)
  }

  attr(long, "nParameters") <- length(unique(long$Parameter))

  # Copy the parameters that ggs would set and extract
  ggs <- ggmcmc::ggs(model)
  attr(long, "nChains") <- attr(ggs, "nChains")
  attr(long, "description") <- attr(ggs, "description")
  attr(long, "nThin") <- attr(ggs, "nThin")
  attr(long, "nBurnin") <- attr(ggs, "nBurnin")
  attr(long, "nIterations") <- attr(ggs, "nIterations")
  long
}

# For testing...
naive_augment_posterior_predict <- function(model, newdata, ..., nsamples = NULL) {
  if (is.null(newdata)) {
    newdata <- model$data
  }

  # Number the observations in the data
  newdata[[".observation"]] <- seq_len(nrow(newdata))

  # Do the predictions
  preds <- rstanarm::posterior_predict(model, newdata = newdata,
                                       draws = nsamples, ...)
  long_preds <- reshape2::melt(
    data = preds,
    varnames = c(".draw", ".observation"),
    value.name = ".posterior_value")

  long_preds <- long_preds[c(".observation", ".draw", ".posterior_value")]
  long_preds_w_data <- merge(long_preds, newdata, by = ".observation")
  tibble::as.tibble(long_preds_w_data)
}



