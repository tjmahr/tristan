#' Tristan's helper functions for Stan samples
#'
#' @name tristan
#' @docType package
NULL


#' Create a long data-frame of linear-posterior predictions
#' @export
augment_posterior_linpred <- function(model, newdata = NULL, ..., nsamples = NULL) {
  if (is.null(newdata)) {
    newdata <- model$data
  }

  # Number the observations in the data
  newdata[[".observation"]] <- seq_len(nrow(newdata))

  # Figure out which observations the model will handle by adding the
  # observation ID to the model formula
  temp_formula <- modelr::add_predictors(formula(model), ~ .observation)
  # Outcome variable not required
  temp_formula[2] <- NULL
  model_ready_data <- stats::model.frame(temp_formula, newdata)

  # Keep only obersvations that the model can handle
  preddata <- newdata[newdata$.observation %in% model_ready_data$.observation, ]

  # Do the predictions
  preds <- rstanarm::posterior_linpred(model, newdata = preddata, ...)
  long_preds <- reshape2::melt(
    data = preds,
    varnames = c(".draw", ".observation"),
    value.name = ".posterior_value")

  long_preds <- long_preds[c(".observation", ".draw", ".posterior_value")]

  if (!is.null(nsamples)) {
    samples <- sample(unique(long_preds$.draw), size = nsamples)
    long_preds <- long_preds[long_preds$.draw %in% samples, ]
    # Renumber the draws
    long_preds$.draw <- match(long_preds$.draw, samples)
  }

  long_preds_w_data <- merge(long_preds, preddata, by = ".observation")
  long_preds_w_data <- dplyr::arrange_(long_preds_w_data,
                                       ~ .observation, ~ .draw)
  tibble::as.tibble(long_preds_w_data)
}






#' Create a long data-frame of posterior predictions
#' @export
augment_posterior_predict <- function(model, newdata = NULL, ..., nsamples = NULL) {
  if (is.null(newdata)) {
    newdata <- model$data
  }

  # Number the observations in the data
  newdata[[".observation"]] <- seq_len(nrow(newdata))

  # Figure out which observations the model will handle by adding the
  # observation ID to the model formula
  temp_formula <- modelr::add_predictors(formula(model), ~ .observation)
  # Outcome variable not required
  temp_formula[2] <- NULL
  model_ready_data <- stats::model.frame(temp_formula, newdata)

  # Keep only obersvations that the model can handle
  preddata <- newdata[newdata$.observation %in% model_ready_data$.observation, ]

  # Do the predictions
  preds <- rstanarm::posterior_predict(model, newdata = preddata,
                                       draws = nsamples, ...)
  long_preds <- reshape2::melt(
    data = preds,
    varnames = c(".draw", ".observation"),
    value.name = ".posterior_value")

  long_preds <- long_preds[c(".observation", ".draw", ".posterior_value")]

  long_preds_w_data <- merge(long_preds, preddata, by = ".observation")

  tibble::as.tibble(long_preds_w_data)
}


#' @export
ggs_rstanarm <- function(model) {
  chains <- as.array(model)
  dimnames(chains)[["chains"]] <- seq_along(dimnames(chains)[["chains"]])
  long <- reshape2::melt(chains, c("Iteration", "Chain", "Parameter"), "value")

  long <- tibble::as.tibble(long)
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

