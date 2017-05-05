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
  newdata[["..obs"]] <- seq_len(nrow(newdata))

  # Figure out which observations the model will handle by adding the
  # observation ID to the model formula
  temp_formula <- modelr::add_predictors(formula(model), ~ ..obs)
  model_ready_data <- model.frame(temp_formula, newdata)

  # Keep only obersvations that the model can handle
  preddata <- newdata[newdata$..obs %in% model_ready_data$..obs, ]

  # Do the predictions
  preds <- rstanarm::posterior_linpred(model, newdata = preddata, ...)
  long_preds <- reshape2::melt(
    data = preds,
    varnames = c("PosteriorDraw", "..obs"),
    value.name = "PosteriorLinPred")

  long_preds <- long_preds[c("..obs", "PosteriorDraw", "PosteriorLinPred")]

  if (!is.null(nsamples)) {
    samples <- sample(unique(long_preds$PosteriorDraw), size = nsamples)
    long_preds <- long_preds[long_preds$PosteriorDraw %in% samples, ]
    # Renumber the draws
    long_preds$PosteriorDraw <- match(long_preds$PosteriorDraw, samples)
  }

  long_preds_with_data <- dplyr::left_join(long_preds, preddata, by = "..obs")

  tibble::as.tibble(long_preds_with_data)
}






#' Create a long data-frame of posterior predictions
#' @export
augment_posterior_predict <- function(model, newdata = NULL, ..., nsamples = NULL) {
  if (is.null(newdata)) {
    newdata <- model$data
  }

  # Number the observations in the data
  newdata[["..obs"]] <- seq_len(nrow(newdata))

  # Figure out which observations the model will handle by adding the
  # observation ID to the model formula
  temp_formula <- modelr::add_predictors(formula(model), ~ ..obs)
  model_ready_data <- model.frame(temp_formula, newdata)

  # Keep only obersvations that the model can handle
  preddata <- newdata[newdata$..obs %in% model_ready_data$..obs, ]

  # Do the predictions
  preds <- rstanarm::posterior_predict(model, newdata = preddata,
                                       draws = nsamples, ...)
  long_preds <- reshape2::melt(
    data = preds,
    varnames = c("PosteriorDraw", "..obs"),
    value.name = "PosteriorPred")

  long_preds <- long_preds[c("..obs", "PosteriorDraw", "PosteriorPred")]

  long_preds_with_data <- dplyr::left_join(long_preds, preddata, by = "..obs")

  tibble::as.tibble(long_preds_with_data)
}

