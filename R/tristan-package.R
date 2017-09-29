#' Tristan's helper functions for RStanARM models and MCMC samples
#'
#' @name tristan
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
augment_posterior_predict <- function(model, newdata = NULL, ..., nsamples = NULL) {
  if (is.null(newdata)) {
    newdata <- model$data
  }

  # Number the observations in the data
  newdata[[".observation"]] <- seq_len(nrow(newdata))

  # Figure out which observations the model will handle by adding the
  # observation ID to the model formula
  temp <- modelr::add_predictors(stats::formula(model), ~ .observation)

  if (inherits(model, "lmerMod")) {
    merform <- imitate_mer_formula(temp, model, newdata)
    model_ready_data <- merform$fr
  } else {
    # Outcome variable not required
    temp[2] <- NULL
    model_ready_data <- model.frame(temp, newdata)
  }

  # Keep only observations that the model can handle
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
#' @rdname augment_posterior
augment_posterior_linpred <- function(model, newdata = NULL, ..., nsamples = NULL) {
  if (is.null(newdata)) {
    newdata <- model$data
  }

  # Number the observations in the data
  newdata[[".observation"]] <- seq_len(nrow(newdata))

  # Figure out which observations the model will handle by adding the
  # observation ID to the model formula
  temp <- modelr::add_predictors(stats::formula(model), ~ .observation)

  if (inherits(model, "lmerMod")) {
    merform <- imitate_mer_formula(temp, model, newdata)
    model_ready_data <- merform$fr
  } else {
    # Outcome variable not required
    temp[2] <- NULL
    model_ready_data <- model.frame(temp, newdata)
  }

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

imitate_mer_formula <- function(formula, model, data, ...) {
  if (length(model$weights) == 0) {
    model$weights <- NULL
  }

  args <- list(
    formula = formula,
    data = data,
    family = model$family,
    control = lme4::glmerControl(check.nlev.gtr.1 = "ignore",
                                 check.rankX = "ignore"))
  args <- c(args, offset = model$offset,
            contrasts = model$contrasts,
            weights = model$weights)
 do.call(lme4::glFormula, args)
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



#' @import rlang
#' @export
draw_fixef <- function(model, nsamples = NULL) {
  avail_draws <- attr(summary(model), "posterior_sample_size")
  to_sample <- sample(seq_len(nsamples %||% avail_draws))
  draw_these_fixef(model, to_sample)
}

draw_these_fixef <- function(model, rows) {
  to_draw <- names(rstanarm::fixef(model))
  draws <- as.data.frame(model, pars = to_draw)
  draws[rows, , drop = FALSE] %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column(".draw") %>%
    tidyr::gather(".parameter", ".posterior_value", -1)
}

#' @export
draw_ranef <- function(model, nsamples = NULL) {
  avail_draws <- attr(summary(model), "posterior_sample_size")
  to_sample <- sample(seq_len(nsamples %||% avail_draws))
  draw_these_ranef(model, to_sample)
}

draw_these_ranef <- function(model, rows) {
  all_names <- model$stanfit@sim$fnames_oi

  re_names <- all_names %>% stringr::str_which("^b\\[")
  re_names <- all_names[re_names] %>% str_omit("_NEW_")

  draws <- as.data.frame(model, pars = re_names)
  ans <- draws[rows, , drop = FALSE]

  fl <- model$glmod$reTrms$flist
  levs <- lapply(fl, levels)
  cnms <- model$glmod$reTrms$cnms

  nc <- vapply(cnms, length, 1L)
  nb <- nc * vapply(levs, length, 1L)
  nbseq <- rep.int(seq_along(nb), nb)

  group_vars <- split(nbseq, nbseq) %>%
    purrr::map2(names(fl), rep_along) %>%
    unname() %>%
    flatten_chr()

  groups <- split(nbseq, nbseq) %>%
    purrr::map2(levs, rep_each_along) %>%
    unname() %>%
    flatten_chr()

  terms <- split(nbseq, nbseq) %>%
    purrr::map2(cnms, rep_along) %>%
    unname() %>%
    flatten_chr()

  scheme <- tibble::data_frame(
    .group_var = group_vars,
    .group = groups,
    .term = terms,
    .parameter = names(ans))

  samples <- ans %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column(".draw") %>%
    tidyr::gather(".parameter", ".posterior_value", -".draw")

  dplyr::left_join(scheme, samples, by = ".parameter") %>%
    dplyr::select(dplyr::one_of(".draw", ".group_var", ".group", ".term",
                         ".parameter", ".posterior_value"))

}

str_omit <- function(string, pattern) {
  string[Negate(stringr::str_detect)(string, pattern)]
}

rep_each_along <- function(x, y) {
  each <- length(x) / length(y)
  rep(y, each = each)
}

#' @export
draw_coef <- function(model, nsamples = NULL) {
  avail_draws <- attr(summary(model), "posterior_sample_size")
  to_sample <- sample(seq_len(nsamples %||% avail_draws))

  fef <- draw_these_fixef(model, to_sample) %>%
    dplyr::rename(.term = .data$.parameter,
                  .fixef_part = .data$.posterior_value) %>%
    dplyr::mutate(.fixef_parameter = .data$.term)

  ref <- draw_these_ranef(model, to_sample) %>%
    dplyr::rename(
      .ranef_part = .data$.posterior_value,
      .ranef_parameter = .data$.parameter)

  terms <- tidyr::crossing(
    .draw  = ref$.draw,
    tidyr::nesting(.group_var = ref$.group_var, .group = ref$.group),
    .term = c(ref$.term, fef$.term)
  )

  terms %>%
    dplyr::left_join(ref, by = c(".draw", ".group_var", ".group", ".term")) %>%
    dplyr::left_join(fef, by = c(".draw", ".term")) %>%
    tidyr::replace_na(list(.ranef_part = 0, .fixef_part = 0)) %>%
    dplyr::mutate(.total = .data$.ranef_part + .data$.fixef_part) %>%
    dplyr::select(dplyr::one_of(".draw", ".group_var", ".group", ".term",
                                ".fixef_parameter", ".ranef_parameter",
                                ".fixef_part", ".ranef_part", ".total"))
}


