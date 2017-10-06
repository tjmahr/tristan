#' Draw posterior samples from mixed effects models
#'
#' `draw_fixef()`, `draw_ranef()`, and `draw_coef()` are analogous to the
#' functions [lme4::fixef()], [lme4::ranef()], and [lme4::coef()] expect that
#' they return a tidy data-frame and return posterior samples.
#'
#' @param model a mixed effects model fit with rstanarm
#' @param nsamples the number of posterior samples to draw. Defaults to all
#'   samples.
#' @return a long/tidy [tibble::tibble()]. Each row contains a posterior sample
#'   of a parameter.
#' @rdname mixed-effects
#' @export
draw_fixef <- function(model, nsamples = NULL) {
  to_sample <- get_sample_nums(model, nsamples)
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
#' @rdname mixed-effects
draw_ranef <- function(model, nsamples = NULL) {
  to_sample <- get_sample_nums(model, nsamples)
  draw_these_ranef(model, to_sample)
}

draw_these_ranef <- function(model, rows) {
  # code from here is adapted from rstanarm
  all_names <- model$stanfit@sim$fnames_oi

  re_names <- all_names %>% stringr::str_which("^b\\[")
  re_names <- all_names[re_names] %>% str_omit("_NEW_")

  draws <- as.data.frame(model, pars = re_names)
  ans <- draws[rows, , drop = FALSE]

  fl <- model$glmod$reTrms$flist
  levs <- lapply(fl, levels)
  cnms <- get_cnms(model)

  nc <- vapply(cnms, length, 1L)
  nb <- nc * vapply(levs, length, 1L)
  nbseq <- rep.int(seq_along(nb), nb)

  # code from here is my own
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


#' @export
#' @rdname mixed-effects
draw_coef <- function(model, nsamples = NULL) {
  to_sample <- get_sample_nums(model, nsamples)

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


# The functionality for the var_corr functions is split over several functions.
#
# The main tasks are extracting the names and lengths of the random effect
# parameters (`get_varr_corr_info()`), computing the var_corr matrices
# (`create_var_corr()` for an individual var_corr matrix and
# `compute_var_corr()` vectorized over a list), and optionally converting to an
# lme4 `VarCorr` object `convert_to_varcorr_object()`.
#
# For `compute_var_corr()`, the input is a vector of values. It could be a row
# of the matrix of posterior samples or a vector with the column means of the
# posterior matrix. `VarCorr.stanreg()` uses the column means, so we can
# replicate that functionality. Indeed, that's what we do in the unit test for
# these functions: Compare `compute_var_corr()` and
# `convert_to_varcorr_object()` on the posterior means to values returned by
# `VarCorr()`.

#' @export
#' @rdname mixed-effects
draw_var_corr <- function(model, nsamples = NULL) {
  to_sample <- get_sample_nums(model, nsamples)
  info <- get_var_corr_info(model)

  post_matrix <- as.matrix(model, pars = info$sigma_pars)
  # sigma_draws <- post_matrix[to_sample, info$sigma_pars]

  df <- to_sample %>%
    purrr::map(~ post_matrix[.x, , drop = TRUE]) %>%
    purrr::map(compute_var_corr, info = info) %>%
    lapply(convert_to_varcorr_object, model = model) %>%
    lapply(as.data.frame) %>%
    purrr::map2_dfr(to_sample,
                    ~ tibble::add_column(.x, .draw = .y, .before = 0)) %>%
    tibble::as_tibble()

  long_vcov <- post_matrix %>%
    reshape2::melt(varnames = c(".draw", ".parameter"), value.name = "vcov")
  long_vcov$.parameter <- as.character(long_vcov$.parameter)

  df %>%
    inner_join(long_vcov, by = c(".draw", "vcov"))  %>%
    select(one_of(".draw", ".parameter", "grp", "var1", "var2",
                  "vcov", "sdcor"))
}

# Get all the information needed to reshape a vector of variance
# values into a VarrCorr matrix
get_var_corr_info <- function(model) {
  sigmas <- get_var_corr_names(model)
  cnames <- get_cnms(model)
  n_cols <- vapply(cnames, length, 1L)
  group_names <- as.list(names(cnames))
  n_cov_var_terms <- (n_cols * (n_cols + 1)) / 2
  level_indices <- rep.int(seq_along(n_cov_var_terms), n_cov_var_terms)

  list(sigma_pars = sigmas,
       cnames = cnames,
       group_names = group_names,
       level_indices = level_indices)
}


# Compute a list of variance-covariance matrices
compute_var_corr <- function(values, info) {
  var_values <- values %>% split(info$level_indices)

  Map(create_var_corr, var_values, info$cnames) %>%
    set_names(info$group_names)
}

# Compute a single variance-covariance matrix
create_var_corr <- function(values, cnames) {
  cols <- length(cnames)

  covmat <- matrix(0, cols, cols)
  covmat[lower.tri(covmat, diag = TRUE)] <- values
  upper_tri <- t(covmat)
  diag(upper_tri) <- 0
  covmat <- covmat + upper_tri
  rownames(covmat) <- colnames(covmat) <- cnames
  stddev <- sqrt(diag(covmat))
  corr <- cov2cor(covmat)
  structure(covmat, stddev = stddev, correlation = corr)
}

# Package a list of variance-covariance matrices into a VarCorr object
convert_to_varcorr_object <- function(var_corr_list, model) {
  sc <- 1
  uses_sigma <- has_sigma_term(model)
  if (uses_sigma) sc <- as.matrix(model, pars = "sigma")

  structure(var_corr_list,
            sc = mean(sc),
            useSc = uses_sigma,
            class = "VarCorr.merMod")
}



str_omit <- function(string, pattern) {
  string[Negate(stringr::str_detect)(string, pattern)]
}

rep_each_along <- function(x, y) {
  each <- length(x) / length(y)
  rep(y, each = each)
}



## Functions for extracting things from rstanarm models

get_par_names <- function(model) {
  names(model$stanfit) %>%
    str_omit("^mean_PPD$") %>%
    str_omit("^log-posterior$") %>%
    str_omit("^b\\[.+:_NEW_.+\\]$")
}

# `cnms` is "a list of column names of the random effects according to the
# grouping factors", according to [lme4::mkReTrms].
get_cnms <- function(model) {
  model$glmod$reTrms$cnms
}

# `flist` is the "list of grouping factors used in the random-effects terms",
# according to [lme4::mkReTrms]
get_flist <- function(model) {
  model$glmod$reTrms$flist
}

has_sigma_term <- function(model) {
  model %>%
    get_par_names() %>%
    stringr::str_detect("^sigma$") %>%
    any()
}

get_var_corr_names <- function(model) {
  model %>%
    get_par_names() %>%
    stringr::str_subset("^Sigma\\[")
}

get_sample_nums <- function(model, nsamples = NULL) {
  avail_draws <- get_posterior_sample_size(model)
  all_draws <- seq_len(avail_draws)

  nsamples <- nsamples %||% avail_draws
  sample(all_draws, size = nsamples, replace = FALSE)
}

get_posterior_sample_size <- function(model) {
  sum(model$stanfit@sim$n_save)
}

