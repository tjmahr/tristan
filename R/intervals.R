
#' Calculate posterior density intervals
#'
#' Calculate the highest posterior density interval (HPDI) or the equal-tailed
#' density interval (ETDI) of posterior samples.
#'
#' @name posterior-intervals
#' @param x a model fitted with RStanARM or a vector of MCMC samples
#' @param prob a probability content or width of the interval. Defaults to .9.
#' @return a data-frame (a [tibble::tibble()]) of intervals. Contains columns
#'   with the `term` name, `interval` type and `density`, and the `lower`
#'   and `upper` values of the interval.
#'
#' @details
#' The equal-tailed density interval is a term I made up for the traditional
#' sort of confidence or uncertainty interval. A 90% ETDI tells you the value at
#' the 5th and 95th percentiles. The interval contains 90% of the probability
#' density, and the left and right tails outside of the interval contain an
#' equal amount of probability density (5%). I use [stats::quantile()] to
#' compute the interval.
#'
#' The highest density posterior interval is the narrowest interval with a given
#' probability density. A 90% HPDI tells you the skinniest interval that
#' contains 90% of the probability density. The left and right tails outside of
#' the interval might contain unequal amounts of probability density. I use
#' [coda::HPDinterval()] to compute these intervals.
NULL

#' @rdname posterior-intervals
#' @export
tidy_hpdi <- function(x, prob = .9) {
  UseMethod("tidy_hpdi")
}

#' @rdname posterior-intervals
#' @export
tidy_hpdi.numeric <- function(x, prob = .9) {
  label <- lazyeval::expr_text(x)
  x %>%
    as.matrix() %>%
    coda::as.mcmc() %>%
    coda::HPDinterval(prob) %>%
    convert_hpdi_to_df() %>%
    dplyr::mutate_(term = ~ label)
}

#' @rdname posterior-intervals
#' @export
tidy_hpdi.stanreg <- function(x, prob = .9) {
  x %>%
    as.matrix() %>%
    coda::as.mcmc() %>%
    coda::HPDinterval(prob) %>%
    convert_hpdi_to_df()
}

#' @rdname posterior-intervals
#' @export
tidy_hpdi.mcmc.list <- function(x, prob = .9) {
  x %>%
    flatten_mcmc_list() %>%
    coda::as.mcmc() %>%
    coda::HPDinterval(prob) %>%
    convert_hpdi_to_df()
}

convert_hpdi_to_df <- function(hpdi) {
  hpdi %>%
    as.data.frame() %>%
    set_colnames(c("lower", "upper")) %>%
    tibble::rownames_to_column("term") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_(
      density = ~ attr(hpdi, "Probability"),
      interval = ~ "HPDI") %>%
    dplyr::select_(~ term, ~ interval, ~ density, ~ lower, ~ upper)
}

flatten_mcmc_list <- function(mcmc_list) {
  # Stack the matrices from each chain on each other
  stacked <- do.call(rbind, lapply(mcmc_list, as.matrix))
  row.names(stacked) <- seq_len(nrow(stacked))
  stacked
}



#' @rdname posterior-intervals
#' @export
tidy_etdi <- function(x, prob = .9) {
  UseMethod("tidy_etdi")
}

#' @rdname posterior-intervals
#' @export
tidy_etdi.numeric <- function(x, prob = .9) {
  label <- lazyeval::expr_text(x)

  x %>%
    as.matrix() %>%
    create_etdi_df_from_matrix(prob) %>%
    dplyr::mutate_(term = ~ label)
}

#' @rdname posterior-intervals
#' @export
tidy_etdi.stanreg <- function(x, prob = .9) {
  x %>%
    as.matrix() %>%
    create_etdi_df_from_matrix(prob)
}

#' @rdname posterior-intervals
#' @export
tidy_etdi.mcmc.list <- function(x, prob = .9) {
  x %>%
    flatten_mcmc_list() %>%
    create_etdi_df_from_matrix(prob)
}

create_etdi_df_from_matrix <- function(x, prob) {
  purrr::map_df(prob, ~ create_one_etdi_df_from_matrix(x, prob = .x))
}

create_one_etdi_df_from_matrix <- function(x, prob) {
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)

  x %>%
    as.matrix() %>%
    apply(2, quantile, probs = probs) %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(c("lower", "upper")) %>%
    tibble::rownames_to_column("term") %>%
    tibble::as_tibble()  %>%
    dplyr::mutate_(density = ~ prob, interval = ~ "ETDI") %>%
    dplyr::select_(~ term, ~ interval, ~ density, ~ lower, ~ upper)
}

set_colnames <- `colnames<-`

tidy_median <- function(x) {

}



#' @export
tidy_median <- function(x, prob = .9) {
  UseMethod("tidy_median")
}

#' @export
tidy_median.numeric <- function(x) {
  label <- lazyeval::expr_text(x)

  x %>%
    as.matrix() %>%
    create_median_df_from_matrix() %>%
    dplyr::mutate_(term = ~ label)
}

#' @export
tidy_median.stanreg <- function(x) {
  x %>%
    as.matrix() %>%
    create_median_df_from_matrix()
}

#' @export
tidy_median.mcmc.list <- function(x) {
  x %>%
    flatten_mcmc_list() %>%
    create_median_df_from_matrix()
}

create_median_df_from_matrix <- function(x) {
  x %>%
    as.matrix() %>%
    apply(2, median) %>%
    tibble::enframe(name = "term", value = "estimate") %>%
    mutate(est_type = "median")
}

#' @export
double_etdi <- function(x, prob_outer, prob_inner) {
  UseMethod("double_etdi")
}


#' @export
double_etdi.default <- function(x, prob_outer, prob_inner) {
  outer <- tidy_etdi(x, prob_outer) %>%
    rename_(outer_lower = ~ lower,
            outer_upper = ~ upper,
            outer_density = ~ density) %>%
    select_(~ -interval)

  inner <- tidy_etdi(x, prob_inner) %>%
    rename_(inner_lower = ~ lower,
            inner_upper = ~ upper,
            inner_density = ~ density) %>%
    select_(~ -interval)

  tidy_median(x) %>%
    left_join(inner, by = "term") %>%
    left_join(outer, by = "term") %>%
    select_(~ term, ~ outer_lower, ~ inner_lower,
            ~ estimate, ~ inner_upper, ~ outer_upper, ~ everything())

}

#' @export
double_etdi.numeric <- function(x, prob_outer, prob_inner) {
  result <- double_etdi.default(x, prob_outer, prob_inner)
  result$term <- lazyeval::expr_text(x)
  result
}
