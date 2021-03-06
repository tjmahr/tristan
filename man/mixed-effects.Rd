% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mer-samples.R
\name{draw_fixef}
\alias{draw_fixef}
\alias{draw_ranef}
\alias{draw_coef}
\alias{draw_var_corr}
\title{Draw posterior samples from mixed effects models}
\usage{
draw_fixef(model, nsamples = NULL)

draw_ranef(model, nsamples = NULL)

draw_coef(model, nsamples = NULL)

draw_var_corr(model, nsamples = NULL)
}
\arguments{
\item{model}{a mixed effects model fit with rstanarm}

\item{nsamples}{the number of posterior samples to draw. Defaults to all
samples.}
}
\value{
a long/tidy \code{\link[tibble:tibble]{tibble::tibble()}}. Each row contains a posterior sample
of a parameter.
}
\description{
\code{draw_fixef()}, \code{draw_ranef()}, and \code{draw_coef()} are analogous to the
functions \code{\link[lme4:fixef]{lme4::fixef()}}, \code{\link[lme4:ranef]{lme4::ranef()}}, and
\code{\link[lme4:merMod-class]{lme4:::coef.merMod()}} except that they return a tidy
data-frame and return posterior samples.
}
