
#' Prior Distribution
#'
#' Constructs a prior distribution, based on a known precision. Parameters are named such that they match 'blm' objects.
#'
#'
#' @param model      A formula describing the model.
#' @param alpha      Precision
#'
#' @return           A list of the mean (always 0 for all parameters) and the covariance for the prior distribution.
#' @export

make_prior = function(model, alpha) {
  if (alpha <= 0) stop("alpha must be a positive nonzero number!")
  else return(list(sigma = diag(1/alpha, nrow = length(all.vars(model))), mean = rep(0, length(all.vars(model)))))
}


#' Bayesian linear model.
#'
#' Constructor for class 'blm'.
#' Calculates a posterior distribution of weights, given a model.
#'
#' @param model   A formula describing the model.
#' @param prior   A prior distribution in the form of a covariance matrix
#' @param beta    Beta, the precision
#' @param ...     Additional data, for example a data frame.
#'
#' @return A list of descriptive parameters of the prior and posterior distributions
#' @export

blm = function(model, prior, beta, ...) {
  if (beta <= 0) stop("beta must be a positive nonzero number!")
  else{
    posterior = update(model, prior, beta, ...)
    if (all(!sapply(posterior, is.null)) == F) stop("Posterior distribution contains NULL values, check your input!")
    else structure(list(sigma = posterior$sigma, mean = posterior$mean, beta = beta, data = posterior$data, model = model, prior = prior, posterior = posterior, Call = sys.call()), class = "blm")
  }
}


