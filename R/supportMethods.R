#' Responseless model matrix
#'
#' Builds a model matrix from data, excluding the response variable.
#' Support function for update() to build the phiX matrix.
#'
#' @param model      A formula describing the model.
#' @param dataframe  A dataframe with the data
#'
#' @return           A model matrix without a response variable.
#' @import stats

responselessModelMatrix = function(model, dataframe){
  responseless.formula <- delete.response(terms(model))
  frame <- model.frame(responseless.formula, dataframe)
  model.matrix(responseless.formula, frame)
}

#' Posterior Distribution
#'
#' Updates the prior distribution using data to give the posterior distribution og weights.
#' Support method for class 'blm'
#'
#' @param model   A formula describing the model.
#' @param prior   A prior distribution, must have at least a covariance matrix named sigma
#' @param beta    The precision
#' @param ...     Additional data, for example a data frame.
#'
#' @return        A list contating the mean and the covariance matrix for the posterior distribution as well as the original data packed into a data frame.
#' @import stats

update = function(model, prior, beta, ...) {
  if (beta <= 0) stop("beta must be a positive nonzero number!")
  else{
    data = model.frame(model, as.data.frame(list(...)))
    phiX = responselessModelMatrix(model, ...)
    Sxy = solve(prior$sigma + beta * t(phiX) %*% phiX) #The covariance
    responseVar = as.character(formula(model)[[2]]) #Extract the name of the response variable
    mxy = beta * Sxy %*% t(phiX) %*% data[, responseVar] #the mean
    return(list(sigma = Sxy, mean=mxy, data=data))
  }
}