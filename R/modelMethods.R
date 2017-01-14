#' Coefficients of Bayesian Linear Model
#'
#' Reports the mean and covariance of the weights of a Bayesian linear model.
#'
#' @param object   An object of class 'blm'
#' @param ... Not used
#'
#' @return      A list of coefficients from the posterior distributions
#' @import stats
#' @export

coef.blm = function(object,...){
  if (all(!sapply(object[1:8], is.null)) == F) stop("'blm' object contains NULL values")
  else return(list(mean=object$mean, sigma=object$sigma))
}

#' Confidence Interval for Bayesian Linear Model Weights
#'
#' Calculates the confidence interval of the weights
#'
#' @param object    An object of class 'blm'
#' @param parm   A vector specifying which parameters to report CI's on.
#' @param level  Confidence level. default = 0.95
#' @param ... Not used
#'
#' @return A dataframe containing the weights and their upper and lower confidence limits.
#' @import stats
#' @export

confint.blm = function(object, parm, level = 0.95,...){
  if (all(!sapply(object[1:8], is.null)) == F) stop("'blm' object contains NULL values")
  else if (level <= 0 | level > 1) stop("Level must be larger than 0 and smaller than or equal to 1")
  else{
    weights = as.data.frame(object$mean)
    colnames(weights)="Weights"
    lower = (1-level)/2
    upper = level+(1-level)/2
    weights[,as.character(lower)] = 0
    weights[,as.character(upper)] = 0
    for (row in 1:nrow(weights)){
      weights[row, c(as.character(lower), as.character(upper))] = qnorm(c(lower, upper), mean = weights[row,1], sd = object$sigma[row,row])
    }
    return(weights[parm,])
  }
}

#' Deviance of Bayesian Linear Model Fit
#'
#' Reports the sum of squared residuals from a Bayesian linear model fit.
#'
#' @param object   An object of class 'blm'
#' @param ... Not used
#'
#' @return      A number
#' @import stats
#' @export

deviance.blm = function(object,...){
  if (all(!sapply(object[1:8], is.null)) == F) stop("'blm' object contains NULL values")
  else return(sum((residuals(object))^2))
}

#' Fitting a Bayesian Linear Model
#'
#' Predicts response variable values from the data used to fit a Bayesian linear model.
#'
#' @param object   An object of class 'blm'
#' @param ... Not used
#'
#' @return      A data frame containing the predicted values and their variance.
#' @import stats
#' @import MASS
#' @export

fitted.blm <- function(object,...) {
  if (all(!sapply(object[1:8], is.null)) == F) stop("'blm' object contains NULL values")
  else{
    phiX = responselessModelMatrix(object$model, object$data)
    mxy = object$mean
    Sxy = object$sigma

    fitResults=as.data.frame(matrix(nrow=nrow(phiX), ncol=0))

    for (i in 1:nrow(fitResults)){
      fitResults[i,"Prediction"] = t(mxy)%*%phiX[i,]
      fitResults[i,"Variance"] = 1/object$beta + (t(phiX[i,]) %*% Sxy %*% phiX[i,])
    }
    return(fitResults)
  }
}

#' Plot Bayesian Linear Model
#'
#' Plots the residuals vs the fitted values.
#'
#' @param x   An object of class 'blm'
#' @param ... Not used
#'
#' @return      A plot of residuals vs fitted values.
#' @import graphics
#' @export

plot.blm = function(x,...){
  if (all(!sapply(x[1:8], is.null)) == F) stop("'blm' object contains NULL values")
  else{
    d = data.frame(Pred = fitted(x)$Prediction, Res = residuals(x), Number = 1:nrow(residuals(x)))
    d = d[order(d$Pred),]
    plot(d$Pred, d$Res, type="l", col="red",
         main="Residuals vs. Fitted",
         xlab="Fitted response",
         ylab="Residuals")
    text(d$Pred, d$Res, d$Number)
  }
}

#' Predict Response from Bayesian Linear Model Fit
#'
#' Predict response of new data based on existing Bayesian linear model. It uses the old posterior distribution as a new prior distibution and then fits the new data by calling fitted().
#'
#' @param object   An object of class 'blm'
#' @param newdata A data frame containing data different from that used to build the blm. If not specified, fitted.blm() will be called. If newdata contains a response variable, this will be used to update the blm before fitting. default = NULL.
#' @param ... Not used
#'
#' @return see fitted.blm().
#' @import stats
#' @export

predict.blm = function(object, newdata = NULL, ...){
  if (all(!sapply(object[1:8], is.null)) == F) stop("'blm' object contains NULL values")
  else if (is.null(newdata)){
    #If no new data is given, call fitted():
    cat("newdata argument not specified, old data will be fitted:")
    return(fitted(object))

  } else if (length(all.vars(object$model))==ncol(newdata)+1){
    #If newdata does not contain repsponse variable, predict this:
    cat("Predicting response variable based on newdata:")
    phiX = responselessModelMatrix(object$model, newdata)
    mxy = object$mean
    Sxy = object$sigma

    prediction=as.data.frame(matrix(nrow=nrow(phiX), ncol=0))

    for (i in 1:nrow(prediction)){
      prediction[i,"Prediction"] = t(mxy)%*%phiX[i,]
      prediction[i,"Variance"] = 1/object$beta + (t(phiX[i,]) %*% Sxy %*% phiX[i,])
    }
    return(prediction)

  } else if (length(all.vars(object$model))==ncol(newdata)){
    # If newdata contains a response variable, use newdata to improve the blm:
    cat("newdata contains a response variable. Using new data to improve model fit:")
    newBlm = blm(object$model, object$posterior, object$beta, newdata)
    return(fitted(newBlm))
  }
}

#' Print a Bayesian Linear Model.
#'
#' Prints the function call that was made to fit the model as well as the coefficients (mean and covariance).
#'
#' @param x   An object of class 'blm'
#' @param ... Not used
#'
#' @return None.
#' @export

print.blm = function(x,...){
  writeLines("\nCall:")
  print(x$Call)
  writeLines("\nCoefficients:\nWeights (with 95% confidence interval):")
  print(confint(x), names(x$mean))
  writeLines("\nSigma:")
  print(coef(x)$sigma)
}


#' Residuals from Bayesian Linear Model Fit
#'
#' Calculates the difference between observed and predicted data.
#'
#' @param object   An object of class 'blm'
#' @param ... Not used
#'
#' @return      A vector of residuals.
#' @import stats
#' @export

residuals.blm = function(object, ...){
  if (all(!sapply(object[1:8], is.null)) == F) stop("'blm' object contains NULL values")
  else{
    responseVar = as.character(formula(object$model)[[2]])
    observed = object$data[, responseVar]
    predicted = fitted(object)$Prediction
    return(predicted-observed)
  }
}


#' Summary of Bayesian Linear Model
#'
#' Prints summary statistics for a blm fit: Residuals, Deviance.
#'
#' @param object   An object of class 'blm'
#' @param ... Not used
#'
#' @return None.
#' @export

summary.blm = function(object,...){
  print(object)
  writeLines("\nResiduals:")
  residual = t(residuals(object))
  colnames(residual)=1:ncol(residual)
  if (ncol(residual)>10){
    cat(residual[1:10], "...\n(Showing first 10 residuals)\n")
  }
  else print(residual)
  writeLines("\nDeviance:")
  print(deviance(object))
}
