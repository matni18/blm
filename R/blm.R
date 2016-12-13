
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export


blm <- function(model, alpha, beta, data, ...) {
  phiX = model.matrix(model, data)
  Sxy = solve(diag(alpha, nrow=ncol(phiX)) + beta * t(phiX) %*% phiX)
  mxy = beta * Sxy %*% t(phiX) %*% model.response(model.frame(model, data))
  structure(list(sigma = Sxy, mean = mxy, data = data, model = model), class = "blm")
}
#Test:
#test =blm(y ~ x+I(x^2), 1,1,data.frame(x=x,y=y))
#predict(test)

predict.blm <- function(blmModel) {
  phiX = model.matrix(blmModel$model, blmModel$data)
  mxy = blmModel$mean
  Sxy = blmModel$sigma

  apply(phiX, 1, function(row) t(mxy)%*%row)

}

