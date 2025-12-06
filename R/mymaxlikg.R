#' mymaxlikg
#'
#' @param lfun A string giving the name of the log-likelihood function to evaluate.
#' @param theta A numeric vector of parameter values over whihc the likliehood shoud be evaluated.
#'
#' @returns A numeric value giving the maximum likelihood estimate.
#' @export
#'
#' @examples
#' # mymaxlikg(lfun = "logbin2", theta = seq(0.01, 0.99, length = 100))
mymaxlikg = function(lfun = "logbin2", theta) {
  nth = length(theta)
  thmat = matrix(theta, nr = nth, nc = 1, byrow = TRUE)
  # Evaluate log-likelihood for each theta
  z = apply(thmat, 1, lfun)
  # Find index of maximum likelihood
  zmax = max(which(z == max(z)))
  # Plot likelihood (exp of log-likelihood)
  plot(theta, exp(z), type = "l",
       xlab = "p",
       ylab = "Likelihood",
       main = "Maximum Likelihood for p")
  # Vertical line at estimated p
  abline(v = theta[zmax], col = "blue", lwd = 2)
  # Mark the value on the top axis
  axis(3, theta[zmax], round(theta[zmax], 4))
  # Return MLE
  theta[zmax]
}
