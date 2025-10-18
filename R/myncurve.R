#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#'
#' @returns A curve with a shaded region from -infinity to x=a and its area.
#' @export
#'
#' @examples
#' mycurve(mu=10, sigma=2, a=12)
myncurve = function(mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
  # Create x and y for shaded region
  xcurve = seq(mu - 3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  # Polygon for shaded area
  polygon(c(mu - 3*sigma, xcurve, a, a, mu - 3*sigma), c(0, ycurve, 0, 0, 0), col="purple")
  # Computes the probability P(X ≤ a)
  prob = pnorm(a, mean=mu, sd=sigma)
  prob = round(prob, 4)
  # Displays probability on plot
  text(x=mu, y=max(ycurve)/2, paste("P(X ≤", a, ") =", prob))
  # Return values as a list
  list(mu = mu, sigma = sigma, a = a, probability = prob)
}
