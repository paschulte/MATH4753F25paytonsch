#' Chebyshev's Theorem
#'
#' @param k A numeric value greater than 1, the number of standard deviations
#'
#' @returns A  value between 0 and 1, the proportion given by Chebyshev's theorem.
#' @export
#'
#' @examples
#' chebyshev(2)
chebyshev <- function(k) {
  if (k <= 1) stop("k must be greater than 1")
  1 - (1 / (k^2))
}
