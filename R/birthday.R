#' Birthday
#'
#' @param x A numeric vector
#'
#' @returns Probability that 2 or more people in a random sample k have the same birthday.
#' @export
#'
#' @examples
#' birthday(10)
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
birthday(20:24)
