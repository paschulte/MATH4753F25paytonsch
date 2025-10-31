#' ntickets
#'
#' @param N the number of seats on plane
#' @param gamma the probability of overbooking
#' @param p the probability that a passanger will show up for flight
#'
#' @importFrom stats qbinom pbinom qnorm
#' @importForm graphics abline barplot points
#'
#' @returns a list of nc, nd, N, p, gamma. a plot is alos returned
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=400, gamma=0.02, p=0.95)}
ntickets = function(N, gamma, p){
  # range of n
  range = seq(N, N+50, b=1)

  # discrete objective (1-gamma-P(X<=N))
  obj_discrete = 1 - gamma - pbinom(N, size=n_range, prob=b)
  # index of min objective
  idx_discrete = which.min(abs(obj_discrete))
  # minimum n value
  nd = range[idx_discrete]
  # creates plot
  plot(range, obj_discrete,
       type = "n",
       main = "Objectives vs n For Optimal Tickets Sold (Discrete)",
       xlab = "n",
       ylab = "Objective")
  # plots line of discrete objective
  lines(range, obj_discrete, col = "black")
  # plots points of discrete objective
  points(range, obj_discrete, col = "blue")
  # plots points for optimal number of tickets sold (n)
  points(range[idx_discrete], obj_discrete[idx_discrete], col = 'green')
  # horizontal and vertical lines that intersect n
  abline(h=0, v=range[idx_discrete], col = 'red')

  # normal approximation objective
  # approximates P(X <= N) using Z value
  z_value = (N +(1/2) - range * p) / sqrt(range * p * (1-p))
  p_approx = pnorm(z_value)
  obj_continous = 1 - gamma - p_approx
  # finds the index of min objective
  idx_cont = which.min(abs(obj_continous))
  # fins minimum n-value
  nc = range[inx_cont]
  # creates plot
  plot(range, obj_continous,
       type = "n",
       main = "Objectives vs n For Optimal Tickets Sold (Continuous)",
       xlab = "n",
       ylab = "Objective")
  # plots line of continuous objective
  lines(range, obj_continous, col = "black")
  # plots points for optimal number of tickets sold (n)
  points(range[idx_cont], obc_continuous[idx_cont], col = 'green')
  # horizontal and vertical lines that intersect n
  abline(h=0, v=range[idx_cont], col = 'red')
  # returns variables
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
