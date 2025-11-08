#' mycltu
#'
#' @param n sample size
#' @param iter number of iterations
#' @param a lower bound of distribution
#' @param b upper bound of distribution
#'
#' @returns vector of sample means
#' @export
#'
#' @examples
#' mycltu(10,10000,0,5)
mycltu <- function(n,iter,a=0,b=10){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  # The apply function applies the mean function to each column.
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  # Adds density plot
  lines(density(w),col="Blue",lwd=3)
  # This equation is used because it is the standard deviation equation used in dnorm
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
