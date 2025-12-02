#' myboot2
#'
#' @param iter number of bootstrap resamples
#' @param x numeric vector of data
#' @param fun statistic to compute on each sample
#' @param alpha significance level
#' @param cx character expansion size used for CI and point estimate
#' @param ... additional arguments
#'
#' @returns
#'   \item{ci}{Numeric vector of length 2 giving the bootstrap CI}
#'   \item{fun}{The statistic used}
#'   \item{x}{The original data vector}
#' @export
#'
#' @examples
#' \dontrun{myboot2(iter=10000, x=sam, fun="mean", alpha=0.05)}
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  # sample size
  n=length(x)
  # bootstroap resamples
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  # bootstrap stats
  xstat=apply(rs.mat,2,fun)
  # confidence interval
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  # Creates histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),...)
  # point estimate
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  # vertical line
  abline(v=pte,lwd=3,col="Black")
  # makes the segment for the ci
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)
  # plots the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  return(list(ci=ci,fun=fun,x=x))
}
