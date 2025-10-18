#' testsample
#'
#' Creates iter barplots at a rate of 1 per time with a sample size n
#'
#' @param n Numeric. Sample size for each iteration.
#' @param iter Integer. Number of iterations (barplots) to generate.
#' @param time Numeric. Pause time (in seconds) between iterations.
#'
#' @returns barplot
#' @export
#'
#' @examples
#' testsample(iter=1, n=1000, time=1)
testsample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    # Creates a sample
    s=sample(1:10,n,replace=TRUE)
    # Turns the sample into a factor
    sf=factor(s,levels=1:10)
    # Creates a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Iteration ", i, ", n= ", n,sep=""), ylim=c(0,0.2))
    # Releases the table
    Sys.sleep(time)
  }
}
