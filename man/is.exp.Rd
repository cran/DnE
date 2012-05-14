\name{is.exp}
\alias{is.exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.exp}
\description{judge if the data obey exp exponential distribution.}
\usage{
is.exp(x, m, a, lambda = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
  \item{lambda}{the parameter lambda}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Exponential distribution or not.The Exponential distribution mentioned in this function is the one with a mean of 1/lambda and a variance of 1/lambda^2. The function will work better if the number you choose to devide the interval is between 5 and 20. 
}
\value{if data obey the distribution, return a value represent the likelihood, the larger the better; else return -1.}
\references{
 ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\author{JunYao Chen, CuiYi He, YuanRui Wu, MengQing Sun}
\note{
please play attention to the definition of the parameter in our function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.dt}} ,  \code{\link{DnE-package}}
}
\examples{
require(stats)
examplecheck<-rexp(100,10)
is.exp(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, m, a, lambda = NULL) 
{
    re = 1
    for (i in 1:length(x)) if (x[i] < 0) 
        re = -1
    p = rep(0, m + 1)
    y = rep(0, m + 1)
    q = 0
    if (re == -1) {
        return(-1)
    }
    else {
        if (is.null(lambda)) {
            p0 = 1/mean(x)
            df = m - 1
        }
        else {
            df = m
        }
        if (p0 > 0) {
            di = max(x)
            for (i in 1:m) {
                p[i] = pexp(di * i/m, p0) - pexp(di * (i - 1)/m, 
                  p0)
                if (p[i] == 0) {
                  break
                }
                for (j in 1:length(x)) if (x[j] > di * (i - 1)/m && 
                  x[j] <= di * i/m) 
                  y[i] = y[i] + 1
                q = q + (y[i] - (length(x) * p[i]))^2/(length(x) * 
                  p[i])
            }
            p[m + 1] = pnorm(Inf, p0) - pnorm(max(x), p0)
            q = q + length(x) * p[m + 1]
            q0 = qchisq(1 - a, df)
            if (q <= q0) {
                return(q0 - q)
            }
            else {
                return(-1)
            }
        }
        else {
            return(-1)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
