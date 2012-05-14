\name{is.binom}
\alias{is.binom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.binom}
\description{judge if the data obey binomial distribution.}
\usage{
is.binom(x, m, a, n = NULL, p0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
  \item{n}{the number of experiments}
  \item{p0}{the probability of successful event}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of binomial or not.The function will work better if the number you choose to devide the interval is between 5 and 20. 
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
examplecheck<-rbinom(100,50,0.3)
is.binom(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, m, a, n = NULL, p0 = NULL) 
{
    re = 1
    for (i in 1:length(x)) if (x[i] < 0) 
        re = -1
    p = rep(0, m)
    y = rep(0, m)
    q = 0
    if (re == -1) {
        return(-1)
    }
    else {
        x1 = mean(x)
        sum = 0
        for (i in 1:length(x)) {
            sum = sum + x[i]^2
        }
        x2 = sum/length(x)
        if (is.null(n) && is.null(p0)) {
            p0 = 1 + (x1^2 - x2)/x1
            n = round(x1/p0)
            df = m - 3
        }
        else if (is.null(n)) {
            n = round(x1/p0)
            df = m - 2
        }
        else if (is.null(p0)) {
            p0 = 1 + (x1^2 - x2)/x1
            df = m - 2
        }
        else {
            df = m - 1
        }
        if (p0 > 0 && p0 < 1) {
            for (i in 1:m) {
                p[i] = pbinom(round(n * i/m), n, p0) - pbinom(round(n * 
                  (i - 1)/m), n, p0)
                if (p[i] == 0) {
                  break
                }
                for (j in 1:length(x)) if (x[j] > n * (i - 1)/m && 
                  x[j] <= n * i/m) 
                  y[i] = y[i] + 1
                q = q + (y[i] - (length(x) * p[i]))^2/(length(x) * 
                  p[i])
            }
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
