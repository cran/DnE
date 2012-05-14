\name{is.f}
\alias{is.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.f}
\description{judge if the data obey f-distribution.}
\usage{
is.f(x, m, a, k1 = NULL, k2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
  \item{k1}{the first degree of feedom}
  \item{k2}{the second degree of freedom}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of f distribution or not. The function will work better if the number you choose to devide the interval is between 5 and 20. 
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
examplecheck<-rf(100,10,20)
is.f(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, m, a, k1 = NULL, k2 = NULL) 
{
    re = 1
    for (i in 1:length(x)) if (x[i] < 0) 
        re = -1
    p = rep(0, m + 2)
    y = rep(0, m + 2)
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
        if (is.null(k1) && is.null(k2)) {
            k2 = 2 * x1/(x1 - 1)
            k1 = 2 * x1^2 * (k2 - 2)/((k2 - 4) * (x2 - x1^2) - 
                2 * x1^2)
            df = m - 1
        }
        else if (is.null(k1)) {
            k1 = 2 * x1^2 * (k2 - 2)/((k2 - 4) * (x2 - x1^2) - 
                2 * x1^2)
            df = m
        }
        else if (is.null(k2)) {
            k2 = 2 * x1/(x1 - 1)
            df = m
        }
        else {
            df = m + 1
        }
        if (k1 > 0 && k2 > 4) {
            di = max(x) - min(x)
            for (i in 1:m) {
                p[i] = pf(min(x) + di * i/m, k1, k2) - pf(min(x) + 
                  di * (i - 1)/m, k1, k2)
                if (p[i] == 0) {
                  break
                }
                for (j in 1:length(x)) if (x[j] > (min(x) + di * 
                  (i - 1)/m) && x[j] <= (min(x) + di * i/m)) 
                  y[i] = y[i] + 1
                q = q + (y[i] - (length(x) * p[i]))^2/(length(x) * 
                  p[i])
            }
            p[m + 1] = pf(Inf, k1, k2) - pf(max(x), k1, k2)
            p[m + 2] = pf(min(x), k1, k2)
            y[m + 1] = length(which(x == min(x)))
            q = q + (y[m + 1] - (length(x) * p[m + 1]))^2/(length(x) * 
                p[m + 1])
            q = q + (y[m + 2] - (length(x) * p[m + 2]))^2/(length(x) * 
                p[m + 2])
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
