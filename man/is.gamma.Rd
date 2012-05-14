\name{is.gamma}
\alias{is.gamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.gamma}
\description{judge if the data obey gamma distribution.}
\usage{
is.gamma(x, m, a, a0 = NULL, b0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
  \item{a0}{represent the first parmeter in gamma distribution}
  \item{b0}{represent the second parmeter in gamma distribution}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of gamma distribution or not. The gamma distribution mentioned in this function is the one with a mean of a/b and a variance of a/b^2, where a is the first parameter and b is the second parameter of gamma distribution.The function will work better if the number you choose to devide the interval is between 5 and 20. 
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
examplecheck<-rgamma(100,2,4)
is.gamma(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, m, a, a0 = NULL, b0 = NULL) 
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
        x2 = var(x)
        if (is.null(a0) && is.null(b0)) {
            b0 = x1/x2
            a0 = x1 * b0
            df = m - 1
        }
        else if (is.null(a0)) {
            a0 = x1 * b0
            df = m
        }
        else if (is.null(b0)) {
            b0 = x1/x2
            df = m
        }
        else {
            df = m + 1
        }
        if (a0 > 0 && b0 > 0) {
            di = max(x) - min(x)
            for (i in 1:m) {
                p[i] = pgamma(min(x) + di * i/m, a0, rate = b0) - 
                  pgamma(min(x) + di * (i - 1)/m, a0, rate = b0)
                if (p[i] == 0) {
                  break
                }
                for (j in 1:length(x)) if (x[j] > (min(x) + di * 
                  (i - 1)/m) && x[j] <= (min(x) + di * i/m)) 
                  y[i] = y[i] + 1
                q = q + (y[i] - (length(x) * p[i]))^2/(length(x) * 
                  p[i])
            }
            p[m + 1] = pgamma(Inf, a0, rate = b0) - pgamma(max(x), 
                a0, rate = b0)
            y[m + 1] = 0
            p[m + 2] = pgamma(min(x), a0, rate = b0)
            y[m + 2] = length(which(x == min(x)))
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
