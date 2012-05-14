\name{is.t}
\alias{is.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.t}
\description{judge if the data obey t-distribution.}
\usage{
is.t(x, m, a, n0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
  \item{n0}{the degree of freedom}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of t distribution or not. The function will work better if the number you choose to devide the interval is between 5 and 20. 
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
examplecheck<-rt(100,10)
is.t(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, m, a, n0 = NULL) 
{
    p = rep(0, m + 2)
    y = rep(0, m + 2)
    q = 0
    x2 = var(x)
    if (is.null(n0)) {
        n0 = 2 * x2/(x2 + 1)
        df = m
    }
    else {
        df = m + 1
    }
    if (n0 > 0) {
        di = max(x) - min(x)
        for (i in 1:m) {
            p[i] = pt(min(x) + di * i/m, n0) - pt(min(x) + di * 
                (i - 1)/m, n0)
            if (p[i] == 0) {
                break
            }
            for (j in 1:length(x)) if (x[j] > (min(x) + di * 
                (i - 1)/m) && x[j] <= (min(x) + di * i/m)) 
                y[i] = y[i] + 1
            q = q + (y[i] - (length(x) * p[i]))^2/(length(x) * 
                p[i])
        }
        p[m + 1] = pt(Inf, n0) - pt(max(x), n0)
        p[m + 2] = pt(min(x), n0)
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
