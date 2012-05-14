\name{is.norm}
\alias{is.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.norm}
\description{judge if the data obey normal distribution.}
\usage{
is.norm(x, m, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of normal distribution or not. The function will work better if the number you choose to devide the interval is between 5 and 20. 
}
\value{
if data obey the distribution, return a value represent the likelihood, the larger the better; else return -1.
}
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
examplecheck<-rnorm(100,0,1)
is.norm(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, m, a) 
{
    x = scale(x)
    p = rep(0, m + 2)
    y = rep(0, m + 2)
    q = 0
    df = m + 1
    di = max(x) - min(x)
    for (i in 1:m) {
        p[i] = pnorm(di * i/m) - pnorm(di * (i - 1)/m)
        if (p[i] == 0) {
            break
        }
        for (j in 1:length(x)) if (x[j] > di * (i - 1)/m && x[j] <= 
            di * i/m) 
            y[i] = y[i] + 1
        q = q + (y[i] - (length(x) * p[i]))^2/(length(x) * p[i])
    }
    p[m + 1] = pnorm(Inf) - pnorm(max(x))
    p[m + 2] = pnorm(min(x))
    y[m + 2] = length(which(x == min(x)))
    q = q + length(x) * p[m + 1]
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
