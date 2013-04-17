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
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of f distribution or not. The function will work better if the number you choose to devide the interval is between 10 and 20. This number cannot excess the number of given oberservation. 
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
