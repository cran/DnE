\name{is.exp}
\alias{is.exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.exp}
\description{judge if the data obeys exp exponential distribution.}
\usage{
is.exp(x, m, a, lambda = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the number of intervals you want to divide the data in, default value is 10}
  \item{a}{significance level}
  \item{lambda}{the parameter lambda}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Exponential distribution or not. Usually, to ensure the function works well, the sample size needs to be large enough, i.e. the result will be stable if the sample size is larger than 100. The Exponential distribution mentioned in this function is the one with mean 1/lambda and variance 1/lambda^2. The function will work better if the number of intervals you choose to divide the data in is between 10 and 20. This number cannot excess the number of given oberservations.  
}
\value{if data possibly obeys exponential distribution, return a value named qchisq which represents the possibility. The larger qchisq is, the larger the possibility will be; else return -1.}
\references{
 ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\author{JunYao Chen, CuiYi He, BoXian Wei}
\note{
please pay attention to the definition of parameters in our functions.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.dt}} ,  \code{\link{DnE-package}}
}
\examples{
require(stats)
examplecheck<-rexp(100,10)
is.exp(examplecheck,10,0.05)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
