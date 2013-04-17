\name{is.pois}
\alias{is.pois}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.pois}
\description{judge if the data obey poisson distribution}
\usage{
is.pois(x, a, lambda = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x}
  \item{a}{confidence level}
  \item{lambda}{parameter, the number of successful events in unit time.}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of possion distribution or not. 
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
examplecheck<-rpois(100,10)
is.pois(examplecheck,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
