\name{is.geom}
\alias{is.geom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.geom}
\description{judge if the data obey geometric distribution.}
\usage{
is.geom(x, a, p0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{a}{confidence level}
  \item{p0}{the pobability of successful events}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Geometric distribution or not. The Geometric distribution mentioned in this function is defined as the repetition of Bernoulli when the first success occur. The observations must be Positive integer.
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
examplecheck<-rgeom(100,0.4)+1
is.geom(examplecheck,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
