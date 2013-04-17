\name{is.nbinom}
\alias{is.nbinom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.nbinom}
\description{judge if the data obey negative binomial distribution}
\usage{
is.nbinom(x, m, a, p0 = NULL, r0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
  \item{p0}{the pobability that result is success in each experiment}
  \item{r0}{the number of successful events you want to wait for}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Two negative binomial distribution distribution or not. The function will work better if the number you choose to devide the interval is between 10 and 20. This number cannot excess the number of given oberservation. 
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
examplecheck<-rnbinom(100,10,0.1)
is.nbinom(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
