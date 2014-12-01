\name{is.geom}
\alias{is.geom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.geom}
\description{judge if the data obeys geometric distribution.}
\usage{
is.geom(x, a, p0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{a}{significance level}
  \item{p0}{the pobability of success}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Geometric distribution or not. Usually, to ensure the function works well, the sample size needs to be large enough, i.e. the result will be stable if the sample size is larger than 100. The Geometric distribution mentioned in this function is defined as the repetition of Bernoulli when the first success occurs. The observations of geometric distribution must be positive integer.
}
\value{if the data possibly obeys geometric distribution, return a value named qchisq which represents the possibility. The larger qchisq is, the larger the possibility will be; else return -1.}
\references{
 ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\author{JunYao Chen, CuiYi He, BoXian Wei}
\note{
please play attention to the definition of parameters in our functions.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.dt}} ,  \code{\link{DnE-package}}
}
\examples{
require(stats)
examplecheck<-rgeom(100,0.4)+1
is.geom(examplecheck,0.05)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
