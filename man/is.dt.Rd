\name{is.dt}
\alias{is.dt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.dt}
\description{judge the distribution of the data.}
\usage{
is.dt(x, m, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the number of intervals you want to divide the data in, default value is 10}
  \item{a}{significance level}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of normal distribution or not. Usually, to ensure the function works well, the sample size needs to be large enough, i.e. the result will be stable if the sample size is larger than 100. The function will work better if the number of intervals you choose to divide the data in is between 10 and 20. This number cannot excess the number of given oberservation. 
}
\value{the distribution of the data}
\references{
 ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\author{JunYao Chen, CuiYi He, BoXian Wei}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DnE-package}} , \code{\link{is.bern}} ,  \code{\link{is.beta}} , \code{\link{is.binom}} , \code{\link{is.chisq}} , \code{\link{is.degen}} , \code{\link{is.exp}} , \code{\link{is.f}} , \code{\link{is.gamma}} , \code{\link{is.geom}} , \code{\link{is.nbinom}} , \code{\link{is.norm}} , \code{\link{is.pois}} , \code{\link{is.t}} , \code{\link{is.unif}}
}
\examples{
require(stats)
x=rnorm(100)
is.dt(x,10,0.05)
#@@@@   dis       pr
#@@@@4 norm 0.007222132
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
