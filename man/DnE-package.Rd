\name{DnE-package}
\alias{DnE-package}
\alias{DnE}
\docType{package}
\title{DnE-package}
\description{The DnE package involves functions to analyse the distribution of a set of given data. The basic idea of the analysis is chi-squared test. Functions which have the form as "is.xxdistribution" are used to analyse whether the data obeys the xxdistrbution. If you do not know which distribution to judge, use function is.dt().}
\details{
\tabular{ll}{
Package: \tab DnE\cr
Type: \tab Package\cr
Version: \tab 2.1.0\cr
Date: \tab 2014-12-01\cr
License: \tab GPL (>= 2)\cr
}
}
\author{JunYao Chen, CuiYi He, Boxian Wei}
\references{ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\keyword{ package }
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
