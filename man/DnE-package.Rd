\name{DnE-package}
\alias{DnE-package}
\alias{DnE}
\docType{package}
\title{DnE-package}
\description{This package is made for solving cubic and quartic equations and analyze the distribution of the data that R user provide. The package contains function Cubic() and Quart() for equations and all kinds of functions for distributions analysis which have format as is.xxdistrbution() and if you don't know which distribution to judge, use function dt().}
\details{
\tabular{ll}{
Package: \tab DnE\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-05-14\cr
License: \tab GPL (>= 2)\cr
}
}
\author{JunYao Chen, CuiYi He, YuanRui Wu, MengQing Sun}
\references{ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\keyword{ package }
\seealso{
\code{\link{DnE-package}} , \code{\link{is.bern}} ,  \code{\link{is.beta}} , \code{\link{is.binom}} , \code{\link{is.chisq}} , \code{\link{is.degen}} , \code{\link{is.exp}} , \code{\link{is.f}} , \code{\link{is.gamma}} , \code{\link{is.geom}} , \code{\link{is.nbinom}} , \code{\link{is.norm}} , \code{\link{is.pois}} , \code{\link{is.t}} , \code{\link{is.unif}} , \code{\link{Cubic}} , \code{\link{Quart}} 
}
\examples{
require(stats)
x=rpois(100,0.5)
is.dt(x,10,0.05)
#@@@@   dis       pr
#@@@@4 pois 5.656147
#we will check out the distribution of dataset x with the function is.dt and with the confidence level of 0.05#
}
