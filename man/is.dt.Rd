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
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of normal distribution or not. The function will work better if the number you choose to devide the interval is between 10 and 20. This number cannot excess the number of given oberservation. 
}
\value{the distribution of the data}
\references{
 ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\author{JunYao Chen, CuiYi He, YuanRui Wu, MengQing Sun}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DnE-package}} , \code{\link{is.bern}} ,  \code{\link{is.beta}} , \code{\link{is.binom}} , \code{\link{is.chisq}} , \code{\link{is.degen}} , \code{\link{is.exp}} , \code{\link{is.f}} , \code{\link{is.gamma}} , \code{\link{is.geom}} , \code{\link{is.nbinom}} , \code{\link{is.norm}} , \code{\link{is.pois}} , \code{\link{is.t}} , \code{\link{is.unif}}
}
\examples{require(stats)
x=rpois(100,0.5)
is.dt(x,10,0.05)
#@@@@   dis       pr
#@@@@4 pois 5.656147
#we will check out the distribution of dataset x with the function is.dt and with the confidence level of 0.05#
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
