\name{Quart}
\alias{Quart}
\title{Quart}
\description{solve quartic equation.}
\usage{
Quart(p)
}
\arguments{
  \item{p}{a five-dimensional vetor, its components are descending dimension coefficients of the equation.}
}
\details{
the function is used to solve an quart equation. the parameter of this function is the coefficients of your function. if you want to solve the equation x^4+5x^3-4x^2+9x-2, then the parameter should be a<-c(1,5,-4,9,-2)
}
\value{all the solution to the equation}
\references{
the function includes some other functions which have been exsisted in R
}

\author{JunYao Chen, CuiYi He, YuanRui Wu, MengQing Sun}
\note{
the first num of the parameter you put in can't be zero. for example, you can't define a<-(0,4,3,5,7) and compute Quart(a)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DnE-package}}
}
\examples{
require(stats)
a <- c(1,5,-4,9,-2)
Quart(a)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
