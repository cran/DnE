\name{Cubic}
\alias{Cubic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cubic}
\description{solve cubic equation.}
\usage{
Cubic(b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{a four-dimensional vetor, its components are descending dimension coefficients of the equation.}
}
\details{
the function is used to solve an cubic equation. the parameter of this function is the coefficients of your function. if you want to solve the equation x^3+5x^2-4x+9, then the parameter should be a<-c(1,5,-4,9)
}
\value{all the solution to the equation}
\references{
the function includes some other functions which have been exsisted in R
}
\author{JunYao Chen, CuiYi He, YuanRui Wu, MengQing Sun}
\note{
the first num of the parameter you put in can't be zero. for example, you can't define a<-(0,4,3,5) and compute Cubic(a)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DnE-package}}
}
\examples{
require(stats)
a <- c(1,5,-4,9)
Cubic(a)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
