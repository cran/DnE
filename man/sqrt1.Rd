\name{sqrt1}
\alias{sqrt1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sqrt1}
\description{calculate the evolution of real number}
\usage{
sqrt1(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{real number}
}
\details{
this function is used to solve the square root of a complex number or a real number
}
\value{the evolution of the number}
\references{
the function includes some other functions which have been exsisted in R
}
\author{JunYao Chen, CuiYi He, YuanRui Wu, MengQing Sun}
\note{
any real number and complex number can be used as a parameter in this function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DnE-package}}
}
\examples{
require(stats)
a <- -2
sqrt1(a)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    if (x >= 0) {
        x = sqrt(x)
    }
    else {
        x = complex(real = 0, imaginary = sqrt(-x))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
