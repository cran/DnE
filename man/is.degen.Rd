\name{is.degen}
\alias{is.degen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.degen}
\description{judge if the data obey degenerate distribution.}
\usage{
is.degen(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Degenerate distribution or not.The function will work better if the number you choose to devide the interval is between 5 and 20. 
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
examplecheck<-c(1,1,1,1,1)
is.degen(examplecheck)
#examplecheck is a dataset with a defined distribution you want to check. suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    if (var(x) == 0) {
        return(Inf)
    }
    else {
        return(-1)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
