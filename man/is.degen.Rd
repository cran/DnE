\name{is.degen}
\alias{is.degen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.degen}
\description{judge if the data obeys degenerate distribution.}
\usage{
is.degen(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Degenerate distribution or not. Usually, to ensure the function works well, the sample size needs to be large enough, i.e. the result will be stable if the sample size is larger than 100.  
}
\value{if the data possibly obeys degenerate distribution, return a value named qchisq which represents the possibility. the larger qchisq is, the larger the possibility will be; else return -1.}
\references{
 ROBERT V. HOGG/ALLEN T. CRAIG (Fifth Edition) Introduction Mathematical Statistics.}
\author{JunYao Chen, CuiYi He, BoXian Wei}
\note{
please pay attention to the definition of parameters in our functions.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.dt}} ,  \code{\link{DnE-package}}
}
\examples{
require(stats)
examplecheck<-c(1,1,1,1,1)
is.degen(examplecheck)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
