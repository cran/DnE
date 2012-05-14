\name{is.pois}
\alias{is.pois}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.pois}
\description{judge if the data obey poisson distribution}
\usage{
is.pois(x, a, lambda = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x}
  \item{a}{confidence level}
  \item{lambda}{parameter, the number of successful events in unit time.}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of possion distribution or not. 
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
examplecheck<-rpois(100,10)
is.pois(examplecheck,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,a,lambda=NULL)
{
	re=1;
	for(i in 1:length(x))
		if(x[i]<0&&round(x)!=x)
			re=-1;
	di=max(x)-min(x)+1;
	p=rep(0,di+1);
	y=rep(0,di+1);
	q=0;
	if(re==-1)
	{
		return(-1);
	}
	else
	{
		if(is.null(lambda))
		{
			lambda=mean(x);
			df=di-1;
		}
		else
		{
			df=di;
		}
		if(lambda>=0)
		{
			for(i in 1:di)
			{
				p[i]=dpois(min(x)+i-1,lambda);
			if(p[i]==0)
			{
				break;
			}
				for(j in 1:length(x))
				if(x[j]==min(x)+i-1)
					y[i]=y[i]+1;
			q=q+(y[i]-(length(x)*p[i]))^2/(length(x)*p[i]);
			}
		p[di+1]=1-ppois(max(x),lambda);
		q=q+length(x)*p[di+1];
		q0=qchisq(1-a,df);
		if(q<=q0)
		{
			return(q0-q);
		}
		else
		{
			return(-1);
		}
		}
		else 
		{
			return(-1);
		}
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
