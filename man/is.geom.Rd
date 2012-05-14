\name{is.geom}
\alias{is.geom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.geom}
\description{judge if the data obey geometric distribution.}
\usage{
is.geom(x, a, p0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{a}{confidence level}
  \item{p0}{the pobability of successful events}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Geometric distribution or not. The Geometric distribution mentioned in this function is defined as the repetition of Bernoulli when the first success occur. The observations must be Positive integer.
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
examplecheck<-rgeom(100,0.4)+1
is.geom(examplecheck,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,a,p0=NULL)
{
	re=1;
	for(i in 1:length(x))
		if(x[i]<0&&round(x)!=x)
			re=-1;
	if(re==-1)
	{
		return(-1);
	}
	else
	{
		di=max(x)-min(x)+1;
		if(is.null(p0))
		{
			p0=1/mean(x);
			df=di;
		}
		else
		{
			df=di+1;
		}
		if(p0>=0&&p0<=1)
		{
		p=rep(0,max(x)+1);
		y=rep(0,max(x)+1);
		q=0;
		for(i in 1:max(x))
		{
			p[i]=dgeom(i-1,p0);
			if(p[i]==0)
			{
				break;
			}
			for(j in 1:length(x))
				if(x[j]==i)
					y[i]=y[i]+1;
			q=q+(y[i]-length(x)*p[i])^2/(length(x)*p[i]);
		}
		p[max(x)+1]=1-pgeom(max(x)-1,p0);
		q=q+length(x)*p[max(x)+1];
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
