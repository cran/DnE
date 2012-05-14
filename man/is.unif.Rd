\name{is.unif}
\alias{is.unif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{is.unif}
\description{judge if the data obey Uniform Distribution}
\usage{
is.unif(x, m, a, sita1 = NULL, sita2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data}
  \item{m}{the numbers of intervals of data you wanna devide}
  \item{a}{confidence level}
  \item{sita1}{distribution parameter, the lower limit of the data}
  \item{sita2}{distribution parameter, the upper limit of the data}
}
\details{
Given a set of observations from a certain distribution, this function is used to test whether the observations are from a distribution of Uniform distribution or not. The function will work better if the number you choose to devide the interval is between 5 and 20. 
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
examplecheck<-runif(100,10,50)
is.unif(examplecheck,10,0.05)
#examplecheck is a dataset with a defined distribution you want to check. Suppose you want to devide the interval into 10 parts and want the confidence level to be 0.05#
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,m,a,sita1=NULL,sita2=NULL)
{
	p=rep(0,m);
	y=rep(0,m);
	q=0;
	x1=mean(x);sum=0;
	for(i in 1:length(x))
	{
		sum=sum+x[i]^2;
	}
	x2=sum/length(x);
	if (3*x2-3*x1^2>=0)
	{
		if(is.null(sita1)&&is.null(sita2))
		{
			sita1=x1-sqrt(3*x2-3*x1^2);
			sita2=x1+sqrt(3*x2-3*x1^2);
			df=m-2;
		}
		else if(is.null(sita1))
		{
			sita1=x1-sqrt(3*x2-3*x1^2);
			df=m-1;
		}
		else if(is.null(sita2))
		{
			sita2=x1+sqrt(3*x2-3*x1^2);
			df=m-1;
		}
		else 
		{
			df=m;
		}
		di=max(x)-min(x);
		for(i in 1:m)
		{
			p[i]=punif(min(x)+di*i/m,min=sita1,max=sita2)-punif(min(x)+di*(i-1)/m,min=sita1,max=sita2);
			if(p[i]==0)
			{
				break;
			}
			for(j in 1:length(x))
				if(x[j]>(min(x)+di*(i-1)/m) && x[j]<=(min(x)+di*i/m))
					y[i]=y[i]+1;
			q=q+(y[i]-(length(x)*p[i]))^2/(length(x)*p[i]);
		}
                             p[m+1]=punif(Inf,sita1,sita2);
                             q=q+length(x)*p[m+1];
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
