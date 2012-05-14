is.degen <-
function(x)
{
	if(var(x)==0)
	{
		return(Inf);
	}
	else
	{
		return(-1);
	}
}
