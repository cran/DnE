is.bern <-
function(x)
{
	re=Inf;
	for(i in 1:length(x))
		if(x[i]!=1&&x[i]!=0)
			re=-1;
	return(re);
}
