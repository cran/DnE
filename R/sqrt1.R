sqrt1 <-
function(x)
{ 
    if (x>=0){x=sqrt(x)}
    else{x=complex(real=0,imaginary=sqrt(-x))}
}
