Cubic <-
function(b)
{   a=c(0,0,0)
    for(i in 2:4) {a[i-1]=b[i]/b[1]}
    p=a[2]-a[1]^2/3
    q=a[3]-a[1]*a[2]/3+2*a[1]^3/27
    if(is.complex(sqrt1(q^2/4+p^3/27)))
       {  A=(-q/2+sqrt1(q^2/4+p^3/27))^(1/3)
          B=(-q/2-sqrt1(q^2/4+p^3/27))^(1/3)
        }
    else
       {  if((-q/2+sqrt(q^2/4+p^3/27))<0) {A=-(-(-q/2+sqrt(q^2/4+p^3/27)))^(1/3)}
          else{A=(-q/2+sqrt(q^2/4+p^3/27))^(1/3)}
          if((-q/2-sqrt(q^2/4+p^3/27))<0) {B=-(-(-q/2-sqrt(q^2/4+p^3/27)))^(1/3)}
          else{B=(-q/2-sqrt(q^2/4+p^3/27))^(1/3)}
        }
    y=A+B
    y[2]=-(A+B)/2+sqrt(3)*(A-B)*complex(real=0,imaginary=1)/2
    y[3]=-(A+B)/2-sqrt(3)*(A-B)*complex(real=0,imaginary=1)/2
    x=c(0,0,0)
    for(i in 1:3) {x[i]=y[i]-a[1]/3}
    print(x)
}
