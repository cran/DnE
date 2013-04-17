Quart <-
function(p)
{
    
a=p[2]/p[1]
b=p[3]/p[1]
c=p[4]/p[1]
d=p[5]/p[1]

A=2^(1/3)*(b^2-3*a*c+12*d)
B=sqrt(as.complex(-4*(b^2-3*a*c+12*d)^3+(2*b^3-9*a*b*c+27*c^2+27*a^2*d-72*b*d)^2))
C=(2*b^3-9*a*b*c+27*c^2+27*a^2*d-72*b*d+B)^(1/3)
D=0.25*a^2-2*b/3
E=A/(3*C)+C/((54)^(1/3))
F=-a^3+4*a*b-8*c
G=0.25*F/sqrt(as.complex(D+E))

x=-a/4-0.5*sqrt(as.complex(D+E))-0.5*sqrt(as.complex(2*D-E-G))
x[2]=-a/4-0.5*sqrt(as.complex(D+E))+0.5*sqrt(as.complex(2*D-E-G))
x[3]=-a/4+0.5*sqrt(as.complex(D+E))-0.5*sqrt(as.complex(2*D-E+G))
x[4]=-a/4+0.5*sqrt(as.complex(D+E))+0.5*sqrt(as.complex(2*D-E+G))
x=round(x,6)
x[2]=round(x[2],6)
x[3]=round(x[3],6)
x[4]=round(x[4],6)
    print(x)
}
