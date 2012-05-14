Quart <-
function(p)
{
    a=p[1];b=p[2];c=p[3];d=p[4];e=p[5]
    D=-(3*b^2-8*a*c);F=-(b^3-4*a*b*c+8*a^2*d)
    E=(3*b^4+16*a^2*c^2-16*a*b^2*c+16*a^2*b*d-64*a^3*e)
    A=D^2-3*E;B=D*E-9*F;C=E^2-3*D*F
    if((B^2-4*A*C)>=0)
        { Y1=A*D+3*(-B+sqrt(B^2-4*A*C))/2;Y2=A*D-3*(-B-sqrt(B^2-4*A*C))/2
          if (Y1>=0){Y10=Y1^(1/3)}else{Y10=-((-Y1)^(1/3))}
          if (Y2>=0){Y20=Y2^(1/3)}else{Y20=-((-Y2)^(1/3))}
          Z1=(-2*D-Y10-Y20)/6;Z2=sqrt(3)*(Y10-Y20)/6
          Z=-(-D+Y10+Y20)/3
          W1=sqrt1(2*(Z1+sqrt(Z1^2+Z2^2)));W2=sqrt1(2*(-Z1+sqrt(Z1^2+Z2^2)))
          x=(-b+sqrt1(Z)+2*W1)/(4*a)
          x[2]=(-b+sqrt1(Z)-2*W1)/(4*a)
          x[3]=complex(real=(-b-sqrt1(Z))/(4*a),imaginary=-2*W2/(4*a))
          x[4]=complex(real=(-b-sqrt1(Z))/(4*a),imaginary=2*W2/(4*a))
         }
    else
        { T=acos((2*A*D-3*B)/(2*sqrt1(A^3)))
          y1=-(D+2*sqrt1(A)*cos(T/3))/3
          y2=-(D+2*sqrt1(A)*cos((T+2*pi)/3))/3
          y3=-(D+2*sqrt1(A)*cos((T-2*pi)/3))/3
          x=(-b+sqrt1(y1)+sqrt1(y2)+sqrt1(y3))/(4*a)
          x[2]=(-b+sqrt1(y1)-sqrt1(y2)-sqrt1(y3))/(4*a)
          x[3]=(-b-sqrt1(y1)-sqrt1(y2)+sqrt1(y3))/(4*a)
          x[4]=(-b-sqrt1(y1)+sqrt1(y2)-sqrt1(y3))/(4*a)
        }
    print(x)
}
