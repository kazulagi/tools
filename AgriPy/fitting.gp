#f(x)=a*x*x+b*x+c
f(x)=a*exp(x)-b*x+c
a=1	
b=1
c=1
fit f(x) 'xy_sample.txt' using 1:2 via a,b,c
plot f(x), 'xy_sample.txt' using 1:2
