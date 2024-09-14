#task 1
# estimate integration of sin(x) from 0 to 1

runs=10000
xs = runif(runs,min=0.0,max=1.0)
#example
print(runif(10))
print(rnorm(10))
#end
ys = runif(runs,min=0.0,max=1.0)
blow.curve = ys<=sin(xs)
#example
x=1
x==2
x!=2
x<3

print(below.curve)
mc.integration = sum(below.curve)/runs
plot(xs,ys,pch='.',col=ifelse(below.curve,"blue","grey"),xlab='',ylab='',asp=1,main=paste("MC Aprroximation =",mc.integration))
help("plot")
#task 2
# initial points are 1000
#if head ,+1,if tail,-1
# simple use of function and while loop
f = function(x,y){
  result = (x+y)/2
}
print(f(1,2))
x=0
#while loop
while(x<10){
  print(x)
  x=x+1
}
# for loop
for(x in 1:10){
  print(x)
}
toss_t_times = function(t){
  path = c(1000)
  current = 1000
  while(length(path)<=t){
    current=current+ifelse(runif(1)<0.5,1,-1)
    path=c(path,current)
  }
  path
}
#example
toss_t_times(10)
#end

N=100
T=1000
vlist = replicate(N,toss_t_times(T))
plot(c(1,T),c(min(vlist),max(vlist)),xlab="number of tosses",ylab="points",type="n")


for(i in 1:N){
  lines(1:(T+1),vlist[,i])
}
hist(vlist[T+1,],xlab="final score",col="green",border="black")