#task 1
## Estimate the integration of sin(x) from 0 to 1

runs = 10000
xs = runif(runs,min=0.0,max=1.0)
##example
print(runif(10))
print(rnorm(10))
##end
ys = runif(runs,min=0.0,max=1.0)

below.curve = ys <= sin(xs)
##example
x=1
x==2
x!=2
x<3
print(below.curve)
##end

mc.integration = sum(below.curve)/runs

plot(xs,ys,pch='.',col=ifelse(below.curve,"blue","grey")
     ,xlab='',ylab='',asp=1,
     main=paste("MC Approximation =",mc.integration))















#task 2
#Let's play a game. Your initial points are 1000. 
#In each round, tossing a coin, and if it is head, you gain one point. 
#If it is tail, you lose one point.
#After 1000 rounds, what is your total score 




toss_t_times=function(t){
  path=c(1000)
  current=1000
  while (length(path)<t){
    current=current+ifelse(runif(1)<0.6,1,-1)
    path=c(path,current)
  }
  path
}
##example
toss_t_times(10)
##end
N=1000
T=1000

vlist = replicate(N, toss_t_times(T))

plot(c(1, T), c(min(vlist), max(vlist)), xlab="number of tosses",ylab="points",type = "n")

for (i in 1:N) {
  lines(1:T, vlist[,i])
}


hist(vlist[T,], xlab = " final score",
     col = "green", border = "black")