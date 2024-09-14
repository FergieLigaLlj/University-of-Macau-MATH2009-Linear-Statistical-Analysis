#####################################################################
## the plot on page 54 of the slide "Chapter 1:Simple Data Visulization and Location"
install.packages('L1pack')
library(L1pack)
EARTHQUAKES<-read.table('C:/Users/zhixzhang/Dropbox/Teaching/2023-2/math2009/Introduction/Datasets/EARTHQUAKES.txt', sep = "\t",header =TRUE)

E=EARTHQUAKES$`log.of.energy`
M_S=EARTHQUAKES$`Magnitude`
fit=lm(E~M_S)
co=fit$coefficients
fitlad=lad(E~M_S)
colad=fitlad$coefficients
plot(M_S,E,ylab="log(Energy)",xlab="Magnitude",ylim=c(21,29),bty="n")
lines(M_S,co[1]+co[2]*M_S,col=2,lty=3,cex=2)
lines(M_S,colad[1]+colad[2]*M_S,col=4,lty=3,cex=2)
legend(5.5,29,legend=c("Least Squares Line","Least Absolute Distance Line"),col=c("red", "blue"),lty=1:2,cex=0.8)

## Solving the linear programming on page 53 gives the same solution
#install.packages('lpSolve')
library(lpSolve)
#install.packages('matlab')
library(matlab)
E=EARTHQUAKES$`log.of.energy`
M_S=EARTHQUAKES$`Magnitude`
n=length(E)
f.obj=c(ones(1,n),0,0)
A=cbind(diag(n),ones(n,1),ones(n,1)*M_S)
B=cbind(diag(n),-ones(n,1),-ones(n,1)*M_S)
f.con=rbind(A,B)
f.dir=rep(">=",2*n)
f.rhs=c(E,-E)
sol=lp("min", f.obj, f.con, f.dir, f.rhs)

## agree with the solution by using the function "lad" 
sol$solution[n+1] 
sol$solution[n+2]


#########################################################################
## the plot on page 18 of the slide "Chapter 2:Data Dispersion and Correlation"
sample=c(7,6.8,6.7,5.9,7.3,7.6,6.2)
x=sample
y=c(24.53,24.57,24.6,23.4,25.42,26.09,24.01)
fit=lm(y~x)
co=fit$coefficients
par(mfrow = c(1, 2))
plot(x,y,ylab="log(Energy)",xlab="Magnitude",bty="n",ylim=c(23,26.1),pch=16)
lines(x,co[1]+co[2]*x,col=2,lty=1)
lines(x,co[1]+co[2]*x+0.25,col=4,lty=3)
lines(x,co[1]+co[2]*x-0.25,col=4,lty=3)
plot(x,y, ylab="log(Energy)",xlab="Magnitude",pch=16)
lines(x,24.66+c(0,0,0,0,0,0,0),col=2,lty=1)
lines(x,24.66+c(0,0,0,0,0,0,0)+0.88,col=4,lty=3)
lines(x,24.66+c(0,0,0,0,0,0,0)-0.88,col=4,lty=3)
dev.off()




##the plot on page 36 of the slide "Chapter 2:Data Dispersion and Correlation"
E=EARTHQUAKES$'log.of.energy'
M_S=EARTHQUAKES$'Magnitude'
fit=lm(E~M_S)
co=fit$coefficients
plot(M_S,E,ylab="log(Energy)",xlab="Magnitude",ylim=c(21,29))
lines(M_S,co[1]+co[2]*M_S,col=2,lty=3)
fit1=lm(M_S~E)
co1=fit1$coefficients
lines(M_S,(M_S-co1[1])/co1[2],col=3,lty=1)
legend(5.5,28.5,legend=c("Magnitude=0.51*log(Energy)-5.71","log(Energy)=1.71*Magnitude+13.1"),col=c("green","red"),lty=1:2,cex=0.8)