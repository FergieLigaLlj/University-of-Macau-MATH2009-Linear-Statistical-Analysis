
# Download R from https://cran.r-project.org/mirrors.html 
# and the Rstudio IDE from www.rstudio.com
# There are quite a lot of good materials online for learning some basics of R. For instance, you can visit  https://rstudio-education.github.io/hopr/ for a reference.
# The best way is to write your own code and learn from errors!
# The code in this file includes some examples in the slide named "Introduction"
# Press ctrl+Enter  (Windows) or Command+Enter(Mac) to run the code

#########################################
# dumbbell calibration data
x=c(25,50,75,100,125,150,175,200,225,250)
y=c(26,50,74,99,123,152,175,197,221,247)


#dumbbell_df<-data.frame(Dumbbel_Weights=x, Scale_Weights = y)
#dumbbell_df$Dumbbel_Weights

plot(x,y,xlab="Dumbbell Weights",ylab="Scale Weights")
lines(x,x,col=2)

##################################################
# earthquakes data
EARTHQUAKES<-read.table('C:/Users/zhixzhang/Dropbox/Teaching/2023-2/math2009-Zhixiang Zhang/Introduction/Datasets/EARTHQUAKES.txt', sep = "\t",header =TRUE)
# Try to run the following code, what is the difference?
# EARTHQUAKES<-read.table('C:/Users/zhixzhang/Dropbox/Teaching/2023-2/math2009-Zhixiang Zhang/Introduction/Datasets/EARTHQUAKES.txt',sep='\t')

head(EARTHQUAKES)
names(EARTHQUAKES)

L=EARTHQUAKES$Length
W=EARTHQUAKES$Width
A=L*W
M_S=EARTHQUAKES$Magnitude
plot(A,M_S,xlab="Area",ylab="Magnitude")

logA=log(A)
fit=lm(M_S~logA)
co=fit$coefficients
plot(logA,M_S,xlab="log(A)",ylab="Magnitude")
lines(logA,co[1]+co[2]*logA,col=2,lty=3)

D=EARTHQUAKES$Displacement
logD=log(D)
fit=lm(M_S~logD)
co=fit$coefficients
plot(logD,M_S,xlab="log(D)",ylab="Magnitude",ylim=c(5,9))
lines(logD,co[1]+co[2]*logD,col=2,lty=3)

#install.packages('scatterplot3d')
library(scatterplot3d)
x1=logD
x2=logA
plot3d=scatterplot3d(x1,x2,M_S,color="red",xlab="log(D)",ylab="log(A)",zlab="Magnitude",xlim=c(-5,5),ylim=c(4,12),zlim=c(4,11))
my.lm=lm(M_S~x1+x2)
plot3d$plane3d(my.lm,lty.box="solid")
dev.off()

E=EARTHQUAKES$log.of.energy
fit=lm(M_S~E)
co=fit$coefficients
plot(E,M_S,xlab="log(Energy)",ylab="Magnitude",ylim=c(5,9))
lines(E,co[1]+co[2]*E,col=2,lty=3)