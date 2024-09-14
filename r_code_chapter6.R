#######################################################
### scale calibration example
x=c(25,50,75,100,125,150,175,200,225,250)
y=c(26,50,74,99,123,152,175,197,221,247)
plot(x,y)
fit<-lm(y~x)
summary(fit)
fit_aov<-aov(y~x)
summary(fit_aov)
predict(fit, newdata =data.frame(x=c(53)), se.fit = TRUE, interval='prediction',level=0.95)

#######################################################
### regression through the origin
fit1<-lm(y~0+x)
summary(fit1)
fit1_aov<-aov(y~0+x)
summary(fit1_aov)



###########################################
### Cases that R squared can be misleading
##########################################
## case 1
x<-runif(10000,0,1)
y<-x^2
summary(lm(y~x))
plot(x,y)
## In this example, the R squared is large, but using the simple linear model y=a+bx is not proper

#########################################
## case 2
n <- 10000
x <- 100*runif(n)
y <- x+rnorm(n)
plot(x,y)
summary(lm(y~x))
summary(aov(y~x))

x1<-x[10<x&x<12]
y1<-y[10<x&x<12]
plot(x1,y1)
summary(lm(y1~x1))
summary(aov(y1~x1))
#In this example, both SS_{reg} and SS_{res} decrease, 
#but the decrease of SS_{res} is much larger than the decrease of SS_{reg}