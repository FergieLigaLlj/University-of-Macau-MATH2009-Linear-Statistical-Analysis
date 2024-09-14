##############################################################################
EARTHQUAKES<-read.table('C:/Users/zhixzhang/Dropbox/Teaching/2023-2/math2009/Introduction/Datasets/EARTHQUAKES.txt', sep = "\t",header =TRUE)
E=EARTHQUAKES$`log.of.energy`
M_S=EARTHQUAKES$`Magnitude`
n=length(E)
fit=lm(E~M_S)
plot(fit,1)
plot(fit$fitted.values,fit$residuals,ylab='residuals')
abline(0, 0)

#sqrt_v<-sqrt((1-1/n- (M_S-mean(M_S))^2/((n-1)*var(M_S))))*0.4572
#plot(sqrt_v,ylab='sqrt(v)')
#fit$residuals/sqrt_v
standardized_residuals<-rstandard(fit)
plot(fit$fitted.values, standardized_residuals,ylab='standardized residuals') 
abline(0, 0)

##normal Q-Q plot of residuals
qqnorm(fit$residuals,pch = 1, frame = FALSE)
qqline(fit$residuals, col='red',lwd = 2)

##normal Q-Q plot of standardized residuals, the qqline is y=x
qqnorm(standardized_residuals,pch = 1, frame = FALSE)
qqline(standardized_residuals, col='red',lwd = 2)


#################################################################
## It is often hard to judge residual plots without prior experience,
## so it is helpful to generate some artificial plots 
## where the true relationship is known.
n=200
x=runif(n,1,15)
y=0.4*x^2+rnorm(n,sd=3)
fit=lm(y~x)
plot(fitted(fit), residuals(fit))

x=runif(n,1,15)
y=1+x+2*sin(x)+rnorm(n,1)
fit=lm(y~x)
plot(fitted(fit), residuals(fit), type="p")
#plot(fit,1)

x=runif(n,1,15)
y=1+ x+ (1+0.3*x)*rnorm(n, sd = 3)
fit=lm(y~x)
plot(fitted(fit), residuals(fit))
#plot(fit,1)


######################################
par(mfrow=c(1,2))
x1=rnorm(200)
qqnorm(x1, pch = 1, frame = FALSE)
qqline(x1, col='red',lwd = 2)

x2=runif(200)
qqnorm(x2, pch = 1, frame = FALSE)
qqline(x2, col='red',lwd = 2)