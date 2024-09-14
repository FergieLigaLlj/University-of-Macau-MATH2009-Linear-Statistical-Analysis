#Input data from question 4 of assignment 4
x=c(2.2,1.3,3.6,2.1,3.8,1.4,2.3,2.6,3.4,2,2.7)
y=c(13.5,5.2,52.9,13.5,66.3,5.8,15.2,23.7,46.5,12.5,22.6)
#Number of elements in x
n=length(x)
#find linear best line
fit=lm(y~x)
#summary fitted line
summary(fit)
#plot Residuals against Fitted Values Way 1
plot(fit,1)
#plot Residuals against Fitted Values Way 2
plot(fit$fitted.values,fit$residuals,ylab='residuals')
abline(0, 0)
#plot Standardized Residuals against Fitted Values
standardized_residuals<-rstandard(fit)
plot(fit$fitted.values, standardized_residuals,ylab='standardized residuals') 
abline(0, 0)