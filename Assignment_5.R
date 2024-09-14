##input data by reading files
Assignment_5<-read.table('C:/Users/YangYiShu/Desktop/University Life/weight.txt',header =TRUE)
y=Assignment_5$"weight"
x1=Assignment_5$"abdom"
x2=Assignment_5$"thigh"
##summary fit of bivariate linear model
fit=lm(y~x1+x2)
summary(fit)
##added variable plot
fit1=lm(y~x1)
fit2=lm(x2~x1)
fit3=lm(fit1$residuals~fit2$residuals)
plot(fit2$residuals,fit1$residuals)
abline(fit3)
summary(fit3)
#compute sample correlation of x1 and x2
s12=sum((x1-mean(x1))*(x2-mean(x2)))
s11=sum((x1-mean(x1))^2)
s22=sum((x2-mean(x2))^2)
sample_correlation = (s12)/((s11*s22)^(1/2))
print(sample_correlation)