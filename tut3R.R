url <- "https://tinyurl.com/mtktm8e5"
insurance <- read.csv(url)
library(ggplot2)

##
ggplot(data=insurance,mapping=aes(x=age,y=expenses))+
  geom_point(color="hotpink1",alpha=0.7,size=2,position="jitter")+
##color=？，点颜色；alpha=?,透明度;size=?,点大小。
##position = "jitter":扰动点，fluctuation.
  geom_smooth(method = lm)

ggplot(data=insurance,mapping=aes(x=age,y=expenses,color=smoker))+
  geom_point(alpha=0.7,size=2,position="jitter")+
  ##color=？，点颜色；alpha=?,透明度;size=?,点大小。
  ##position = "jitter":扰动点，fluctuation.
  ##color=smoker放ggplot函数里面，对散点图 bestfit都有效
  geom_smooth(method = lm)
###
ggplot(data=insurance,mapping=aes(x=age,y=expenses))+
  geom_point(alpha=0.7,size=2,position="jitter",aes(color=smoker))+
  ##color=？，点颜色；alpha=?,透明度;size=?,点大小。
  ##position = "jitter":扰动点，fluctuation.
  ##color=smoker放geom_point函数里面所以只对散点图有效
  ##对面下面的best fit无效
  geom_smooth(method = lm)
###
ggplot(data=insurance,mapping=aes(x=age,y=expenses))+
  geom_point(alpha=0.7,size=2,position="jitter",aes(color=bmi))+
  ##color=？，点颜色；alpha=?,透明度;size=?,点大小。
  ##position = "jitter":扰动点，fluctuation.
  ##color=smoker放aes函数里面
  geom_smooth(method = lm)+scale_x_continuous(breaks=seq(0,70,5))
########
insurance$obese=ifelse(insurance$bmi>=30,"obese","not obese")
ggplot(data=insurance,mapping=aes(x=age,y=expenses))+
  geom_point(alpha=0.7,size=2,position="jitter",aes(color=bmi))+
  ##color=？，点颜色；alpha=?,透明度;size=?,点大小。
  ##position = "jitter":扰动点，fluctuation.
  ##color=smoker放aes函数里面
  geom_smooth(method = lm)+scale_x_continuous(breaks=c(19,30,55,69))+
  facet_wrap(~obese)
#####
ggplot(data=insurance,mapping=aes(x=age,y=expenses))+
  geom_point(alpha=0.7,size=2,position="jitter",aes(color=smoker))+
  ##color=？，点颜色；alpha=?,透明度;size=?,点大小。
  ##position = "jitter":扰动点，fluctuation.
  ##color=smoker放geom_point函数里面所以只对散点图有效
  ##对面下面的best fit无效
  geom_smooth(method = lm)+
  facet_wrap(~ obese)