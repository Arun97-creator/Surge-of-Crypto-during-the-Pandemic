data <- read.csv("D:/Research Paper Finance/Finance Data/quantile-regression/USA.csv")
head(data)
library(forecast)
library(smooth)
library(graphics)
library(datasets)
library(tseries)
library(ggplot2)
library(fpp2)
library(imputeTS)
library(xts)
datats = ts(data[,3], start = c(2020,1), frequency = 365.25)
install.packages("quantreg")
library(quantreg)



summary(data)
# we can see at different quartiles price of BTC rices

#Let's  look at the graph with ols line
ggplot(data,aes(SUM,Close))+
  geom_point() +
  geom_smooth(method="lm",color="red")+
  geom_quantile(quantiles = seq(0.05,0.95,by = 0.05))

head(datats)
install.packages("ggplot2")
library(ggplot2)

library(lmtest)

install.packages("olsrr")



#Quantile regression

quantreg25 <- rq(Close ~ SUM,tau = 0.25,data=data)

quantreg50 <- rq(Close ~ SUM,tau = 0.50,data=data)

quantreg75 <- rq(Close ~ SUM,tau = 0.75,data=data)

ols <- lm(Close ~ SUM,data=data)

install.packages("stargazer")
library(stargazer)

stargazer(ols,quantreg25,quantreg50,quantreg75,type = "text")


#Graph to visualize all quantiles
quantreg.all <- rq(Close ~ SUM,tau = seq(0.05,0.95,by=0.05),data=data)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)





