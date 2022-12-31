?cor.test() #get correlation coefficient
#not work in matrix

#read the file
rairuoho <- read.delim(url('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt'))

cor.test(rairuoho$day6, rairuoho$day7)
corrplot() # draw correlation plot with coefficient and p-value
#if non-significance (p>0.05) 畫叉叉
??rcorr() #show出我要的資料，這邊是找p-value

#install.packages('corrplot')
library(corrplot)
library(tidyverse)
library(broom)
library(Hmisc)
library(corrplot)
library(MASS)
library(car)
library(interactions)
library(yarrr)

library(readr)
library(lme4)
library (lmerTest)
library(nlme)
library(gvlma)

corr<-cor(rairuoho[,1:6])
corr # cor.test does not work on Matrix

p.val<-rcorr(as.matrix(rairuoho[,1:6]))$P
corrplot(corr,type='upper',method='color', 
         addCoef.col = "black", p.mat=as.matrix(p.val), sig.level = 0.05,
         title = "Correlation Matrix", mar = c(2,0,2,0), diag=F)

plot(rairuoho$day6, rairuoho$day7)
abline(lm(rairuoho$day7~rairuoho$day6), col="red", lwd=2)

# remember `ggplot`
ggplot(rairuoho, aes(x = day6, y = day7)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")  

# regression
#it is used to predict a quantitative outcome of a `dependent` variable y 
#on the basis of one single `independent` predictor variable x.

#ϵ  is the error term (also known as the residual errors)
#the value you do not need to explain why
#最小平方差啦!
#Residual Sum of Squares (RSS).最小平方和

#The average variation of points is the Residual Standard Error (RSE). 最小差的平均值
#This is one the metrics used to evaluate the overall quality of the fitted regression model. 
#The lower the RSE, the better it is,最好靠近 0

#RSS is as minimal as possible. 
#To get least squares regression or ordinary least squares (OLS) regression.

#model
model1 <- lm(Petal.Width ~ Petal.Length, data = iris) #lm(dependent value y ~ independent value x)
#if I don know who is dependent or independent, assume it
model1$coefficients

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(fill=Species),shape = 21, size=5) +
  stat_smooth(method = "lm", col = "blue") 

summary(model1) #秀出不同項目:call, residuals(四分位數), coefficients, signif.codes
#call means call the formula you use
#coefficient: coefficient+t value + Std.error+Pr (with***<0.001, **<0.01, *<0.05)
#degree of freedom: x-1 & y-1 共-2
#The higher the t-statistic (and the lower the p-value), the more significant the predictor


confint(model1)

sigma(model1)*100/mean(iris$Petal.Width)


#Multiple regression
fit1 <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)
summary(fit1)

# Other useful functions
coefficients(fit1) # model coefficients
confint(fit1, level=0.95) # CIs for model parameters
fitted(fit1) # predicted values
residuals(fit1) # residuals
anova(fit1) # anova table
vcov(fit1) # covariance matrix for model parameters
influence(fit1) # regression diagnostics

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit1) #plot multiple plot to understand normality, homogenity, outlier

# Assessing Outliers
outlierTest(fit1) # Bonferonni p-value for most extreme obs
#會列出哪一個是outlier

qqPlot(fit1, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit1) # leverage plots

#It is not outlier, but still influential
# Influential Observations
# added variable plots
avPlots(fit1)

# to see outliers
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(iris)-length(fit1$coefficients)-2))
plot(fit1, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(fit1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit1, main="QQ Plot")

# distribution of studentized residuals
sresid <- studres(fit1)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit1)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit1)

# Evaluate Collinearity
vif(fit1) # variance inflation factors
sqrt(vif(fit1)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(fit1)
# Ceres plots
ceresPlots(fit1)

# Test for Autocorrelated Errors
durbinWatsonTest(fit1)

gvmodel <- gvlma(fit1)
summary(gvmodel)
library(gvlma)
