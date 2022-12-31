#2022.11.22
#linear model_2

#請搭配Vianney講義看

#lm(x ~ y)
#take Multiple R-squred

#lm(x ~ y1 + y2 + y3 + ....)
#take Ajusted R-squred(有眾多variables to improve your R-square value, but not each one is meaningful)
#notice that y1, y2, y3...are independent, and please put sth meaningful

#linear model can test normality, homoscedasticity, outlier

#Influential observations -> see outlier

#qqPlot can give you confidence level -> determine whether the outlier should be take or not

#spreadLevelPlot(fit1) -> test homoscedasticity, plot should be horizontal

#gvmodel <- gvlma(fit1) => test four assumptions in linear regression: normality, heteroscedasticity, and linearity(線性)


#Dealing with violation of assumption
#Think....how to transform your data (e.g., log, x2)
#Sooo cool => count lamba value to see which kind of formation you need to transfer to.
#boxcox() -> can plot the most possibility style that your data should be transform to 
#--> similar thing to spreadLevelPlot(fit1)!!!

#用新的轉換過的資料再作之前的線性檢定
#不一定有better result

#Vianney tried 'Weighted Least Squares Regression
#不一定有better result.....


#Bootstrap: is a method of random sampling with replacement
#to prove it is not too bad

#if I have good model, 不管重做幾次，結果都是stable的


#set.seed(): get sequence of random number
set.seed(2021)
n <- 1000
x <- rnorm(n) #set it is a normal distribution
y <- x + rnorm(n)
population.data <- as.data.frame(cbind(x, y))

sample.data <- population.data[sample(nrow(population.data), 20, replace = TRUE),] #選20個

population.model <- lm(y~x, population.data)
summary (population.model)


#Cross Validation: stalibility of my model
#not common
#like bootstrap, but it take samples, do not put it back
#cv.lm()

# Assessing R2 shrinkage using 10-Fold Cross-Validation
#it is a predict value, to see how much it fit my original data
#cor(): correlation coefficient

#if R square value drop 亂七八糟 not stable, that mean your model is not that fit


#Comparing Models
library(MBESS)
data(prof.salary)

# compare model 3 to model 1 - stepping approach, evaluating a new variable (cits)
anova(fit.prof1,fit.prof3)# note this is anova, not Anova


#BIC & AIC are going to see whether A model is better than B model
#Both are based on a balance between the model fitness and its complexity
#The lower the better 
#AIC: 加哪些parameter會得比較好的model
#BIC: 哪一個model比較好

model.full <- lm(salary ~., data = prof.salary)
#. 點點表示把所有data中的variables都拿出來
model.partial1 <- lm(salary ~. -sex, data = prof.salary)
#take everthing but no sex





