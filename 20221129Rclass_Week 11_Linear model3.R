#2022.11.29 Linear model continue--

#F-test: analyze variance----
#ratio of variance

#in simple linear model, F value is t value 的平方

#H0: 每條slope都依樣

#F = MSM / MSE = (explained variance) / (unexplained variance)



#ANOVA
#like make a boxplot, comparing the variance, actually we are comparing the mean
#it is a linear regression
#也沒有可以提供比linear model更多的東西
#但仍在很多科學文章可看到，所以Vian才講

#between
#within
#total

#sum of square 很重要

#type1 anova: anova()做ANOVA前，先做lm, 再將lm放到anova

#one category: one-way anova
#two category: two-way anova
#還有很多不同的ANOVA  three, four, five......

# full-factorial, between-subjects ANOVAs
#observations (data) are assigned to a unique combination of factors
#install.packages('yarrr')
library(yarrr)
pirateplot(formula = time ~ cleaner + type,
           data = poopdeck,
           ylim = c(0, 150),
           xlab = "Cleaner",
           ylab = "Cleaning Time (minutes)",
           main = "poopdeck data",
           back.col = gray(.97), 
           cap.beans = TRUE, 
           theme = 2)

#anova(a ~ b*c) b*c means that b & c這兩個因子的interaction

#aov可直接導入公式

#remember: linear model is the base of ANOVA

#estimate intercept: missing variable in out com 


#ANOVA不能有missing data, type 1一定要每個category有相同數量的觀測值
#ANOVA type2&3 -> Anova() => library(car), can assign what type of ANOVA you want
#unbalance data: 用 ANCOVA 


#Random effect


#自己做作業，最後一題





# students data set url 
students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') 

prop.table (table(students$gender))
table(students$gender, students$shoesize,students$population)
?aggregate

?apply
boxplot(iris$Sepal.Length ~ iris$Species)
psych::describeBy(iris, iris$Species)
table(iris$Species)

tapply(iris, iris$Species, median)

library(ggplot2)
ggplot(students, aes(x = height, y = shoesize)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


w<-(1:100)
z<-exp(1:100)
cor.test(w,z,method='pearson') # super low
cor.test(w,z,method='spearman') #super high
plot(z~w)

?chisq.test()

F <- matrix(nrow=4,ncol=2,data=c(33,14, 8,18,31,25,14,12))
chisq.test(F) # alternative see `fisher.test`

obs <- c(750, 50, 200)
exp <- c(0.60, 0.35, 0.05)
chisq.test (x=obs, p=exp)
t.test (students$height, mu=170)
?t.test()
?wilcox.test()
qqnorm(students$height) 
qqline(students$height) 
