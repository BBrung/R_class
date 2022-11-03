#2022.11.01_Rclass Week8
#Statistics

#Most important is hypotheses
install.packages('psych')

library (psych)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)

library(carData)

# students data set url 
students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') 
# to write it:
write.table(students, file = "Data/students.txt", sep = "\t", row.names=T)

summary(students)

psych::describe(students) #psych::describy可以以不同類別去做統計
 
psych::describeBy (students,students$gender) #for each gender
psych::describeBy (students, list(students$gender,students$population))  #for each gender and population

# one variables
prop.table (table(students$gender)) #table in proportion

# two variables
table(students$gender, students$shoesize)
prop.table (table(students$gender, students$shoesize)) #table in proportion

# three variables, with nicer formatting
ftable(students$gender, students$shoesize,students$population)

mean(students$height)
ind.male <- students$gender == 'male'
mean(students$height[ind.male])

#apply function like a loop to calculate
aggregate(students$height,list (students$gender),median) #get dataframe
t <- tapply(students$height,students$gender, median) #get a list

#===============================================================================
#Practice7.1
#Use iris dataset
iris <- iris
#1) Use boxplot as visual summaries then extract descriptive statistics 
#by species (describeBy or others)  ****boxplot is important to start statistics****

plot1 <- boxplot(iris$Sepal.Length~iris$Species)
plot2 <- boxplot(iris$Sepal.Width~iris$Species)
plot3 <- boxplot(iris$Petal.Length~iris$Species)
plot4 <- boxplot(iris$Petal.Width~iris$Species)
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)  #!!不能用 因為ggplot才能用grid.arrange

#以下示範用gglot畫
#ggplot(iris, aes(x = Species, y = Sepal.Length))+
#    geom_boxplot()

#grid.arrange()  library(gridExtra)可以把四個圖排一起

psych::describeBy(iris, iris$Species)

#(2) Count number of observations in each trait and species (be creative). 
#it should results in a table of 3 species x 4 traits filled with “50” values
#其他整理方式
library(tidyverse)
iris %>% group_by(Species) %>% summarise(across(c(1:4),mean)) #can put anything at the place 'mean'

#(3) Calculate the median of each variable by Species, 
#then calculate the mean by Species but for the Sepal.Length only
aggregate(iris[,1:4],by=list(iris$Species), median)
tapply(iris$Sepal.Length , iris$Species, mean)


# dataset hypotheses?
x<-students$height
y<-students$shoesize
s<-students[,1:2] # a matrix
# Pearson correlation
 cor(x,y)
 cor(s)
cor.test(x,y)  #show good correlation

ggplot(students, aes(x = height, y = shoesize)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") #get smooth line


#Practice7.2
#改其中一個student的值，看correlation、pvalue變化


# Spearman correlation (monotonic)
# cor(x,y, method ='spearman')
cor.test(x,y, method ='spearman')

w<-(1:100)
z<-exp(x)
cor.test(w,z,method='pearson') # super low
cor.test(w,z,method='spearman') #super high




#Cast 240 times a die. We counted occurrence of 1,2,3,4,5,6
die<-data.frame(obs=c(55,44,35,45,31,30), 
                row.names=c('Cast1','Cast2','Cast3','Cast4','Cast5','Cast6'))
die #Is this die fair? Define H0 and H1.  

chisq.test(die)
# I am cheating

obs <- c(750, 50, 200)
exp <- c(0.60, 0.35, 0.05)
chisq.test (x=obs, p=exp)

F <- matrix(nrow=4,ncol=2,data=c(33,14, 8,18,31,25,14,12))
chisq.test(F) # alternative see `fisher.test`

# One sample
t.test (students$height, mu=170)  #mu = mean
# Two sample (with equal variances)
t.test (students$height~students$gender, var.equal = TRUE) #var.equal***
#Welch t test: 沒有相同的變異量

# Two sample (with unequal variances, default option when using t.test) 
t.test (students$height~students$gender)
# Two sample paired t.test
t.test (students$height~students$gender, paired=T)


#t.test 有不同理論，R會以保守地去算
#t.test: independent variables

#paired t.test: non-independent 如:同一人前後變化

#Practice7.3 ===>> 回家自己做


#用多個test去test是否顯著
#找?OOXXtest -> 裡面有那個test的文獻可以去看是算什麼的
#few sample -> 常態性分布就很重要，很多sample常態性分布就不重要


#Practice7.4
#How to calculate t value

getAnywhere("t.test.default")  #可知道原本的function的function
#compare two value
?pt


