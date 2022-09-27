#If you already have R on your computer, an alternative is to run the following line of code in R native GUI (it will tell you if your R version is out-of-date ):
  
# R update (to run in R native GUI only)
if (!require(installr)) {
  install.packages("installr")
  require(installr)
  }
updateR()
#This will start the updating process of your R installation. 
#It will check for newer versions, and if one is available, 
       #it will guide you through the decisions you need to make.
#You may have to choose a CRAN mirror if it is your first download. 
#A mirror is just a copy of the CRAN archives. Choose one in Taiwan, it is usually a bit faster to download and install.


#Install Approximate Bayesian Computation (ABC)
#install.packages("abc") 
# use/load the package `abc` with its name
library("abc")

#install package 'vegan'
#install.packages('vegan')
library('vegan')
#find packages here
#use task views available at http://cran.r-project.org/web/views. 
#It group packages to subject areas such environmetrics, multivariate, etc.

#use online discussion, forum, and docs. Among many others sources, 
#you will often be directed to discussion in stackoverflow


#ls() will provide you with all the objects in the memory.
# clean objects in memory
rm(list=ls())


#set pathway
setwd('C:/Users/user/Downloads')
#install.packages('readxl')
library(readxl) # load the package `readxl'
read_excel('reef_fish.xlsx') # automatically print on my screen
#read_excel('Data/reef_fish.xlsx') # Data means if the file is in Data in Downloads, Dont need to change path by type like this

#Read .txt -> use read.table
#1.Read the txt file to check the format
#2.to decide header、sep、dec(decimal point小數點)
fish <- read.table('reef_fish.txt', header = T, sep ='\t')


#You can also use `file.choose()’ to locate directly the file on computer

#check the version is the newest:click on “Help” > “Check for updates”

#改顏色、大小
#Tools>Global Options>Appearance>Layout

# import data set and create an object in R studio + simple plot 
fish <- read.table('reef_fish.txt', header=T, sep='\t', dec='.')
barplot(fish$richness, main="Top 10 reef fish Richness (Allen, 2000)", horiz=TRUE, names.arg=fish$country, cex.names=0.5, las=1)


