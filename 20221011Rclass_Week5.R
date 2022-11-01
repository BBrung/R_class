#2022.10.11_R class_week5
#plot

plot(Petal.Length ~ Petal.Width, data = iris) # pairwise

plot(iris$Petal.Length ~ iris$Petal.Width) # using the $ operator

# add labels to x- and y-axes, title 
?plot
plot(Petal.Length ~ Petal.Width, data = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     pch = 19, cex=2, 
     col = rgb (1,0,0,0.10))
#pch (plotting character)
#cex will control his size (character expansion)
#color: https://htmlcolorcodes.com/
#color: https://www.color-hex.com/

#set the different color by different species with ifelse()
col.iris<-ifelse(iris$Species=='setosa','purple',ifelse(iris$Species=='versicolor','blue','pink')) 
col.iris
plot(Petal.Length ~ Petal.Width, data = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     pch = 19, cex=2, 
     col = scales::alpha(col.iris, 0.2))

#create a legend and overlap on my plot
legend(x="bottomright", pch= 19, cex=1.0, 
       legend= c("versicolor","setosa", "virginica"), 
       col=levels(as.factor(scales::alpha(col.iris, 0.2)))) #we only need three level of species 


legend(x="bottomright", pch= 19, cex=1.0, 
       legend= c("versicolor","setosa", "virginica"), 
       col=scales::alpha(levels(as.factor(col.iris)), 0.2)) 
#這樣和上面可以用出一樣的結果(上面先把字串變色號，再分有幾個level的色號；下面是先把字串分成幾個level，再把這幾個變成色號)

ratio<-iris$Petal.Length/iris$Sepal.Width  # ratio between the length of petal and the width of Sepal
plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     cex.axis=1.0, cex.lab=1.5, cex.main=1.5,
     pch = 19, las=1, cex= ratio * 2, 
     col = scales::alpha(col.iris, 0.2))

legend(x="bottomright", pch= 19, cex=1.0, legend= c("versicolor","setosa", "virginica"), col=levels(as.factor(scales::alpha(col.iris, 0.2))))

#las把軸上的字變成正向
#cex.axis x軸字大小


#pairs allows a quick examination of the relationship among variables (scatterplot matrix)
pairs(iris[1:4], pch=19, col = scales::alpha(col.iris, 0.2))


blossom<-NULL
blossom$year <- 2010:2019
blossom$count.alaska <- c(3, 1, 5, 2, 3, 8, 4, 7, 6, 9)
blossom$count.canada <- c(4, 6, 5, 7, 10, 8, 10, 11, 15, 17)
as.data.frame(blossom)

plot(count.alaska ~ year,dat = blossom, type='l',
     ylab = "No. of flower blossoming") 
#type: 預設散布圖，'l'表示折線圖
#type='b'  -> plot both points and line
#line type:lty=2
#line width : lwd=0.5

plot(count.alaska ~ year,dat = blossom, type='b', pch=20,
     lty=2, lwd=0.5, col='red',
     ylab = "No. of flower blossoming") 
lines(count.canada ~ year,dat = blossom, type='b', pch=20,
      lty=3, lwd=0.5, col='blue') #add the other line overlap on last plot
#But plot的界線範圍是基於寫在plot()中的那條，所以lines()加上去的可能在範圍外

#change the range by xlim & ylim, 也可以用range包入所有表格有的值
y.rng<-range(c(blossom$count.alaska, blossom$count.canada))
plot(count.alaska ~ year,dat = blossom, type='l', ylim = y.rng,
     lty=2, lwd=1, col='red',
     ylab = "No. of flower blossoming") 
lines(count.canada ~ year,dat = blossom,
      lty=1, lwd=1, col='blue')



iris.ver<- subset(iris, Species == "versicolor")
iris.vir<- subset(iris, Species == "virginica")

y.rng <- range( c(iris.ver$Petal.Length, iris.vir$Petal.Length) , na.rm = TRUE) 
x.rng <- range( c(iris.ver$Petal.Width, iris.vir$Petal.Width) , na.rm = TRUE) 

# Plot an empty plot
#by type = 'n' 
plot(Petal.Length ~ Petal.Width, dat = iris.ver,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     cex.axis=1.0, cex.lab=1.5, cex.main=1.5, type='n',
     xlim=x.rng,  ylim=y.rng)

# and add somthing step by step (can be more customized)

# Add points for versicolor
points(Petal.Length ~ Petal.Width, dat = iris.ver, pch = 20,cex=2, 
       col = rgb(0,0,1,0.10))

# Add points for versicolor
points(Petal.Length ~ Petal.Width, dat = iris.vir, pch = 20,cex=3, 
       col =  scales::alpha('#fc03c6', 0.2))

# Add legend
legend("topleft", c("versicolor", "virginica"), pch = 19, cex=1.2,
       col = c(rgb(0,0,1,0.10), scales::alpha('#fc03c6', 0.2)))


#boxplot: to summarize the distribution of a data batch
boxplot(iris$Sepal.Width,iris$Sepal.Length, 
        iris$Petal.Width,iris$Petal.Length, 
        names = c("Sepal.Width", "Sepal.Length", "Petal.Length","Petal.Width"), 
        main = "Iris flower traits")  # plot four varibles at the same time

#remove outliner: outline = FALSE
#make the plot horizontal: horizontal = TRUE


boxplot(Sepal.Width ~ Species,iris) 


#reorder the boxplots based on the median value. 
#By default, boxplot will order the boxplots following the factor’s level order.

iris$Species.ord <- reorder(iris$Species,iris$Sepal.Width, median)
levels(iris$Species.ord)
   
boxplot(Sepal.Width ~ Species.ord, iris)

#hist
hist(iris$Sepal.Width, xlab = "Width of Sepal", main = NA)

hist(iris$Sepal.Width, xlab = "Width of Sepal", main = NA, breaks=20)
#break:分成幾分


dens <- density(iris$Sepal.Width)
plot(dens, main = "Density distribution of the width of sepal")

dens <- density(iris$Sepal.Width, bw=0.05) #bw = bandwidth
plot(dens, main = "Density distribution of the width of sepal")
?density
#test 是否常態分佈
qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width)

#===============================
#Prctice4.1----
#read the file
rairuoho <- read.delim(url('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt'))
# the length of the plant at day3 and the length at day7: create a scatterplot
plot(day3 ~ day7,dat=rairuoho,
     xlab = 'Length at day 3', 
     ylab = 'Length at day 7', 
     main = 'Relationship between the length at day 3 and day 7')

#Examine if the length at day7 is approaching a normal distribution
#hist
hist(rairuoho$day7, xlab = 'Length at day 7', main = 'Histogram of day 7')

#density plot:Kernel Density Estimation-> 算出現在那個區間的可能性
plot(density(rairuoho$day7), main = "Density distribution of the length of day 7")

#QQplot
qqnorm(rairuoho$day7)
qqline(rairuoho$day7) #Ans: Yes

#How does the treatment affect the length at day7?
boxplot(day7 ~ treatment, data = rairuoho, las = 1, ylab = 'Length at day 7')

#Compare the length of the Easter Grass among days(scatterplot matrix)
pairs(rairuoho[,1:6])

#===============================
#See the current graphical settings using:
par() # graphical options
par(las = 2) # graphical options在裡面可以一次改所有設定，接下來畫的圖都會照這個設定的畫

#reinitialize the graphical parameters using dev.off()
dev.off()

#Text annotation text or in the margin using mtext
text (x=1, y=1,'text')
mtext ('text', side=1, line=1)


#Export the plot
#先開畫布的概念，再畫上圖，再關掉畫布
tiff(filename = "Figures/iris_plot.tif", width = 5, height = 6, units = "in", compression = "none", res = 300)
plot()
dev.off()

#==================================================
#Practice4.2----
pdf('Practice4.2_rairuoho scatterplot.pdf', width = 6, height = 5)
plot(day7 ~ day3,dat=rairuoho,
     xlab = 'Length at day 7', 
     ylab = 'Length at day 3', 
     main = 'Relationship between the length at day 3 and day 7',
     ylim = c(0, 130))
dev.off()
#==================================================

#Lattice:(multi-panel display) used to visualize multivariate data 
library(lattice)

densityplot(~ Petal.Length | Species, iris, plot.points = "", layout=c(1,3))  
#layout= c(1,3) -> 1column, 3row
histogram(~ Petal.Length | Species, iris, plot.points = "", nint = 20, layout=c(1,3))


qqmath(~ Petal.Length | Species, iris, plot.points = "", nint = 20, layout=c(3,1))

iris$variety<-rep(c(rep('main',25), rep('hybrid',25)),3) # fake variable
#boxplot
bwplot(Petal.Length ~  variety|Species, iris)

#scatter plot
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2), type=c("p","g"),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

#line plot
xyplot(Sepal.Length + Sepal.Width + Petal.Width ~ Petal.Length  | Species,
       data = iris[order(iris$Petal.Length),], scales = "free", layout = c(2, 2), 
       type=c("l"), auto.key = list(x = .6, y = .7, corner = c(0, 0)))


#To get the list of graphic parameters and their values call:
trellis.par.get()


#primary layering components,
ggplot(dat1l , aes(x = Year, y = Yield)) + geom_point()
#dataset, what I want to plot, displaya type)
#The dataset that houses the data to be plotted;
#The aesthetics which describe how data are to be mapped to the geometric elements (color, shape, size, etc..);
#The geometric elements to use in the p

ggplot(dat1l , aes(x = Year, y = Yield, color = Crop)) + geom_point()
# Crop means different color of crop in dataset





###Exporting ggplot to an image
###it is important
ggplot()......
ggsave("Figures/fig0.png", plot = p1, width = 6, height = 2, units = "in", device = "png")



