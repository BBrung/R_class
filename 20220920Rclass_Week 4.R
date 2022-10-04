#R class Week 4
#Date:2022-10-04

#Discussion of Practice2.2
str()
typeof()

#Data types and structures
#data types
mode() # to show that it is numeric, character...
typeof() #to show that it is [integer(整數), double(有小數點的)]for numeric, logical

x <- 4 #will be seen as double
typeof(x)

x <- 4L # will be seen as integer when add L in the end
typeof(x)

x <- '3'
y <- '8.5'
plot(x,y) #be careful that character can also be printed by plot


x <- c(TRUE, FALSE, FALSE, TRUE)
mode(x)

x1<-c(1,0,0,1)
x2 <- as.logical(c(1,0,0,1))
typeof(x1)
typeof(x2)

a <- c("M", "F", "F", "U", "F", "M", "M", "M", "F", "U")
typeof(a) # mode character
class(a)# class character
a.fact <- as.factor(a)
class(a.fact)# class factor
mode(a.fact)
typeof(a.fact)

a.fact

attributes(a.fact) #這個卻是視 F M U是字串

levels(a.fact)

factor(a)
factor(a, levels=c("U","F","M")) #change the order 


#Practice 3.1
iris.sel <- subset(iris, Species == 'setosa' | Species == 'virginica')
#We remove the data, but why it still there?
levels(iris.sel$Species)  # 3 species are still there
boxplot(Petal.Width ~ Species, iris.sel, horizontal = TRUE) #都還畫得出來
#要多一個步驟!!!
droplevels(iris.sel$Species)
boxplot(Petal.Width ~ Species, iris.sel, horizontal = TRUE) #就沒有了!!

#****非常重要的是去檢查data type & structure of my data set
#install.packages('lubridate')
library('lubridate')


rownames(iris.sel) = seq(length=nrow(iris.sel))

x <- c(23, NA, 1.2, 5)
mean(x)
mean(x, na.rm = T)


#NA: 應該有數值在那但數值未知
#NULL: 從來不可能有這個數值(不存在)

#vector can only combine by one type of data
x <- c( 1.2, 5, "Rt", "2000")
#R will convert the element types to the highest common mode following the order
#NULL < logical < integer < double < character
#所以全部變character


#Matrix can only combine by one type of data
#Data frame can combine by multiple type of data
A<- data.frame(
  x = c(7.3, 29.4, 29.4, 2.9, 12.3, 7.5, 36.0, 4.8, 18.8, 4.2),
  y = c(5.2, 26.6, 31.2, 2.2, 13.8, 7.8, 35.2, 8.6, 20.3, 1.1) )
B <- c(TRUE, FALSE)
C <- c("apples", "oranges", "round")
my.lst <- list(A = A, B = B, C = C)
str(my.lst)
?names

#要叫出list裡的東西，用$或是[[]](more common)
names(my.lst)

#coercing data轉換資料

#Practice 3.2----
#The first element
first <- data.frame(before_diet = c(104, 95, 87, 77, 112), 
                    after_diet = c(104, 95, 87, 77, 112))
mode(first)
str(first)

#The second element
second <- data.frame(subject = c(row.names(first)), 
                     weight_loss = (first$before_diet - first$after_diet)/100)
mode(second)
str(second)


#The third element----

# install.packages("ggplot2")
# install.packages("pryr")
require(dplyr)
library(ggplot2)
library(pryr)

# heart curve formula
heart <- quote((x^2 + y^2 - 1)^3 - x^2 * y^3)

# formula for heart curve at a given x
heart_at_x <- function(x) {
  function(y) eval(substitute_q(heart, list(x = x)), list(y = y))
}

# trace the heart curve
# by evaluating the heart curve formula at each x, then finding the roots of the
# resulting formula in y; e.g. a x==0, find the roots of (y^2 - 1)^3 = 0
# broken up into upper and lower parts (h_y1 and h_y2)
heart_x <- seq(-1.136, 1.136, 0.001)
heart_y_lower <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(-2, 0.6))$root)
heart_y_upper <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(0.6, 2))$root)

# put together data frame
heart_df <- data.frame(x = rep(heart_x, 2), 
                       y = c(heart_y_lower, heart_y_upper))

# show outline
#with(heart_df, plot(x, y))

# create a data frame with one row per x, so we can fill in the heart
heart_df_minmax <- data.frame(x = heart_x,  
                              y_min = heart_y_lower, 
                              y_max = heart_y_upper)

set.seed(20150214)

# fill in the heart by generating random deviates at each x 
# and rejecting those that fall outside the heart curve
heart_full <- apply(heart_df_minmax, 
                    1, 
                    function(w) {
                      x <- w["x"]
                      y_min = w["y_min"]
                      y_max = w["y_max"]
                      y <- rnorm(2, mean = 0.33)
                      y <- y[between(y, y_min, y_max)]
                      x <- x[any(is.finite(y))]
                      data.frame(x, y, row.names = NULL)
                    })

# change from list to data frame
heart_full <- bind_rows(heart_full)

# add random numbers for color and size
heart_full <- heart_full %>% 
  mutate(z1 = runif(n()), 
         z2 = pmin(abs(rnorm(n())), 3), 
         order = runif(n())) %>%
  arrange(order)


# plot the heart
p <- ggplot(heart_full, 
            aes(x, y, color = z1, size = z2)) + 
  geom_point(pch = -1 * as.hexmode(9829)) + 
  scale_color_gradient(limits = c(0, 1), low = "red3", high = "pink") + 
  scale_size(limits = c(0, 3), range = c(0.1, 20)) + 
  theme_bw() +
  theme(legend.position="none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(face = 'bold', size = 60, hjust = 0.5, vjust = -15))+
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  ggtitle("I Love R")
p



#Make a list called BUBBLE_DIET
BUBBLE_DIET <- list(first, WEIGHT_LOSS = second, ILOVER = p)
mode(BUBBLE_DIET)
str(BUBBLE_DIET)





#521.1314
love.you.forever <- function(x){
  ((x+52.8)*5-3.4343)*2-10*x}
love.you.forever(10)

#Love curve
dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l"))































