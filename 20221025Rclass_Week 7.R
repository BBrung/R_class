#2022.10.25_R_class_Week 7
#install.packages('animation')
#install.packages('gganimate')
library(animation)
library(ggplot2)
library (gganimate)


#loop & function----
#for is a kind of control flow (?control instead of ? for)
#for cannont be a variable

#?function also cannot be read

#loop, while, repeat are common control flow function
#They can repeat the action util it fit or unfit the condition

#for (cycle number of loop){
     #what I want you do
#}


system.time() #計算運算時間
#loop 運算時間長

grow <- function (growth.rate) { # argument "growth.rate" of function "grow" 
  num_gen<-10
  generation<-1:num_gen
  N <- rep (0,num_gen)
  N[1] <- 1
  for (i in 2:num_gen) {
    N[i]=growth.rate*N[i-1] # not the use growth.rate argument
  }
  plot(N~generation,type='b', main=paste("Rate =", growth.rate)) 
}

#===============================================================================
#Practice2.1
#where both arguments: the growth.rate and number.generation can be customized.
grow2 <-function (growth.rate, number.generation) { # argument "growth.rate" of function "grow" 
  num_gen<-number.generation
  generation<-1:num_gen
  N <- rep (0,num_gen)
  N[1] <- 1
  for (i in 2:num_gen) {
    N[i]=growth.rate*N[i-1] # not the use growth.rate argument
  }
  plot(N~generation,type='b', main=paste("Rate =", growth.rate,',', 'Generation =', num_gen)) 
}
grow2(1.7, 20)
#===============================================================================

#animation -> make gif----
#if I draw multiple plots and make them into gif. 
#Make sure the xlim and ylim in the same scale!

grow3 <- function (growth.rate) { 
  num_gen<-10
  generation<-1:num_gen
  N <- rep (0,num_gen)
  N[1] <- 1
  for (i in 2:num_gen) {
    N[i]=growth.rate*N[i-1]
  }
  plot(N~generation, xlim=c(0,10), ylim=c(0,100000), type='b', main=paste("Rate =", growth.rate))
}

saveGIF({
  for (i in 2:10){
    grow3(i)
  }})



#Make more fancier with ggplot2 and gganimate

#===============================================================================
#Practice6.2

grow <- function(growth.rate){
  no.gen <- 50
  N <- rep(0, no.gen) #也可以用N <- vector(mode = 'numeric', 50L)
  N[1] <- 10
  generation <- 1:no.gen
  
  for (i in 2:no.gen) {
  N[i] <- N[i-1]+growth.rate*N[i-1]*(1-N[i-1]/100)
  }
  
  plot(N~generation, 
       xlim=c(0,50), ylim=c(0,120), 
       type='b', main=paste("Rate =", growth.rate))
}

grow(3)
saveGIF({
  for (i in seq (1,4,by=0.2)){
    grow(i)
  }}, interval = 0.1) #control animation speed

# if r < 1 then the increase in population size between t and t+1 will be less than the difference between N and K and the population will adjust monotonically.
# if 1 < r < 2 then the population will have a dampened oscillation.
# When r > 2 but < 2.5 the population may display a stable (regular with same amplitude) limit cycle.
# When r > 2 especially if r > 2.52 oscillation will actually increase and the population growth will become chaotic
# When r >> 2 the population will likely crash, generally in a short time
#===============================================================================



#Practice6.2






