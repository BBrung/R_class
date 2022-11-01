#load library
library(animation)

#put each species alone first
#N1
grow1 <- function(start_1){
  num_gen <- 30
  N1 <- rep(0, num_gen) 
  N1[1] <- start_1
  generation <- 1:num_gen
  growth.rate1 <- 3.2
  K1 <- 100
  for (i in 2:num_gen) {
    N1[i] <- N1[i-1]+(growth.rate1*N1[i-1]*(K1-N1[i-1]/K1)) #competition: -a12*N2[i-1]
  }
  
  plot(N1~generation, 
       type='b', main=paste("N1 Growth Alone"))
}

grow1(1.5)

#N2
grow2 <- function(start_2){
  num_gen <- 30
  N2 <- rep(0, num_gen) 
  N2[1] <- start_2
  generation <- 1:num_gen
  growth.rate2 <- 1.2
  K2 <- 120
  for (i in 2:num_gen) {
    N2[i] <- N2[i-1]+(growth.rate2*N2[i-1]*(K2-N2[i-1]/K2)) #competition: -a12*N2[i-1]
  }
  
  plot(N2~generation, 
       type='b', main=paste("N2 Growth Alone"), col = 'red')
}

grow2(0)


#competition
grow.com <- function(start_1, start_2, a12 = 0.8, a21 = 0.8, num_gen = 30,
                     growth.rate1 = 3.2, growth.rate2 = 1.2, K1 = 100, K2 = 120){
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen) 
  
  N1[1] <- start_1
  N2[1] <- start_2
  
  generation <- 1:num_gen

  for (i in 2:num_gen) {
    N1[i] <- N1[i-1]+(growth.rate1*N1[i-1]*((K1-N1[i-1]-a12*N2[i-1])/K1)) #competition: -a12*N2[i-1]
    N2[i] <- N2[i-1]+(growth.rate2*N2[i-1]*((K2-N2[i-1]-a21*N1[i-1])/K2)) #competition: -a21*N1[i-1]
  }
  
  if (N1[1]>0){  
    plot(N1~generation,
         type='b',
         ylim=c(0,max(c(K1,K2))),
         col = 'blue',
         ylab = 'N', main = paste('start_1 =', start_1, ', start_2 = ', start_2, 
                                  ', growth.rate1 = ', growth.rate1, ', growth.rate2 = ',growth.rate2,
                                 '\na12 = ', a12, ', a21 = ', a21, ', K1 = ', K1, ', K2 = ', K2))
  }else{
    plot(N1~generation,
         type = "n",
         ylim = c(0,max(c(K1,K2))),
         col = 'blue',
         ylab="N", main = paste('start_1 =', start_1, ', start_2 = ', start_2, 
                                ', growth.rate1 = ', growth.rate1, ', growth.rate2 = ',growth.rate2,
                                '\na12 = ', a12, ', a21 = ', a21, ', K1 = ', K1, ', K2 = ', K2))  #沒有大於0就不會畫出來
  }
 
  if (N2[1]>0){
    lines(N2~generation,type = "b",col='red')
  }
}


par(mar=c(4,4,2,1),mfrow=c(3,1),las=1)

grow.com(start_1 = 1, start_2 = 0, 
         a12 = 0.5, a21 = 0.5, growth.rate1 = 1, growth.rate2 = 1)
text(3,110,"Species 1 alone")

grow.com(start_1 = 0, start_2 = 1, 
         a12 = 0.5, a21 = 0.5, growth.rate1 = 1, growth.rate2 = 1)
text(3,110,"Species 2 alone")

grow.com(start_1 = 1, start_2 = 2, 
         a12 = 0.5, a21 = 0.5, growth.rate1 = 1, growth.rate2 = 1)
text(4,110,"Both Species competing")

par(mar=c(4,4,2,1),mfrow=c(3,1),las=1)
#saveGIF
saveGIF({
  for (i in seq (0.5,1.5,by=0.2)){
    grow.com(start_1 = 1, start_2 = 2, a12 = i, a21 = i, growth.rate1 = 1, growth.rate2 = 1)
    text(6,110,"Both Species competing")
  }}, interval = 0.8) #control animation speed

?saveGIF()


<<<<<<< HEAD
#show gif on markdown
=======

>>>>>>> 6e1fe7f51fb0aa8f1cd6bfba580d8ad550a8ab65
#install.packages('gifski')
#![](C:\Users\user\Downloads\R_class.gif)




#沒有條件句
grow.com <- function(start_1, start_2){
  num_gen <- 30
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen) 
  
  N1[1] <- start_1
  N2[1] <- start_2
  
  generation <- 1:num_gen
  growth.rate1 <- 1.2
  growth.rate2 <- 1.2
  
  K1 <- 100
  K2 <- 120
  
  a12 <- 0.8
  a21 <- 0.8
  
  for (i in 2:num_gen) {
    N1[i] <- N1[i-1]+(growth.rate1*N1[i-1]*((K1-N1[i-1]-a12*N2[i-1])/K1)) #competition: -a12*N2[i-1]
    N2[i] <- N2[i-1]+(growth.rate2*N2[i-1]*((K2-N2[i-1]-a21*N1[i-1])/K2)) #competition: -a21*N1[i-1]
  }
  
  plot(N1~generation,
         type='b',
         ylim=c(0,max(c(K1,K2))),
         col = 'blue',
         ylab = 'N')
  lines(N2~generation,type = "b",col='red')
}

par(mar=c(2,3,2,3),mfrow=c(3,1),las=1)

grow.com(1,0)
text(4,110,"Species 1 alone")

grow.com(0,1)
text(4,110,"Species 2 alone")

grow.com(1,2)
text(6,110,"Both Species competing")

