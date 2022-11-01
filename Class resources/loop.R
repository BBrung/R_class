library(animation)
library(ggplot2)
library (gganimate)

for(i in 1:100) {
print("Hello world!")
print(i*i)
}

foo <- seq (1,100,by=2) # sequence  1, 3, ..., 99
n<-length(foo) #  size of the foo sequence
foo.squared = NULL #  empty object

for (i in 1:n) { # our counter
  foo.squared[i] = foo[i]^2 # the task
}

foo.df<-data.frame(foo,foo.squared) 
plot (foo.df$foo~foo.df$foo.squared)


foo.squared2<-foo^2
plot (foo~foo.squared2)

system.time(foo.squared2<-foo^2)

num_gen <- 10 #   no. generation
N <- rep (0,num_gen) #  "vector" of 10 zeros (could be `NULL`)
N[1] <- 2 # We need a beginning for our population
for (i in 2:num_gen){ # counter
N[i]=2*N[i-1] # task: double individuals
}
plot (N)

num_gen<-10 
generation<-1:num_gen 
N <- rep (0,num_gen)
N[1] <- 2
for (i in 2:num_gen) { 
  N[i]=2*N[i-1]
   }
plot(N~generation, type='b')

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

par(mfrow=c(2,3))
for (i in 2:7){
  grow(i)
}

grow2 <- function (growth.rate, number.generation) { 
  num_gen<-number.generation
  generation<-1:num_gen
  N <- rep (0,num_gen)
  N[1] <- 1
  for (i in 2:num_gen) {
    N[i]=growth.rate*N[i-1] 
  }
  plot(N~generation,type='b', main=paste("Rate =", growth.rate, ", ", number.generation, "generations"))
  }

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

demo<-NULL
demo$count<-N
demo$generation<-generation
demo<-as.data.frame(demo)

p <- ggplot(demo, aes(x = generation, y=count, size =2)) +
   geom_point(show.legend = FALSE, alpha = 0.7) +
   scale_color_viridis_d() +
   scale_size(range = c(0, 12)) +
   labs(x = "Generation", y = "Individuals")

p + transition_time(generation) +
   labs(title = "Generation: {frame_time}") +
   shadow_wake(wake_length = 0.2, alpha = FALSE)

#Explanation, but no solution. Call me when you are here.

# if r < 1 then the increase in population size between t and t+1 will be less than the difference between N and K and the population will adjust monotonically.

# if 1 < r < 2 then the population will have a dampened oscillation.

# When r > 2 but < 2.5 the population may display a stable (regular with same amplitude) limit cycle.

# When r > 2 especially if r > 2.52 oscillation will actually increase and the population growth will become chaotic

# When r >> 2 the population will likely crash, generally in a short time

# By clicking here, you are ready for the next step below

# WRONG CODE
# ADVICES: START FROM SCRATCH, 
# JUST GET INSPIRATION FROM MY CODE, WORK STEP BY STEP
grow<-function(start_1){
  num_gen<=30
  N1 <- rep(0,10)
  N2 <- rep(0,10)
  generation<-rep(1,num_gen)
  growth.rate<-1.2
  K1<-100
  K2<-120
  a12<-0.8
  a21<-0.8
  N1[1]<-0
  N2[1]<-start_2
  for (i in 2:3)  {
    N1[i] = N1[i-1] + (3.2* N1[i-1] * ((K1-N1[i-1]-(a12*N2[i-1]))/K1))
    N2[i] = N2[i] + (growth.rate * N2[i-1] * ((K2-N2[i-1]-(a21*N1[i-1]))*K2))
    generation[1]=1
    print (N1[i])
  )
  if (N1[1]>2){
    plot(N1~generation,typ="b",ylim=c(0,min(c(K1,K2))),ylab="N")
  }  else {
    plot(N1~generation,typ="n",ylim=c(0,min(c(K1,K2))),ylab="N")
  }
  print(N2[1])
  if (N2[1]>0){
    lines(N2~generation,typ="b",col=2)}
  }

par(mar=c(9,4,1,1),mfrow=c(5,1),las=1)

grow(1,0)
text(4,110,"Species 1 alone")

grow(0,1)
text(4,110,"Species 2 alone")

grow(1,2)
text(6,110,"Both Species competing")
```{.r .distill-force-highlighting-css}
```
