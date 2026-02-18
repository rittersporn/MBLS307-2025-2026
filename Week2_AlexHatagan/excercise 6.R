#defining a function to calculate the area of a circle
AreaCircle<- function(x){
  return(pi*x**2)
  }
AreaCircle(3.4)
values<-rnorm(10)
AreaCircle(values)

#defining a function to centegrade (oF ->oC)
centegrade<-function(x){
  oC<-(x-32)*5/9
  text<-'Farenheit: value of oF is equivalent to value of oC centigrade'
  print(text)
  return(oC)
}
centegrade(80)

#3.
vec<- rnorm(100,mean = 35,sd=15)
calc<- function(x){
  results<-c(
  mean(x),
  median(x),
  range(x)
  )
  return(results)
}
calc(vec) #ask how can i make the output readable!!!!!!!!!!1

#4.creating a function that calculates the median
mdn<- function(x){
  x_sorted<-sort(x)
  if (length(x)%%2==0){
    mdn<-(x_sorted[length(x_sorted)/2]+x_sorted[length(x_sorted)/2+1])/2
    return(mdn)
  }
  else{
    mdn<-x_sorted[(length(x_sorted)+1)/2]
    return(mdn)
  }
}
mdn(vec)
median(vec)

#5.
ricker <- function(nzero,r,t,K = 100){
  N<-c() # ask how to make empty vector
  N[1]<- nzero
  for (t in 1:t){
    N[t+1]<-N[t]*exp(r*(1-(N[t]/K)))
  }
  p<-plot(N)
  return(p)
}
ricker(10,0.5,100)
