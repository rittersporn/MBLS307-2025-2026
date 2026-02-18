#1
area_circle <- function(diameter) {
  radius <- diameter / 2
  area <- pi * radius^2
  return(area)
}
area_circle(3.4)
diameters <- c(1, 2, 3.4, 5)
area_circle(diameters)
length(area_circle(diameters)) == length(diameters)

#2
f_to_c <- function(oF) {
  oC <- (oF - 32) * 5/9
  
  cat("Fahrenheit:", oF, 
      "is equivalent to", oC, 
      "centigrade.\n")
}
f_to_c(68)
f_to_c(c(32, 68, 100))

#3
x <- rnorm(n = 100, mean = 35, sd = 15)
summarise_and_plot <- function(v) {
  m <- mean(v)
  med <- median(v)
  r <- range(v)
  
  cat("Mean:   ", m, "\n")
  cat("Median: ", med, "\n")
  cat("Range:  ", r[1], "to", r[2], "\n")
  
  
  hist(v,
       probability = TRUE,
       main = "Histogram (proportion) + density curve",
       xlab = "Values")
  
  lines(density(v), lwd = 2)
}
summarise_and_plot(x)

#4
my_median <- function(v) {
  
  v <- sort(v)          # step 1: sort
  n <- length(v)        # step 2: size
  
  if (n %% 2 == 1) {    # odd case
    mid <- (n + 1) / 2
    med <- v[mid]
    
  } else {              # even case
    mid1 <- n / 2
    mid2 <- mid1 + 1
    med <- (v[mid1] + v[mid2]) / 2
  }
  
  return(med)
}
x <- c(5, 2, 9, 1, 7)
my_median(x)
median(x) 
y <- c(4, 1, 7, 2)
my_median(y)
median(y)

#5
ricker <- function(nzero, r, time, K = 100) {
  
  N <- numeric(time)   # empty vector to store population
  N[1] <- nzero        # initial population
  
  for (t in 1:(time - 1)) {
    N[t + 1] <- N[t] * exp(r * (1 - N[t] / K))
  }
  
  return(N)
}
pop <- ricker(nzero = 10, r = 1.2, time = 50)
pop
plot(pop, type = "l",
     xlab = "Time",
     ylab = "Population size",
     main = "Ricker model simulation")
plot(ricker(10, 0.5, 50), type="l")
plot(ricker(10, 2, 50), type="l")
plot(ricker(10, 3, 50), type="l")
ricker <- function(nzero, r, time, K = 100)
  ricker(10, 1.5, 50, K = 200) 
