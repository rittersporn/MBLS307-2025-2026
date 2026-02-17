# ============================================================
# Title: Exercise 6 - Basic programming in R
# Description: Functions, simulation, and basic plotting
# Date: 13-02-2026
# Author: Jens de Vor
# ============================================================


# Q1) Function to calculate area of a circle + test + vector use
area_circle <- function(diameter_cm) {
  radius_cm <- diameter_cm / 2
  area_cm2 <- pi * radius_cm^2
  return(area_cm2)
}

# Test: diameter = 3.4 cm
area_circle(3.4)

# Use on a vector of diameters
diam_vec <- c(1, 2, 3.4, 10)
area_circle(diam_vec)


# Q2) Fahrenheit to Centigrade function with required print format
f_to_c <- function(oF) {
  oC <- (oF - 32) * 5/9
  cat("Farenheit :", oF, "is equivalent to", oC, "centigrade.\n")
  return(oC)
}

# Test
f_to_c(32)
f_to_c(100)


# Q3) Create normal vector + function to summarise and plot histogram (proportion) + density
set.seed(1)
x <- rnorm(n = 100, mean = 35, sd = 15)

summarise_and_plot <- function(v) {
  v_mean <- mean(v)
  v_median <- median(v)
  v_range <- range(v)
  
  cat("Mean:", v_mean, "\n")
  cat("Median:", v_median, "\n")
  cat("Range:", v_range[1], "to", v_range[2], "\n")
  
  # Histogram as proportion: use probability = TRUE
  hist(v,
       probability = TRUE,
       main = "Histogram (proportion) with density curve",
       xlab = "Value")
  lines(density(v), lwd = 2)
  
  invisible(list(mean = v_mean, median = v_median, range = v_range))
}

# Run function
summarise_and_plot(x)


# Q4) Custom median function (handles odd/even lengths)
my_median <- function(v) {
  v <- v[!is.na(v)]          # optional: remove NA
  v <- sort(v)
  n <- length(v)
  
  if (n == 0) return(NA_real_)
  
  if (n %% 2 == 1) {
    # odd length: middle value
    mid <- (n + 1) / 2
    return(v[mid])
  } else {
    # even length: mean of two middle values
    mid1 <- n / 2
    mid2 <- mid1 + 1
    return((v[mid1] + v[mid2]) / 2)
  }
}

# Test on odd sample size
odd_vec <- c(9, 1, 5, 2, 7)
my_median(odd_vec)
median(odd_vec)

# Test on even sample size
even_vec <- c(10, 2, 8, 4)
my_median(even_vec)
median(even_vec)


# Q5) Ricker model simulation function
# Nt+1 = Nt * exp[r * (1 - Nt/K)]
ricker_sim <- function(nzero, r, time, K = 100) {
  N <- numeric(time + 1)   # store N0 .. N_time
  N[1] <- nzero
  
  for (t in 1:time) {
    N[t + 1] <- N[t] * exp(r * (1 - N[t] / K))
  }
  
  return(N)
}

# Example runs
N1 <- ricker_sim(nzero = 10, r = 0.5, time = 50)          # default K=100
N2 <- ricker_sim(nzero = 10, r = 1.2, time = 50, K = 200) # changed K

# Quick plot (optional)
plot(N1, type = "l", xlab = "Time", ylab = "Population size (N)", main = "Ricker model simulation")