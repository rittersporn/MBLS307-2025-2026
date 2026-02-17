# ============================================================
# Title: Exercise 2 - Basic R operations
# Description: Answers for Exercise 2 (R module)
# Date: 11-02-2026
# Author: Jens de Vor
# ============================================================

# Q1) Logs, sqrt, natural antilog of 12.43
x <- 12.43
ln_x    <- log(x)        # natural log
log10_x <- log10(x)      # log base 10
log2_x  <- log(x, base=2)# log base 2
sqrt_x  <- sqrt(x)       # square root
antilog <- exp(x)        # natural antilog (e^x)

ln_x; log10_x; log2_x; sqrt_x; antilog

# (If you want the numeric values explicitly:)
# ln_x    = 2.520112906
# log10_x = 1.094471129
# log2_x  = 3.635754391
# sqrt_x  = 3.525620513
# antilog = 250196.0276


# Q3) Area of a circle with diameter 20 cm -> assign to area_circle
diameter <- 20
radius <- diameter / 2
area_circle <- pi * radius^2
area_circle

# area_circle = 314.1593 (cm^2)


# Q4) Cube root of 14 * 0.51
cube_root_val <- (14 * 0.51)^(1/3)
cube_root_val

# cube_root_val = 1.9256


# Q5) Create vector weight (kg) for 10 children
weight <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)

# Q6) Mean, variance, SD, range, number of children; first five weights
mean_w <- mean(weight)
var_w  <- var(weight)    # sample variance (n-1)
sd_w   <- sd(weight)
range_w <- range(weight)
n_children <- length(weight)

mean_w; var_w; sd_w; range_w; n_children

first_five <- weight[1:5]
first_five

# mean_w    = 62.5
# var_w     = 20.72222
# sd_w      = 4.552167
# range_w   = 56 69
# n_children= 10
# first_five= 69 62 57 59 59


# Q7) Create vector height (cm), summary, positional + logical indexing
height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)

summary(height)

some_child <- height[c(2, 3, 9, 10)]
some_child

shorter_child <- height[height <= 99]
shorter_child

# some_child     = 102 83 133 112
# shorter_child  = 83 84 99 90 77


# Q8) BMI vector (kg / m^2); height must be in meters
bmi <- weight / ( (height / 100)^2 )
bmi


# Q9) Sequence 0 to 1 by 0.1 -> seq1
seq1 <- seq(from = 0, to = 1, by = 0.1)
seq1


# Q10) Sequence 10 to 1 by 0.5 -> seq2
seq2 <- seq(from = 10, to = 1, by = -0.5)
seq2
# (Alternative using rev(): seq2 <- rev(seq(1, 10, by = 0.5)))


# Q11) rep() sequence practice
seq_a <- rep(1:3, times = 3)
seq_a

seq_b <- rep(c("a","c","e","g"), each = 3)
seq_b

seq_c <- rep(c("a","c","e","g"), times = 3)
seq_c

seq_d <- rep(rep(1:3, each = 3), times = 2)
seq_d

seq_e <- rep(1:5, times = c(5,4,3,2,1))
seq_e

seq_f <- rep(c(7,2,8,1), times = c(4,3,1,5))
seq_f


# Q12) Sort heights ascending and descending
height_sorted <- sort(height)
height_sorted

height_sorted_desc <- sort(height, decreasing = TRUE)
height_sorted_desc


# Q13) Child names vector
child_name <- c("Alfred", "Barbara", "James", "Jane", "John",
                "Judy", "Louise", "Mary", "Ronald", "William")


# Q14) Order names by height (shortest to tallest); identify shortest/tallest
names_sort <- child_name[order(height)]
names_sort

shortest_child <- names_sort[1]
tallest_child  <- names_sort[length(names_sort)]
shortest_child; tallest_child

# shortest_child = "Louise"
# tallest_child  = "Ronald"


# Q15) Order names by descending weight; identify heaviest/lightest
weight_rev <- child_name[order(weight, decreasing = TRUE)]
weight_rev

heaviest_child <- weight_rev[1]
lightest_child <- weight_rev[length(weight_rev)]
heaviest_child; lightest_child

# heaviest_child = "Alfred"
# lightest_child = "Louise"


# Q16) Missing values and mean()
mydata <- c(2, 4, 1, 6, 8, 5, NA, 4, 7)

mean(mydata)               # returns NA because of missing value
mean_no_na <- mean(mydata, na.rm = TRUE)
mean_no_na

# mean_no_na = 4.625


# Q17) List variables; remove seq1
ls()
rm(seq1)
ls()