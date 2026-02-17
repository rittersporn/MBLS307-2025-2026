print("hello world")
#Excercise 2
#2
log(12.43)
log10(12.43)
log2(12.43)
sqrt(12.43)
exp(12.43)
#3 
r <- 10
area_circle <- pi*r^2
print(area_circle)
#4
(14*0.51)^(1/3)
#5
weight <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)
#6
mean(weight)
var(weight)
sd(weight)
length(weight)
first_five <- weight[1:5]
#7
height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)
summary(height)
some_child <- height[c(2,3,9,10)]
shorter_child <- height[height <= 99]
#8
BMI <- weight/(height)^2
#9
seq1 <- seq(from = 0, to = 1, by = 0.1)
#10
seqx <- seq(from = 1, to = 10, by = 0.5)
seq2 <- rev(seqx)
#11
seq3 <- rep(1:3, times=3)
seq4 <- rep (c("a","c","e","g"), each =3)
seq5 <- rep(c("a","c","e","g"), times=3)
seq6 <- rep(rep (c(1,2,3), each =3), times=2)
seq7 <- rep(1:5, times=c(5,4,3,2,1))
seq8 <- rep(c(7,2,8,1), times=c(4,3,1,5))
#12
height_sorted <- sort(height)
height_decreasing <- rev(height_sorted)
#13
child_names <- (c("Alfred", "Barbara", "James", "Jane", "John", "Judy", "Louise", "Mary", "Ronald", "William"))
#14
names_sort <- child_names [order(height_sorted)]
shortest <- names_sort[1]
tallest <- names_sort[10]
#15
weight_rev <- child_names[order(rev(sort(weight)))]
heaviest <- weight_rev[1]
lightest <- weight_rev[10]
#16
mydata <- c(2, 4, 1, 6, 8, 5, NA, 4, 7)
mean(mydata)
mean(mydata, na.rm=TRUE)
#17
ls()
rm(seq1)
ls()
#Excercise 3!!!
#5
whale <- read.table(file="whaledata.txt", header=TRUE, sep ="\t")
#6
str(whale)
#'data.frame':	100 obs. of  8 variables:
#$ month          : chr  "May" "May" "May" "May" ...
#$ time.at.station: int  1344 1633 743 1050 1764 580 459 561 709 690 ...
#$ water.noise    : chr  "low" "medium" "medium" "medium" ...
#$ number.whales  : int  7 13 12 10 12 10 5 8 11 12 ...
#$ latitude       : chr  "60,37" "60,38" "60,54" "60,29" ...
#$ longitude      : chr  "-4,18" "-4,19" "-4,62" "-4,35" ...
#$ depth          : int  520 559 1006 540 1000 1000 993 988 954 984 ...
#$ gradient       : int  415 405 88 409 97 173 162 162 245 161 ...
#7
summary(whale) #number of missing values: 1 in number whales 
#8
whale.sub <- whale[1:10,1:4]
print(whale.sub)
whale.num <- whale[, c(1,3,4)]
print(whale.num)
whale.may <- whale[1:50, ]
whale.d <- whale[-(1:10), -(ncol(whale))]
print(whale.d)
#9 
deepdepths <- whale[whale$depth > 1200, ]
steepgradient <- whale[whale$gradient > 200,]
lowwaternoise <- whale[whale$water.noise == "low",]
highandmay <- whale[whale$water.noise == "high" & whale$month == "May",]
print(highandmay)
Octlow132 <- whale[whale$water.noise == "low" & whale$month == "October" & whale$gradient > 132,]
latlon <- whale[whale$latitude>=60 & whale$latitude <=61 & whale$longitude <= -6 & whale$longitude<=-4,]
nonmed <- whale[whale$water.noise != "medium",]
#10
Octlow132 <- whale[whale$water.noise == "low" & whale$month == "October" & whale$gradient > median(whale$gradient),]
#11
higherthanavg <- whale[whale$depth > 1500 & whale$number.whales>mean(whale$number.whales), ]
print(higherthanavg)
#number of whales contains NA, therefore hte mean contians NA. Solution:
higherthanavg <- whale[whale$depth > 1500 & whale$number.whales>mean(whale$number.whales, na.rm=TRUE), ]
print(higherthanavg)
#12 
subset(whale, month=="May" & depth > 1000 & time.at.station < 1000,)
subset(whale, month == "October" & latitude > 61, select=c(month, latitude, longitude, number.whales))
#13
whale.depth.sort <- whale[order(whale$depth),]
print(whale.depth.sort)
#14
sort.asc <- whale[order(whale$water.noise, whale$depth),]
print(sort.asc)
sort.desc <- whale[order(whale$water.noise, -whale$depth),]
print(sort.desc)
#15
tapply(whale$number.whales, whale$water.noise, mean, na.rm=TRUE)
tapply(whale$number.whales, list(whale$water.noise, whale$month), median, na.rm=TRUE)
#16
aggregate(cbind(time.at.station, number.whales, depth, gradient) ~ water.noise, data=whale, FUN=mean, na.rm=TRUE)
aggregate(cbind(time.at.station, number.whales, depth, gradient) ~ water.noise + month, data=whale, FUN=mean, na.rm=TRUE)
#17
table(whale$water.noise)
table(whale$water.noise, whale$month)
xtabs(~water.noise+month, data =whale)
#18
write.table(whale.num, file = "whale_num.txt", sep="\t", row.names = FALSE,col.names =TRUE)
