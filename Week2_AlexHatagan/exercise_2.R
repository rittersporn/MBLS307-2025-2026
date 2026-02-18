#1.
log (12.43)  #natural log of 12.43
log10 (12.43) # log to the base 10 of 12.43
log2(12.43) #log to the base 2 of 12.43
sqrt(12.43) #square root of 12.43
exp(12.43) # natural antilog of 12.43

#2,3. finding the area of a circle with a diameter of 20cm
diameter <- 20
radius <- diameter/2
radius
area_circle <- pi * radius**2
area_circle

#4.calculating the cube root of 14 x 0.51
cube_root <- (14*0.51)**1/3
cube_root

#5.creating the first vector - weight - and calculating mean,std,range and length
weight <- c(69,57,59,64,56,66,67,66)
weight
mean (weight) #mean
sd (weight) # standard deviation
length (weight) # range

#6.extracting the weight of the first 5 childred
first_five <- weight[0:5]
first_five

#7.creating the height vector
height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)
summary(height)
some_child <- height[c(2,3,9,10)] # extracting the height of the 2nd,3rd,9th and 10th child
some_child

shorter_child <- height[height<=99]
shorter_child

#8.calculating the bmi of all childer
bmi = weight / (height/100)**2
bmi

#9.creating the first sequence
seq1 <- seq(from=0, to=1, by=0.1)
seq1

#10.creating a reversed sequence
seq2<- seq(from=10, to=1, by=-0.5)
seq2

#11.experimenting with seqeunces - rep function
seq3<- rep(c(1:3), times = 3)
seq3
seq4<-rep(c('a','c','e','g'), each =3)
seq4
seq5<- rep(c('a','c','e','g'), times =3)
seq5
seq6<-rep(c(1:3),each=3,times=3)
seq6
seq7<- rep(c(1:5),times=5:1)
seq7
seq8<- rep(c(7,2,8,1), times=c(4,3,1,5) )
seq8

#12.sorting the height
height_sorted<-sort(height)
height_sorted

#13. giving names to chilren
child_name<-c("Alfred", "Barbara", "James", "Jane", "John", "Judy", "Louise", "Mary", "Ronald", "William")
child_name

#14.sorting the children from smallest to tallest
order_height<-order(height)
names_sort=child_name[order_height]
names_sort

#15.sorting the children from the heaviest to the lightes
weight_order_rev<-rev(order(weight))
weight_rev=child_name[weight_order_rev]
wieght_rev

#16.The NA values
mydata <- c(2, 4, 1, 6, 8, 5, NA, 4, 7)
mean(mydata, na.rm=TRUE)

#17.vizualizing everything
objects()
