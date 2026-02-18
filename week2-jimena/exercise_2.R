#2. Fancy calculator: natural log, log to the base 10, log to the base 2, sqrt, natural antilog of 12.43
x <- 12.43
log(x)
log10(x)
log2(x)
sqrt(x)
exp(x)

#3. Area of a circle with a diameter of 20cm and assign the result to an object called area_circle.
diameter <- 20
radius <- diameter / 2
area_circle <- pi* radius^2
area_circle

#4. Calculate the cube root of 14x0.51
cube_root <- (14*0.51)^(1/3)
cube_root

#5. vectors. Create a vector using the concatenate function c() to create a vector called weight containing the weight of 10 childrens
weight <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)
weight

#6. Calculate mean, variance, standard deviation, range of weights and the number of children of your weight vector. Extract the weight of the first five children using Positional indexes and store these weights in a nw variable called first_five.
mean(weight)
var(weight)
range(weight)
length(weight)
first_five <- weight[1:5]
first_five

#7. Create another vector called height containing the height of the same 10 chidren. Use the summary() function to summarise these data in the height object. Extraxt the height of the 2nd, 3rd, 9th and 10t child and assign these heights to a variable called some_child. Extract all the heights of children less than or equal to 99cm and assign to a variable called shorter_child
height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)
height
summary(height)
some_child <- height[c(2, 3, 9, 10)]
some_child
shorter_child <- height[height <=99]
shorter_child

#8. Calculate the body mass index (BMI) for each child. The BMI is calculated as weight divided by the square of the height. Store the results of this calculation in a variable called bmi. 
height_m <-height/100
height_m
bmi <-(weight/(height_m^2))
bmi

#9. Creating sequences. Lets use the seq() function to create a sequence of numbers ranging from 0 to 1 in steps of 0.1, and assign this sequence to a variable called seq1.
seq1 <- seq(from= 0, to= 1, by= 0.1)
seq1

#10. Create a sequence from 10 to 1 in steps of 0.5. Assign this sequence to a variable called seq2 (Hint: include rev() function)
seq2 <- seq(from=10, to=1, by=-0.5)
seq2
seq3 <- rev(seq(from=1, to=10, by=0.5))
seq3

#11. Sequence generation
s1<- rep(1:3, times=3)
s1
s2 <- rep(c("a", "c", "e", "g"), each=3)
s2
s3 <- rep(c("a", "c", "e", "g"), times=3)
s3
s4<- rep(rep(1:3, each = 3), times=2)
s4
s5<- c(rep(1,5), rep(2,4), rep (3,3), rep(4,2), 5)
s5
s6<- c(rep(7,4), rep(2,3), 8, rep(1,5))
s6

#12. Sort the values of height into ascending order and assign the shorter vector to a new variable called height_sorted. Sort all heights into descending order and assign the new vector a name of your choice.
height_sorted <- sort(height)
height_sorted
height_desc<- sort(height, decreasing=TRUE)
height_desc

#13. Lets give children a name. Create a vector called child_name with the following names of 10 childs.
child_name <- c("Alfred", "Barbara", "James", "Jane", "John", "Judy", "Louise", "Mary", "Ronald", "William")
child_name

#14. Order the values of one variable by the order of another variable. Use order() function in combination with the square bracket notation []. Create a new variable called names_sort.Who is the shortest? who is the tallest?
names_sort<- child_name[order(height)]
names_sort
shortest_child<- child_name[which.min(height)]
shortest_child
tallest_child<- child_name[which.max(height)]
tallest_child

#15. Order the name of the children by descending values of weight and assign the result to a variable called weight_rev
weight_rev<- child_name[order(weight, decreasing=TRUE)]
weight_rev
weight_rev2<- child_name[rev(order(weight))]
weight_rev2
heaviest_child<- child_name[which.max(weight)]
heaviest_child
lightest_child<- child_name[which.min(weight)]
lightest_child

#16. Missing values. Create a vector called mydata with values... (the value 7th is missing). Now use the mean() function to calculate the mean of the values in mydata. Next take a look at the help page for the function mean(). Can you figure out how to alter ypur use of the mean() function to calculate the mean without this missing value?
mydata<- c(2, 4, 1, 6, 8, 5, NA, 4, 7)
mydata
mean(mydata)
#na.rm=TRUE #to remove NA before calculating
mean(mydata, na.rm=TRUE)

#17. List variables in your workspace that you have created in this exercise. Remove the variable seq1 from the workspace using the rm() function
ls()
rm(seq1)
ls()
