print("test")
whale<-read.table("whaledata.txt",
                 header=TRUE,
                 sep="\t")
str(whale)

#6. 
#7
summary (whale)
#8
#a.extract first 10 rows abd first 4 columns
whale.sub<- whale[1:10, 1:4]
whale.sub
#b. all rows, but only some columns by name
whale.num<-whale[,c("month", "water.noise", "number.whales")]
whale.num
#c. first 50 rows, all columns
whale.may<- whale[1:50, ]
whale.may
#d. all rows except first 10, all columns except last column
whale.cut<-whale[-(1:10), -ncol(whale)]
whale.cut

#9
whale$latitude_num  <- as.numeric(sub(",", ".", whale$latitude))
whale$longitude_num <- as.numeric(sub(",", ".", whale$longitude))
#depth>1200
whale_depth_gt1200 <- whale[whale$depth > 1200, ]
#gradient
whale_gradient_gt200 <- whale[whale$gradient > 200, ]
#water.noise == low
whale_noise_low <- whale[whale$water.noise == "low", ]
#water.noise== high Y month == May
whale_high_may <- whale[whale$water.noise == "high" & whale$month == "May", ]
#month == October Y noise == low Y gradient >  mediana(gradient)
whale_oct_low_grad_gt_median <- whale[
  whale$month == "October" &
    whale$water.noise == "low" &
    whale$gradient > median(whale$gradient, na.rm = TRUE),
]
#Latitud between 60.0 and 61.0 and longitud between -6.0 and -4.0
whale_area_box <- whale[
  whale$latitude_num >= 60.0 & whale$latitude_num <= 61.0 &
    whale$longitude_num >= -6.0 & whale$longitude_num <= -4.0,
]
#rows without water.noise == medium
whale_not_medium <- whale[whale$water.noise != "medium", ]

nrow(whale_depth_gt1200)
nrow(whale_gradient_gt200)
str(whale)

#10
whale_oct_low_grad <- whale[
  whale$month == "October" &
    whale$water.noise == "low" &
    whale$gradient > median(whale$gradient),
]
whale_oct_low_grad

#11
whale_problem <- whale[
  whale$depth > 1500 &
    whale$number.whales > mean(whale$number.whales),
]

whale_problem
mean(whale$number.whales, na.rm = TRUE)
whale_fixed <- whale[
  whale$depth > 1500 &
    whale$number.whales > mean(whale$number.whales, na.rm = TRUE),
]

whale_fixed

#12
whale_may_subset <- subset(
  whale,
  month == "May" &
    time.at.station < 1000 &
    depth > 1000
)

whale_may_subset
whale_oct_subset <- subset(
  whale,
  month == "October" & latitude_num > 61,
  select = c(month, latitude, longitude, number.whales)
)

whale_oct_subset

#13
order(whale$depth)
whale[order(whale$depth), ]

#14
whale_noise_depth_sort <- whale[
  order(whale$water.noise, whale$depth),
]
whale_noise_depth_desc <- whale[
  order(whale$water.noise, -whale$depth),
]
head(whale_noise_depth_sort)
head(whale_noise_depth_desc)

#15
mean(whale$time.at.station, na.rm = TRUE)
median(whale$depth, na.rm = TRUE)
length(whale$number.whales)

mean_whales_by_noise <- tapply(whale$number.whales, whale$water.noise, mean, na.rm = TRUE)
mean_whales_by_noise
median_whales_by_noise_month <- tapply(
  whale$number.whales,
  list(whale$water.noise, whale$month),
  median,
  na.rm = TRUE
)
median_whales_by_noise_month
table(whale$water.noise, whale$month)

#16
agg_noise <- aggregate(
  cbind(time.at.station, number.whales, depth, gradient) ~ water.noise,
  data = whale,
  FUN = mean,
  na.rm = TRUE
)
agg_noise
agg_noise_month <- aggregate(
  cbind(time.at.station, number.whales, depth, gradient) ~ water.noise + month,
  data = whale,
  FUN = mean,
  na.rm = TRUE
)
agg_noise_month

#17
table(whale$water.noise)
table(whale$water.noise, whale$month)

#18
write.table(
  whale.num,
  file = "output/whale_num.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)
file.exists("output/whale_num.txt")
