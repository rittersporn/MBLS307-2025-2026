#1-5.importing the data
whale<-read.table(file='/Users/Ake/Desktop/mbls year 2/Omics/Week2-AlexHatagan/whaledata.txt',
           header=TRUE,sep = '\t')

#6.vizualizing the data
str(whale)
#'data.frame':	100 obs. of  8 variables:
    #$ month          : Factor w/ 2 levels "May","October": 1 1 1 1 1 1 1 1 1 1 ...
    #$ time.at.station: int  1344 1633 743 1050 1764 580 459 561 709 690 ...
    #$ water.noise    : Factor w/ 3 levels "high","low","medium": 2 3 3 3 3 1 3 3 3 3 ...
    #$ number.whales  : int  7 13 12 10 12 10 5 8 11 12 ...
    #$ latitude       : num  60.4 60.4 60.5 60.3 60.4 ...
    #$ longitude      : num  -4.18 -4.19 -4.62 -4.35 -5.2 -5.22 -5.08 -5 -4.64 -4.84 ...
    #$ depth          : int  520 559 1006 540 1000 1000 993 988 954 984 ...
    #$ gradient       : int  415 405 88 409 97 173 162 162 245 161 ...
# there are 100 observations, 8 variables and the variables in month and water.noise are characters

#7.
summary(whale)
#number of whales has 1 missing value

#8.
#a.
whale.sub<-whale[1:10,1:4]
whale.sub
#b.
whale.num<-whale[,c(1,3,4)]
whale.num
#c.
whale.may<- whale[whale$month =='May', ]
whale.may
#d.                 
whale.d<- whale[-(0:10),-c(8)]
whale.d

#9,10.
whale.deep<-whale[whale$depth<1200,]
whale.deep
whale.steep<-whale[whale$gradient>200,]
whale.steep
whale.low<-whale[whale$water.noise=='low',]
whale.low
whale.high_in_May<-whale.may[whale.may$water.noise=='high',]
whale.high_in_May
whale.low_noise_October_above_average_gradient<-whale[whale$month=='October'&whale$water.noise=='low'&whale$gradient>median(whale$gradient),]
whale.low_noise_October_above_average_gradient
whale.lat60_61_longmin6_min4<-whale[whale$latitude>60 & whale$latitude<61 & whale$longitude>(-6) & whale$longitude<(-4),]
whale.lat60_61_longmin6_min4

#11.
whale.deeper_more_than_average<-whale[whale$depth>1500 & whale$number.whales>mean(whale$number.whales),]
whale.deeper_more_than_average
mean(whale$number.whales)
whale$number.whales
#with this, all values are NA-not available, this could be due to the fact that there is one value that is not available: NA (the 40th)
correct_mean<-mean(whale$number.whales,na.rm = TRUE)
correct.whale.deeper_more_than_average<-whale[whale$depth>1500 & whale$number.whales>correct_mean,]
correct.whale.deeper_more_than_average

#12.
?subset()
subset(whale,month=='May'& time.at.station<1000 & depth>1000, drop =FALSE)

subset(whale,month =='October' & latitude > 61, select = c('month', 'latitude', 'longitude', 'number.whales'))

#13.
whale.depth.sort <- whale[order(whale$depth),]
whale.depth.sort

#14.
whale.depth.sort.on.waternoise<-whale[order(xtfrm(whale$water.noise),whale$depth),]
whale.depth.sort.on.waternoise

#15.
tapply(whale$number.whales,whale$water.noise, mean,na.rm=TRUE)

#16.
mean.per.waternoise<-aggregate(whale[c('time.at.station','number.whales','depth','gradient')], by=list(water_noise = whale$water.noise), FUN = mean,na.rm=TRUE)
mean.per.waternoise

mean.per.waternoise.and.month<-aggregate(whale[c('time.at.station','number.whales','depth','gradient')], by=list(water_noise = whale$water.noise, month = whale$month), FUN = mean,na.rm=TRUE)
mean.per.waternoise.and.month

#17.
nr.observation.waternoise<-table(whale$water.noise)
nr.observation.waternoise

nr.observations.waternoise.per.month<- table(whale$water.noise,whale$month)
nr.observations.waternoise.per.month

#18. exporting data
write.table(whale.num,file = '/Users/Ake/Desktop/mbls year 2/Omics/Week2-AlexHatagan/whaledata.txt',
            col.names = TRUE, row.names = FALSE, sep = '\t')
