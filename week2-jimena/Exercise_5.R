par(mfrow = c(1,1))
#1
prawn <- read.csv("data/prawnGR.csv")
str(prawn)
summary(prawn)
table(prawn$diet)
boxplot(GRate ~ diet, data = prawn,
        xlab = "Diet",
        ylab = "Growth rate",
        main = "Growth rate by diet")

#2
natural <- prawn$GRate[prawn$diet == "Natural"]
artificial <- prawn$GRate[prawn$diet == "Artificial"]
shapiro.test(natural)
shapiro.test(artificial)
var.test(natural, artificial)

#3
t.test(natural, artificial, var.equal = TRUE)

#4
growth.lm <- lm(GRate ~ diet, data = prawn)
summary(growth.lm)

#5
anova(growth.lm)

#6
par(mfrow = c(2,2))
plot(growth.lm)

#7
gigartina <- read.csv("data/Gigartina.csv")
str(gigartina)
table(gigartina$diatom.treat)
boxplot(diameter ~ diatom.treat,
        data = gigartina,
        xlab = "Treatment",
        ylab = "Diameter",
        main = "Diameter by treatment")

#8
#The null hypothesis states that mean diameter does not differ among the four diatom treatments (ASGM, Sdecl, Sexpo and Sstat).

#9
gigartina.lm <- lm(diameter ~ diatom.treat, data = gigartina)
summary(gigartina.lm)

#10
anova(gigartina.lm)

#11
par(mfrow = c(2,2))
plot(gigartina.lm)

#12 and 13
install.packages("mosaic")
library(mosaic)
TukeyHSD(gigartina.lm)

#14
plot(TukeyHSD(gigartina.lm))

#15
temora <- read.csv("data/TemoraBR.csv")
str(temora)

#16
temora$Facclimitisation_temp <- factor(temora$acclimitisation_temp)
str(temora)
coplot(beat_rate ~ temp | Facclimitisation_temp,
       data = temora,
       xlab = "Temperature",
       ylab = "Beat rate",
       main = "Beat rate vs temperature by acclimatisation group")

#17 and 18
temora.lm <- lm(beat_rate ~ temp * Facclimitisation_temp, data = temora)
summary(temora.lm)

#19
anova(temora.lm)

#20
par(mfrow = c(2,2))
plot(temora.lm)
