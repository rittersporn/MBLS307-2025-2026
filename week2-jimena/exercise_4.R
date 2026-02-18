#4
squid <- read.table("data/squid1.txt",
                    header = TRUE,
                    sep = "\t")
str(squid)

squid$year_f <- factor(squid$year)
squid$month_f <- factor(squid$month)
squid$maturity_f <- factor(squid$maturity.stage)
str(squid)

#5
table(squid$year_f, squid$month_f)
table(squid$year_f)
ftable(xtabs(~ year_f + maturity_f + month_f, data = squid))

#6
names(squid)

pdf("output/squid_dotcharts.pdf", width = 10, height = 7)
par(mfrow = c(2, 2))
dotchart(squid$DML, main = "DML", xlab = "DML")
dotchart(squid$weight, main = "Weight", xlab = "Weight")
dotchart(squid$nid.length, main = "Nidamental gland length", xlab = "nid.length")
dotchart(squid$ovary.weight, main = "Ovary weight", xlab = "ovary.weight")
dev.off()

max(squid$nid.length, na.rm = TRUE)
which.max(squid$nid.length)

min(squid$nid.length, na.rm = TRUE)
which.min(squid$nid.length)

dotchart(squid$DML)

#7
which(squid$nid.length > 400)
squid$nid.length[11]
squid$nid.length[11] <- 43.2
squid$nid.length[11]
dotchart(squid$nid.length, main="Corrected nid.length")

#8
par(mfrow = c(2, 2))
hist(squid$DML, main="DML", xlab="DML")

hist(squid$weight, main="Weight", xlab="Weight")

hist(squid$eviscerate.weight, main="Eviscerate weight", xlab="Eviscerate weight")

hist(squid$ovary.weight, main="Ovary weight", xlab="Ovary weight")
pdf("output/histograms.pdf")

par(mfrow = c(2, 2))

hist(squid$DML)
hist(squid$weight)
hist(squid$eviscerate.weight)
hist(squid$ovary.weight)

dev.off()

hist(squid$DML, breaks=5)
hist(squid$DML, breaks=40)
hist(squid$DML, breaks=15)

#9
plot(squid$DML, squid$weight,
     xlab="DML",
     ylab="Weight",
     main="DML vs Weight")
squid$log_weight <- log(squid$weight)
squid$sqrt_weight <- sqrt(squid$weight)
plot(squid$DML, squid$log_weight,
     xlab="DML",
     ylab="log(Weight)",
     main="DML vs log(Weight)")
plot(squid$DML, squid$sqrt_weight,
     xlab="DML",
     ylab="sqrt(Weight)",
     main="DML vs sqrt(Weight)")
pdf("output/scatterplots.pdf")

par(mfrow=c(1,3))

plot(squid$DML, squid$weight)
plot(squid$DML, squid$log_weight)
plot(squid$DML, squid$sqrt_weight)

dev.off()

#10
boxplot(DML ~ maturity_f,
        data = squid,
        xlab = "Maturity stage",
        ylab = "DML",
        main = "DML by maturity stage")
pdf("output/boxplot_DML.pdf")

boxplot(DML ~ maturity_f,
        data = squid,
        xlab = "Maturity stage",
        ylab = "DML")

dev.off()

install.packages("vioplot")
library(vioplot)
vioplot(split(squid$DML, squid$maturity_f),
        xlab = "Maturity stage",
        ylab = "DML",
        main = "Violin plot of DML")

#11
squid$sqrt_weight <- sqrt(squid$weight)
coplot(sqrt_weight ~ DML | maturity_f,
       data = squid,
       xlab = "DML",
       ylab = "sqrt(Weight)",
       main = "Conditional scatterplot")

library(lattice)
xyplot(sqrt_weight ~ DML | maturity_f,
       data = squid)
pdf("output/conditional_scatterplot.pdf")

coplot(sqrt_weight ~ DML | maturity_f,
       data = squid,
       xlab = "DML",
       ylab = "sqrt(Weight)")

dev.off()

#12
vars <- squid[, c("DML",
                  "weight",
                  "eviscerate.weight",
                  "ovary.weight",
                  "nid.length",
                  "nid.weight")]
pairs(vars)
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="gray")
}

panel.cor <- function(x, y, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=2)[1]
  text(0.5, 0.5, txt, cex=1.5)
}

pairs(vars,
      upper.panel = panel.cor,
      lower.panel = panel.smooth,
      diag.panel = panel.hist)
pdf("output/pairs_plot.pdf")

pairs(vars,
      upper.panel = panel.cor,
      lower.panel = panel.smooth,
      diag.panel = panel.hist)

dev.off()

#13
squid$ovary_log <- log(squid$ovary.weight + 1)
cols <- c("blue", "green", "orange", "red", "purple")
point_cols <- cols[squid$maturity_f]
plot(
  squid$DML,
  squid$ovary_log,
  col = point_cols,
  pch = 16,
  xlab = "Dorsal mantle length (mm)",
  ylab = "Log ovary weight",
  main = "Relationship between body size and ovary weight"
)
legend(
  "topleft",
  legend = levels(squid$maturity_f),
  col = cols,
  pch = 16,
  title = "Maturity stage"
)
pdf("output/DML_vs_ovary.pdf", width = 7, height = 5)

plot(
  squid$DML,
  squid$ovary_log,
  col = point_cols,
  pch = 16,
  xlab = "Dorsal mantle length (mm)",
  ylab = "Log ovary weight",
  main = "Relationship between body size and ovary weight"
)

legend(
  "topleft",
  legend = levels(squid$maturity_f),
  col = cols,
  pch = 16,
  title = "Maturity stage"
)
dev.off()

#Exercise
install.packages("ggplot2")
library(ggplot2)
ggplot(mpg, aes(x = class, y = displ)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_classic()
