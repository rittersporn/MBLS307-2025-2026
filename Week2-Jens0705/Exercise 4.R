# ============================================================
# Title: Exercise 4 - Visualising data using base R and lattice graphics
# Description: Import squid1.txt, recode factors, create plots, save PDFs
# Date: 11-02-2026
# Author: Jens de Vor
# ============================================================

# Q4) Import squid1.txt
squid <- read.table(
  file = "squid1.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)

# Check structure + summary
str(squid)
summary(squid)

# How many observations + variables?
nrow(squid)
ncol(squid)

# Recode year, month, maturity.stage as factors (create new variables)
squid$year_f <- factor(squid$year)
squid$month_f <- factor(squid$month)                 # if month is numeric, levels will be 1..12
squid$maturity.stage_f <- factor(squid$maturity.stage)

# Check factor coding
str(squid)


# Q5) Observations per month-year combination (use factor versions)
table(squid$year_f, squid$month_f)
xtabs(~ year_f + month_f, data = squid)

# Do you have data for each month in each year?
# (The table above shows zeros where missing.)

# Which years have the most observations?
table(squid$year_f)

# Optional: flattened contingency table of year x maturity.stage x month
ftable(xtabs(~ year_f + maturity.stage_f + month_f, data = squid))


# Q6) Dotcharts for DML, weight, nid.length, ovary.weight (save to PDF)
pdf("ex4_dotcharts_outliers.pdf")

par(mfrow = c(2, 2))  # 2 rows, 2 cols

dotchart(squid$DML, main = "Dotchart: DML", xlab = "DML")
dotchart(squid$weight, main = "Dotchart: weight", xlab = "weight")
dotchart(squid$nid.length, main = "Dotchart: nid.length", xlab = "nid.length")
dotchart(squid$ovary.weight, main = "Dotchart: ovary.weight", xlab = "ovary.weight")

par(mfrow = c(1, 1))
dev.off()


# Q7) Fix nid.length typo (> 400), confirm value, correct to 43.2, redo dotchart
which(squid$nid.length > 400)
squid$nid.length[11]     # should be 430.2 according to exercise text
squid$nid.length[11] <- 43.2

pdf("ex4_nidlength_corrected_dotchart.pdf")
dotchart(squid$nid.length, main = "Dotchart: nid.length (corrected)", xlab = "nid.length")
dev.off()


# Q8) Histograms for DML, weight, eviscerate.weight, ovary.weight + breaks experiment
pdf("ex4_histograms.pdf")

par(mfrow = c(2, 2))

hist(squid$DML, main = "Histogram: DML", xlab = "DML")
hist(squid$weight, main = "Histogram: weight", xlab = "weight")
hist(squid$eviscerate.weight, main = "Histogram: eviscerate.weight", xlab = "eviscerate.weight")
hist(squid$ovary.weight, main = "Histogram: ovary.weight", xlab = "ovary.weight")

par(mfrow = c(1, 1))
dev.off()

# Breaks experiment for DML (save separately)
pdf("ex4_DML_breaks_experiment.pdf")
par(mfrow = c(1, 3))
hist(squid$DML, breaks = 10, main = "DML (breaks=10)", xlab = "DML")
hist(squid$DML, breaks = 20, main = "DML (breaks=20)", xlab = "DML")
hist(squid$DML, breaks = 40, main = "DML (breaks=40)", xlab = "DML")
par(mfrow = c(1, 1))
dev.off()


# Q9) Scatterplots: DML vs weight; transform weight (log or sqrt); save to PDF
# Create transformed variables in the dataframe
squid$weight_log <- log(squid$weight)
squid$weight_sqrt <- sqrt(squid$weight)

pdf("ex4_scatter_DML_weight_transformations.pdf")
par(mfrow = c(1, 3))

plot(squid$DML, squid$weight,
     xlab = "DML", ylab = "weight",
     main = "DML vs weight")

plot(squid$DML, squid$weight_log,
     xlab = "DML", ylab = "log(weight)",
     main = "DML vs log(weight)")

plot(squid$DML, squid$weight_sqrt,
     xlab = "DML", ylab = "sqrt(weight)",
     main = "DML vs sqrt(weight)")

par(mfrow = c(1, 1))
dev.off()


# Q10) Boxplot of DML by maturity stage (factor version) + optional violin plot
pdf("ex4_boxplot_DML_by_maturity.pdf")
boxplot(DML ~ maturity.stage_f, data = squid,
        xlab = "Maturity stage", ylab = "DML",
        main = "DML by maturity stage")
dev.off()


# Q11) Conditional scatterplot by maturity stage (coplot) and lattice alternative
pdf("ex4_coplot_DML_vs_sqrtweight_by_maturity.pdf")
coplot(weight_sqrt ~ DML | maturity.stage_f, data = squid,
       xlab = "DML", ylab = "sqrt(weight)",
       main = "DML vs sqrt(weight) by maturity stage")
dev.off()


# Q12) Pairs plot for multiple continuous variables
vars_for_pairs <- squid[, c("DML", "weight", "eviscerate.weight", "ovary.weight", "nid.length", "nid.weight")]

pdf("ex4_pairs_basic.pdf")
pairs(vars_for_pairs)
dev.off()

# Custom pairs: hist on diagonal, correlation on upper, smoother on lower
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  y <- h$counts / max(h$counts)
  rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], y, ...)
}

panel.cor <- function(x, y, ...) {
  r <- cor(x, y, use = "complete.obs")
  txt <- formatC(r, format = "f", digits = 2)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.5, txt, cex = 1.2)
}

panel.smooth2 <- function(x, y, ...) {
  points(x, y, ...)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) > 3) lines(lowess(x[ok], y[ok]), lwd = 2)
}

pdf("ex4_pairs_custom.pdf")
pairs(vars_for_pairs,
      diag.panel = panel.hist,
      upper.panel = panel.cor,
      lower.panel = panel.smooth2)
dev.off()


# Q13) Custom scatterplot: DML vs ovary.weight, color by maturity stage + legend
# (optional transform if ovary.weight is skewed)
squid$ovary_log <- log(squid$ovary.weight)

cols <- as.integer(squid$maturity.stage_f)

pdf("ex4_custom_DML_vs_ovary_by_maturity.pdf")
plot(squid$DML, squid$ovary_log,
     xlab = "DML",
     ylab = "log(ovary.weight)",
     main = "DML vs log(ovary.weight) by maturity stage",
     pch = 16,
     col = cols)

legend("topleft",
       legend = levels(squid$maturity.stage_f),
       col = seq_along(levels(squid$maturity.stage_f)),
       pch = 16,
       title = "Maturity stage")
dev.off()