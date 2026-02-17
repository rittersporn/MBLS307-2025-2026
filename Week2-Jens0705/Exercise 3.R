# ============================================================
# Title: Exercise 3 - Importing and manipulating dataframes
# Description: Completed using whaledata.txt (provided file)
# Date: 11-02-2026
# Author: Jens de Vor
# ============================================================

# Q5) Import
# Use project path if present, otherwise use the absolute path (as in this environment)
file_path <- if (file.exists("whaledata.txt")) "whaledata.txt" else "/mnt/whaledata.txt"

whale <- read.table(
  file = file_path,
  header = TRUE,
  sep = "\t",
  stringsAsFactors = TRUE
)

# Q6) Structure + counts + types
str(whale)
n_obs <- nrow(whale)
n_vars <- ncol(whale)
n_obs; n_vars
# Observations (rows): 100
# Variables (columns): 8

class(whale$month)
class(whale$water.noise)
levels(whale$month)
levels(whale$water.noise)
# month type: Factor with levels May & October
# water.noise type: Factor with levels high, low & medium

# Q7) Summary + missing values
summary(whale)
na_counts <- colSums(is.na(whale))
na_counts
na_counts[na_counts > 0]
# Only number.whales has missing values: 1 NA

# Q8) [ , ] practice
whale.sub <- whale[1:10, 1:4]
whale.num <- whale[, c("month", "water.noise", "number.whales")]
whale.may <- whale[1:50, ]
whale.no.first10_no.lastcol <- whale[-(1:10), -ncol(whale)]

# Q9) Conditional subsetting
whale_depth_gt_1200 <- whale[whale$depth > 1200, ]
whale_gradient_gt_200 <- whale[whale$gradient > 200, ]
whale_noise_low <- whale[whale$water.noise == "low", ]
whale_high_may <- whale[whale$water.noise == "high" & whale$month == "May", ]
whale_oct_low_grad_gt_132 <- whale[whale$month == "October" &
                                     whale$water.noise == "low" &
                                     whale$gradient > 132, ]

whale_lat60_61_lon_6_4 <- whale[whale$latitude >= 60.0 & whale$latitude <= 61.0 &
                                  whale$longitude >= -6.0 & whale$longitude <= -4.0, ]

whale_not_medium <- whale[whale$water.noise != "medium", ]

# Q10) median() inside condition (instead of hard-coding 132)
whale_oct_low_grad_gt_median <- whale[
  whale$month == "October" &
    whale$water.noise == "low" &
    whale$gradient > median(whale$gradient, na.rm = TRUE),
]

# Q11) The NA problem with mean()
mean(whale$number.whales)                 # returns NA because number.whales contains NA
mean(whale$number.whales, na.rm = TRUE)   # works

whale_depth_gt_1500_and_whales_gt_mean_bad <- whale[
  whale$depth > 1500 &
    whale$number.whales > mean(whale$number.whales),
]

whale_depth_gt_1500_and_whales_gt_mean_fix <- whale[
  whale$depth > 1500 &
    !is.na(whale$number.whales) &
    whale$number.whales > mean(whale$number.whales, na.rm = TRUE),
]

# Q12) subset()
whale_may_time_lt_1000_depth_gt_1000 <- subset(
  whale,
  month == "May" & time.at.station < 1000 & depth > 1000
)

whale_oct_lat_gt_61_some_cols <- subset(
  whale,
  subset = (month == "October" & latitude > 61),
  select = c(month, latitude, longitude, number.whales)
)

# Q13) order by ascending depth
whale.depth.sort <- whale[order(whale$depth), ]

# Q14) order by water.noise then depth (asc and desc)
whale_noise_depth_asc <- whale[order(whale$water.noise, whale$depth), ]
whale_noise_depth_desc <- whale[order(whale$water.noise, -whale$depth), ]

# Q15) summaries with tapply
mean_whales_by_noise <- tapply(whale$number.whales, whale$water.noise, mean, na.rm = TRUE)
mean_whales_by_noise

median_whales_by_noise_month <- with(
  whale,
  tapply(number.whales, list(water.noise, month), median, na.rm = TRUE)
)
median_whales_by_noise_month

# Q16) aggregate
agg_mean_by_noise <- aggregate(
  cbind(time.at.station, number.whales, depth, gradient) ~ water.noise,
  data = whale,
  FUN = mean,
  na.rm = TRUE
)
agg_mean_by_noise

agg_mean_by_noise_month <- aggregate(
  cbind(time.at.station, number.whales, depth, gradient) ~ water.noise + month,
  data = whale,
  FUN = mean,
  na.rm = TRUE
)
agg_mean_by_noise_month

# optional 2dp
agg_mean_by_noise_2dp <- agg_mean_by_noise
agg_mean_by_noise_2dp[, -1] <- round(agg_mean_by_noise_2dp[, -1], 2)
agg_mean_by_noise_2dp

agg_mean_by_noise_month_2dp <- agg_mean_by_noise_month
agg_mean_by_noise_month_2dp[, -(1:2)] <- round(agg_mean_by_noise_month_2dp[, -(1:2)], 2)
agg_mean_by_noise_month_2dp

# Q17) table + xtabs
table(whale$water.noise)
table(whale$water.noise, whale$month)

xtabs(~ water.noise, data = whale)
xtabs(~ water.noise + month, data = whale)

# Q18) export whale.num
write.table(
  whale.num,
  file = "whale_num.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)