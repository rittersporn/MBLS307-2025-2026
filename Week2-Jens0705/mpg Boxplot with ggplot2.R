# ============================================================
# Title: mpg Boxplot with ggplot2
# Description: Engine size per car class with points colored by manufacturer
# Date: 11-02-2026
# Author: Jens de Vor
# ============================================================

# Load required package and dataset
install.packages("ggplot2")
library("ggplot2")
data(mpg)

# Create boxplot with overlaid points
ggplot(mpg, aes(x = class, y = displ)) +
  geom_boxplot(outlier.shape = NA) +                # remove explicit boxplot outliers
  geom_jitter(aes(color = manufacturer),           # add individual points
              width = 0.2,                         # slight horizontal spread
              alpha = 0.7) +                       # slight transparency
  scale_y_continuous(limits = c(0, 10)) +          # y-axis limits
  labs(
    x = "Car class",
    y = "Engine size (litres)",
    title = "Engine size distribution by car class",
    color = "Manufacturer"
  ) +
  theme_classic()
