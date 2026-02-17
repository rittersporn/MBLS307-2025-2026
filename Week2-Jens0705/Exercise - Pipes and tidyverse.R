# ============================================================
# Title: Exercise - Pipes and tidyverse
# Description: Calculate average engine size using pipes,
#              select() and pull()
# Date: 11-02-2026
# Author: Jens de Vor
# ============================================================

# Load tidyverse (or at least dplyr)
install.packages("tidyverse")
library(tidyverse)

# The original code:
# mean(mpg$displ)

# Rewritten using pipes and tidyverse functions
mpg %>%
  select(displ) %>%   # select engine size column
  pull(displ) %>%     # extract it as a vector
  mean()              # calculate average
