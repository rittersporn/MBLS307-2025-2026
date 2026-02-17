# ============================================================
# Title: Proteomics IP-MS (PAD4 vs YFP) - Volcano plot + boxplots
# Description: Read Sun et al. 2021 shortened dataset, compute log2FC + p-values,
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAcCAYAAAAAwr0iAAAEuUlEQVR4XtVWf0yUZRx/j+OEe98DjhKkHxjqMGAwVy0XuTJdpCtzzdZaNFer5VqutbZ+zLvDM/kDllkr0mIlGiDoe/eenCKhZiJgOAqMVQYNtRQ9jt+HooXop+f73B3BgyYqtPXZvnvuufd5vp/v9/P9vs/zStINQrOYFmhWRXVYZZfLomSIzycdRN7qTEWrIwVOqwxIkk5cM2lw2oxvVOfFY6A+A+fqFqAm73Y4Lcrb4rpJQf4KyaBlKeiqzcCFI0tw/odF6Pp2HlzsP9UumcT1Ew5W8+zGojT8dfRZ/PnT07jw41IeREPBLFYKY464fkLhWhU2273WjIFfMnHxt+UY/PU5fxBMCd+hhdj5XiRcFlOquG/C4LQqH7eUz8el31dg6PjLPAiuRNNTTIXFOFqaAs0qbxD3TQgcNuWxyvfjMHRyJS6fWsmDuNj6IgabM5kKy3C+8XHekJU5t9Bb8aS4/6bhtMg72mqWAp43cbntdVz641XsKVyMfYWP+svQ+AQGDi/ECVcqBVAh7r8pOG3yK1WfzuCZE3lQAfYIURGGYQUogLPV6TiwPgasWV8T/dwQ7HYpRLMp6P5+GSelzIM9IAUDCPTAubpH0H/wAbS5k6HZZKjPSHrR33XDYZGz6guSeMNRzYeOvcRHmksUgMnA3wJ+KB16CP1V96Nvbxq++yQGmkW2i/6uC6pdiSuzR8BXv4R3OzXcYPPzfKS5FAiAzgFqQJLft/9e9FamoN2VgLLVCsqsxnjR77jhsBk/aCpO5hmSzFRrajg+sjlbgkhTqL/2tfPQf4Blvy8NPRWz0e2+Cw0boumI/lD0Oy6oq4zp5dlm+Grnc3mpxtRo1O000lwKBMDJq+ai75s5LPtkdO+aia4dd8C7PQa71ijsbAh/WPR/TbBjtbC5JJHLSrWlBqNMg0ZziQJQ9P7MA+Q95bPQVRaPTuc0dGyLxs+fsQBsSqno/1+hWZTMvblmLifVlBqLupuCOVvzIB9pLgUCoHVB8m73dHRqcejYfiu8JRFoLzJiT7aRqaAsF3muCLrX6X4/XnInryU1FHU1Zejbf8+w0Zz+p+e85iQ7ZU7kKpFHMnIZnq8MaNkYOv5vBodVeevgukheQ2qkHua4Z3cieivuRu/XSTxTPrJ5y9aZOFaSwNfRei47z5yRFxP5FHg263Fmk4SqnDBS4R2RbxToPnexA+RkoRmdjhieDTmmzIike2fCsHm06YiN1iPWrMfpbdP4eqq5X/bR5GQn8nUg3+q70VEi7zDoPj+8XoZ3qwne0ijusEOdypzH8uwooE7tNj425U/lPUDWkBfF19M+qjnJPpI8aHXrDGBfU7kiL4eapaS5VxvRVhDKHIRxR95ihWfkD8bsDyhg3lIz7kvUIz1JD0+REiAOg2dLKM4UhIwhJzv1hQ7Ewb6e5oj8dNttbPyIbd6kY9GHcEckY3thOHdONW1nAf1jsv9/9pzLTes3E7FuDPFIO8I42BH9+Shyl82UvHtNOE5/KWxgmZCU3PkWg99I3uBvTqq/asZXMuIotxuh2sJnDAfAvnReqM6dMmbxaGOZFYywa2R6NaMyVK4Nh2o1zh2hgb8E9K7+B9Y36R+v/yv8DUnsdx0sZGQAAAAAAElFTkSuQmCC#              make volcano plot (with labels) and boxplot(s) for selected proteins.
# Date: 13-02-2026
# Author: Jens de Vor
# ============================================================

# Packages
install.packages(c("tidyverse", "ggrepel"))

library(tidyverse)
library(ggrepel)

# ----------------------------
# 1) Read data
# ----------------------------
# Put the file in your project's data/ folder
infile <- "Sun-etal-2021-IP-MS-PAD4-vs-YFP - Copy.txt"

raw <- read.delim(infile, check.names = FALSE, stringsAsFactors = FALSE)

# Take a look
head(raw)
names(raw)

# ----------------------------
# 2) Identify columns: protein id/name + PAD4 replicates + YFP replicates
# ----------------------------
# This tries to find sample columns by matching "PAD4" and "YFP" in column names.
pad4_cols <- names(raw)[str_detect(names(raw), regex("PAD4", ignore_case = TRUE))]
yfp_cols  <- names(raw)[str_detect(names(raw),  regex("YFP",  ignore_case = TRUE))]

pad4_cols
yfp_cols

# Identify a protein name column (common possibilities). Adjust if needed after checking names(raw).
protein_col <- c("Protein", "Protein.IDs", "Protein IDs", "Gene", "Gene.names", "Gene names",
                 "Majority protein IDs", "Leading.razor.protein", "Name")[c(
                   "Protein" %in% names(raw),
                   "Protein.IDs" %in% names(raw),
                   "Protein IDs" %in% names(raw),
                   "Gene" %in% names(raw),
                   "Gene.names" %in% names(raw),
                   "Gene names" %in% names(raw),
                   "Majority protein IDs" %in% names(raw),
                   "Leading.razor.protein" %in% names(raw),
                   "Name" %in% names(raw)
                 )][1]

protein_col

# If protein_col prints NA, set it manually after checking names(raw), e.g.:
# protein_col <- "Gene names"

# ----------------------------
# 3) Compute per-protein stats: mean PAD4, mean YFP, log2FC, p-value (t-test)
# ----------------------------
df <- raw %>%
  mutate(across(all_of(c(pad4_cols, yfp_cols)), as.numeric)) %>%
  mutate(
    mean_PAD4 = rowMeans(across(all_of(pad4_cols)), na.rm = TRUE),
    mean_YFP  = rowMeans(across(all_of(yfp_cols)),  na.rm = TRUE),
    log2FC    = mean_PAD4 - mean_YFP
  )

# Row-wise t-test PAD4 vs YFP for each protein
pvals <- apply(df[, c(pad4_cols, yfp_cols)], 1, function(v) {
  x <- as.numeric(v[seq_along(pad4_cols)])
  y <- as.numeric(v[length(pad4_cols) + seq_along(yfp_cols)])
  if (all(is.na(x)) || all(is.na(y))) return(NA_real_)
  if (sum(!is.na(x)) < 2 || sum(!is.na(y)) < 2) return(NA_real_)
  tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)
})

df$p_value <- pvals
df <- df %>% mutate(neglog10_p = -log10(p_value))

df %>% select(all_of(protein_col), mean_PAD4, mean_YFP, log2FC, p_value, neglog10_p) %>% head()

# ----------------------------
# 4) Volcano plot + labels (top differentially abundant)
# ----------------------------
# Choose proteins to label: e.g., top 10 by abs(log2FC) among those with smallest p-values
label_df <- df %>%
  filter(!is.na(p_value)) %>%
  arrange(p_value) %>%
  slice_head(n = 100) %>%                           # focus on the most significant first
  arrange(desc(abs(log2FC))) %>%
  slice_head(n = 10)

label_df %>% select(all_of(protein_col), log2FC, p_value)

volcano <- ggplot(df, aes(x = log2FC, y = neglog10_p)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(
    data = label_df,
    aes(label = .data[[protein_col]]),
    size = 3,
    max.overlaps = Inf
  ) +
  labs(
    title = "Volcano plot: PAD4 vs YFP (IP-MS)",
    x = "log2(PAD4 / YFP)  (difference of log2 means)",
    y = "-log10(p-value)"
  ) +
  theme_classic()

volcano

# Save volcano plot
ggsave("output/volcano_PAD4_vs_YFP.png", plot = volcano, width = 8, height = 6, dpi = 300)

# ----------------------------
# 5) Boxplot(s) for selected protein(s) + annotate log2FC and p-value
# ----------------------------
# Pick one or more proteins.
# Option A: choose the top labeled proteins automatically:
selected_proteins <- label_df[[protein_col]]

# Option B: manually set:
# selected_proteins <- c("PAD4", "EDS1")  # change to names that exist in your file

selected_proteins

# Make long format for plotting
long <- df %>%
  select(all_of(protein_col), all_of(pad4_cols), all_of(yfp_cols), log2FC, p_value) %>%
  pivot_longer(
    cols = c(all_of(pad4_cols), all_of(yfp_cols)),
    names_to = "sample",
    values_to = "abundance"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(sample, regex("PAD4", ignore_case = TRUE)) ~ "PAD4",
      str_detect(sample, regex("YFP",  ignore_case = TRUE)) ~ "YFP",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(condition))

# Function to make and save a boxplot for ONE protein
plot_one_protein <- function(protein_name) {
  one <- long %>% filter(.data[[protein_col]] == protein_name)
  
  # Grab stats for annotation
  st <- df %>%
    filter(.data[[protein_col]] == protein_name) %>%
    select(log2FC, p_value) %>%
    slice(1)
  
  # If abundance has NA, ggplot will ignore those points automatically
  p <- ggplot(one, aes(x = condition, y = abundance)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.8) +
    labs(
      title = paste0("Protein: ", protein_name),
      x = "Condition",
      y = "Normalized abundance (log2 scale if input is log2)"
    ) +
    theme_classic()
  
  # Add annotation (log2FC + p-value) near top of plot
  y_top <- max(one$abundance, na.rm = TRUE)
  p <- p +
    annotate(
      "text",
      x = 1.5,
      y = y_top,
      vjust = -0.5,
      label = paste0(
        "log2FC (PAD4-YFP) = ", round(st$log2FC, 3),
        "\n",
        "p = ", signif(st$p_value, 3)
      )
    )
  
  # Print + save
  p
}

# Create + save boxplots for selected proteins (separate files)
for (prot in selected_proteins) {
  p <- plot_one_protein(prot)
  print(p)
  out_name <- paste0("output/boxplot_", gsub("[^A-Za-z0-9_]+", "_", prot), "_PAD4_vs_YFP.png")
  ggsave(out_name, plot = p, width = 5.5, height = 4.5, dpi = 300)
}