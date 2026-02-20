# Task 1 
# Loading the data
data <- read.table("Sun-etal-2021-IP-MS-PAD4-vs-YFP.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
colnames(data)

# Loading required packages
library(tidyverse)
library(ggrepel)

# Creating the -log10(p-value) column for volcano plot
volcano_data <- data %>%
  mutate(neg_log10_p = -log10(pval_PAD4_vs_YFP))

# Looking for the top 10 proteins to label
top_proteins <- volcano_data %>%
  arrange(pval_PAD4_vs_YFP) %>%
  slice(1:10)

# Creating the volcano plot (I've gone crazy doing this but IT FINALLY WORKED!!!)
ggplot(volcano_data, aes(x = log2_PAD4_vs_YFP, y = neg_log10_p)) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), color = "red", linetype = "dashed") +
  geom_text_repel(data = top_proteins, aes(label = Majority.protein.IDs), size = 3, max.overlaps = 20) +
  labs(title = "Volcano Plot: PAD4 vs YFP", x = "log2 abundance PAD4 vs YFP", y = "-log10(p-value)") +
  theme_minimal() 

# Saving the volcano plot
ggsave("volcano_plot_PAD4_vsYFP.png", width = 6, height = 5, dpi = 300)

#
# Task 2
# Choosing a protein (2 is my lucky number so: AT3G08920.1)
protein_id <- "AT3G08920.1"

# Filtering the data set for my "lucky protein"
protein_data <- data %>%
  filter(Majority.protein.IDs == protein_id)

# Converting to long format from the abundance columns
long_data <- protein_data %>%
  pivot_longer(cols = starts_with("Norm_abundance"), names_to = "Sample", values_to = "Abundance") %>%
  mutate(Sample = ifelse(grepl("PAD4", Sample), "PAD4", "YFP"))

# Extracting the log2 ratio and p-value
log2_value <- protein_data$log2_PAD4_vs_YFP
p_value <- protein_data$pval_PAD4_vs_YFP

# Making the box plot (If I write a word wrong one more time I'll crash out)
ggplot(long_data, aes(x = Sample, y = Abundance, fill = Sample)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 2) +
  labs(title = paste0("Normalized abundance of ", protein_id, ", log2 ratio ", round(log2_value, 2), ", p = ", signif(p_value, 3)), x = "Sample", y = "Normalized abundance") +
  theme_minimal() +
  theme(legend.position = "none")

# Saving the box plot
ggsave("boxplot_AT3G08920.png", width = 6, height = 5, dpi = 300)

# THE END
