library(tidyverse)
install.packages("ggrepel")
library(ggrepel)
data <- read.delim("Sun-etal-2021-IP-MS-PAD4-vs-YFP.txt", stringsAsFactors=FALSE)
names(data)
#VOLCANO PLOT

#"Fasta.headers"            "Protein.IDs"             
#[3] "Majority.protein.IDs"     "Norm_abundance_PAD4_rep1"
#[5] "Norm_abundance_PAD4_rep2" "Norm_abundance_PAD4_rep3"
#[7] "Norm_abundance_PAD4_rep4" "Norm_abundance_YFP_rep1" 
#[9] "Norm_abundance_YFP_rep2"  "Norm_abundance_YFP_rep3" 
#[11] "Norm_abundance_YFP_rep4"  "log2_PAD4_vs_YFP"        
#[13] "pval_PAD4_vs_YFP"    
volcano_df <- data %>% mutate (neglog10p=-log10(pval_PAD4_vs_YFP), sig=case_when(pval_PAD4_vs_YFP <0.05 & log2_PAD4_vs_YFP>1 ~ "Up in PAD4", 
                                                                                 pval_PAD4_vs_YFP <0.05 & log2_PAD4_vs_YFP< -1 ~ "Down in PAD4",
                                                                                 TRUE ~ "NS"))
topproteins <- volcano_df %>%  arrange(pval_PAD4_vs_YFP) %>% slice_head(n=10) #this labels 10 most sig prots
plotvolcano <- ggplot(volcano_df, aes(x=log2_PAD4_vs_YFP,y=neglog10p, color=sig)) + 
  geom_point(alpha=0.7,size=1.5) + 
  scale_color_manual(values = c("Up in PAD4" = "red", "Down in PAD4" = "blue", "NS" = "grey70")) + 
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black") + 
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") + 
  geom_text_repel(data = topproteins, aes(label = Protein.IDs), size = 3, max.overlaps = 20) + 
  labs (x="log2 fold change (PAD4/YFP)", y="-log10(p-value)", title="Volcano plot: PAD4 vs YFP")
plotvolcano
ggsave("volcano_PAD4_vs_YFP.png", plotvolcano, width = 7, height = 5, dpi = 300)

#BOXPLOT

protein <- data %>% filter(Protein.IDs == "AT1G01680.1")
class(protein)
statsprot <- data %>% select(log2_PAD4_vs_YFP) 
protreshape <- protein %>% 
  pivot_longer( cols=matches("PAD4|YFP"), names_to = "sample", values_to = "abundance") %>%  mutate(condition=case_when( str_detect(sample, "PAD4") ~ "PAD4", str_detect(sample, "YFP") ~ "YFP" ) )
stats <- protein %>% select(log2_PAD4_vs_YFP, pval_PAD4_vs_YFP)
label_text <- paste0("log2_PAD4_vs_YFP = ", round(stats$log2_PAD4_vs_YFP, 2), "\np = ", signif(stats$pval_PAD4_vs_YFP,3))
boxplot <- ggplot(protreshape, aes(x=condition, y= abundance, fill=condition)) +
  geom_boxplot(alpha=0.7) +
  geom_jitter(width=0.1) +
  scale_fill_manual(values=c("PAD4" = "red", "YFP" = "blue")) +
  labs(x="",y="Normalized Abundance", title="AT1G01680.1")
boxplot <- boxplot + annotate( "text", x = 1.5, y = max(protreshape$abundance, na.rm = TRUE) * 1.3, label = label_text, size = 4 )
boxplot + ylim(0, 35)

ggsave("AT1G01680_boxplot.png")
