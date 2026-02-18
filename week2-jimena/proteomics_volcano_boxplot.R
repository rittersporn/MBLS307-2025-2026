
# Volcano plot + boxplot (Sun et al. 2021 IP-MS PAD4 vs YFP)
library(tidyverse)
library(ggrepel)

infile  <- "Sun-etal-2021-IP-MS-PAD4-vs-YFP.txt"
outdir  <- "figures"

# Volcano thresholds
p_thresh  <- 0.01
fc_thresh <- 1   # |log2FC| >= 1 = 2-fold

# How many proteins to label on volcano
n_labels <- 10

protein_to_plot_manual <- NA_character_



if (!dir.exists(outdir)) dir.create(outdir)

df_raw <- read_tsv(infile, show_col_types = FALSE)


# Helper: pick first matching column name by regex

pick_col <- function(x, patterns) {
  hits <- unlist(lapply(patterns, function(p) grep(p, x, ignore.case = TRUE, value = TRUE)))
  hits <- unique(hits)
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}


# 1) Find replicate columns (PAD4 / YFP)

pad4_cols <- names(df_raw) %>% str_subset(regex("PAD4", ignore_case = TRUE))
yfp_cols  <- names(df_raw) %>% str_subset(regex("YFP",  ignore_case = TRUE))


# 2) Find a protein ID/annotation column to build SHORT labels

name_candidates <- c(
  "Protein", "protein", "Protein.name", "Gene", "Gene.name",
  "Majority.protein.IDs", "Uniprot", "Accession", "Name", "Protein_ID"
)
name_col <- name_candidates[name_candidates %in% names(df_raw)][1]
if (is.na(name_col)) name_col <- names(df_raw)[1]

df_raw <- df_raw %>%
  mutate(
    full_label = as.character(.data[[name_col]]),
    
    # Extract gene symbols if present like: "Symbols: PAD4, ATPAD4"
    symbol = str_match(full_label, "Symbols:\\s*([^|]+)")[,2] %>%
      str_replace_all("\\s+", " ") %>%
      str_trim(),
    
    symbol1 = if_else(!is.na(symbol),
                      str_split(symbol, ",") %>% map_chr(1) %>% str_trim(),
                      NA_character_),
    
    # Fallback: first token before "|" (often ATxGxxxxx.x)
    id1 = str_split(full_label, "\\|") %>% map_chr(1) %>% str_trim(),
    
    short_label = coalesce(symbol1, id1),
    short_label = str_trunc(short_label, 25)
  )

message("Using protein annotation column: ", name_col)


# 3) Try to detect PRECOMPUTED log2FC and p-values in the file

log2fc_col <- pick_col(names(df_raw), c("log2fc", "log2_fc", "log2\\.fc", "log2.*ratio", "log2.*fold", "log2"))
pval_col   <- pick_col(names(df_raw), c("p\\.value", "p_value", "pvalue", "p-val", "^p$", "pval"))
padj_col   <- pick_col(names(df_raw), c("fdr", "qvalue", "q_value", "adj.*p", "p_adj", "padj"))

use_precomputed <- !is.na(log2fc_col) && (!is.na(pval_col) || !is.na(padj_col))


# 4) Build df_stats with log2FC + p-values

if (use_precomputed) {
  
  df_stats <- df_raw %>%
    mutate(
      log2FC = as.numeric(.data[[log2fc_col]]),
      p_value = if (!is.na(padj_col)) as.numeric(.data[[padj_col]]) else as.numeric(.data[[pval_col]])
    ) %>%
    mutate(
      p_value_safe = pmax(p_value, 1e-300),
      neg_log10_p  = -log10(p_value_safe),
      significant  = !is.na(p_value) & p_value < p_thresh & abs(log2FC) >= fc_thresh
    )
  
} else {
  
  if (length(pad4_cols) < 2 || length(yfp_cols) < 2) {
    stop("Could not find enough PAD4/YFP replicate columns to compute stats. Run names(df_raw) to inspect column names.")
  }
  
  message("PAD4 replicate columns: ", paste(pad4_cols, collapse = ", "))
  message("YFP replicate columns:  ", paste(yfp_cols,  collapse = ", "))
  
  df_stats <- df_raw %>%
    mutate(
      PAD4_mean = rowMeans(across(all_of(pad4_cols)), na.rm = TRUE),
      YFP_mean  = rowMeans(across(all_of(yfp_cols)),  na.rm = TRUE),
      log2FC    = PAD4_mean - YFP_mean
    ) %>%
    rowwise() %>%
    mutate(
      p_value = {
        x <- c_across(all_of(pad4_cols))
        y <- c_across(all_of(yfp_cols))
        if (sum(!is.na(x)) >= 2 && sum(!is.na(y)) >= 2) {
          tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)
        } else NA_real_
      }
    ) %>%
    ungroup() %>%
    mutate(
      p_value_safe = pmax(p_value, 1e-300),
      neg_log10_p  = -log10(p_value_safe),
      significant  = !is.na(p_value) & p_value < p_thresh & abs(log2FC) >= fc_thresh
    )
}


# 5) Volcano plot 
df_labels <- df_stats %>%
  filter(significant) %>%
  arrange(p_value) %>%
  slice_head(n = n_labels)

if (nrow(df_labels) < 3) {
  df_labels <- df_stats %>%
    filter(!is.na(p_value)) %>%
    arrange(p_value) %>%
    slice_head(n = n_labels)
}

p_volcano <- ggplot(df_stats, aes(x = log2FC, y = neg_log10_p)) +
  geom_point(aes(color = significant), alpha = 0.75, size = 1.7) +
  geom_vline(xintercept = c(-fc_thresh, fc_thresh), linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = -log10(p_thresh),          linetype = "dashed", linewidth = 0.6) +
  geom_text_repel(
    data = df_labels,
    aes(label = short_label),
    size = 4,
    box.padding = 0.4,
    point.padding = 0.2,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(`FALSE` = "grey40", `TRUE` = "red3")) +
  theme_classic(base_size = 16) +
  labs(
    title = "Volcano plot: PAD4 vs YFP (IP-MS)",
    subtitle = paste0("Thresholds: p < ", p_thresh, " and |log2FC| ≥ ", fc_thresh),
    x = "log2 fold-change (PAD4 − YFP)",
    y = "-log10(p-value)",
    color = "Significant"
  ) +
  theme(legend.position = "right")

ggsave(file.path(outdir, "volcano_PAD4_vs_YFP.png"),
       plot = p_volcano, width = 8, height = 6, dpi = 300)


# 6) Boxplot for one protein 
if (length(pad4_cols) < 2 || length(yfp_cols) < 2) {
  warning("Not enough PAD4/YFP replicate columns detected for boxplot. Skipping boxplot.")
} else {
  
 
  if (!is.na(protein_to_plot_manual)) {
    prot_row <- df_stats %>%
      filter(short_label == protein_to_plot_manual | full_label == protein_to_plot_manual) %>%
      slice(1)
  } else {
    prot_row <- df_stats %>%
      filter(significant) %>%
      arrange(p_value) %>%
      slice(1)
    
    if (nrow(prot_row) == 0) {
      prot_row <- df_stats %>%
        filter(!is.na(p_value)) %>%
        arrange(p_value) %>%
        slice(1)
    }
  }
  
  prot_full  <- prot_row$full_label[1]
  prot_short <- prot_row$short_label[1]
  prot_log2  <- prot_row$log2FC[1]
  prot_p     <- prot_row$p_value[1]
  
  df_long <- df_raw %>%
    select(full_label, short_label, all_of(pad4_cols), all_of(yfp_cols)) %>%
    pivot_longer(
      cols = all_of(c(pad4_cols, yfp_cols)),
      names_to = "sample",
      values_to = "abundance"
    ) %>%
    mutate(
      bait = case_when(
        str_detect(sample, regex("PAD4", ignore_case = TRUE)) ~ "PAD4",
        str_detect(sample, regex("YFP",  ignore_case = TRUE)) ~ "YFP",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(bait)) %>%
    filter(full_label == prot_full)
  
  y_max <- max(df_long$abundance, na.rm = TRUE)
  
  p_box <- ggplot(df_long, aes(x = bait, y = abundance)) +
    geom_boxplot(width = 0.55, outlier.shape = NA, linewidth = 0.9) +
    geom_jitter(width = 0.10, size = 2.3, alpha = 0.85) +
    theme_classic(base_size = 16) +
    labs(
      title = paste0("Protein: ", prot_short),
      subtitle = str_trunc(prot_full, 80),
      x = "",
      y = "Normalized abundance"
    ) +
    annotate(
      "label",
      x = 1.5,
      y = y_max * 1.05,
      label = paste0("log2FC = ", round(prot_log2, 2), "\n", "p = ", signif(prot_p, 3)),
      label.size = 0.25
    ) +
    coord_cartesian(ylim = c(min(df_long$abundance, na.rm = TRUE), y_max * 1.12))
  
  ggsave(file.path(outdir, paste0("boxplot_", prot_short, ".png")),
         plot = p_box, width = 6.5, height = 6, dpi = 300)
  
  print(p_box)
}

print(p_volcano)
print(p_box)

