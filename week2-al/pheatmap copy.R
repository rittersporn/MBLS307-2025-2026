install.packages('pheatmap')
# make a data frame with gene expression values. The gene IDs should be in row names.
#df <- data.frame(row.names = c("geneA", "geneB", "geneC"),
                # sample1 = runif(3, min = -3, max = 3),
                # sample2 = runif(3, min = -3, max = 3),
                # sample3 = runif(3, min = -3, max = 3),
                # sample4 = runif(3, min = -3, max = 3))
# make a simple heatmap
# load library
library(pheatmap)
# make the heatmap
#pheatmap(df)

 
?pheatmap
#data frame with the gene expression of 10 genes across 5 conditions
df<- data.frame(row.names = paste(rep('gene',10),1:10,sep=''),
                condition1 = runif(10, min = -3, max = 3),
                condition2 = runif(10, min = -3, max = 3),
                condition3 = runif(10, min = -3, max = 3),
                condition4 = runif(10, min = -3, max = 3),
                condition5 = runif(10, min = -3, max = 3)
)
pheatmap(df,,cluster_cols = FALSE ,border_color = FALSE)
