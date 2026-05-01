#### Plot PCA ####

##use condition because that is the thing you're most interested in

pca_plot <- plotPCA(rld, intgroup = "condition")
pca_plot
print(pca_plot)
# Save the plot
ggsave(paste(celltype,contrast,"pca_plot.png", sep = "_"), plot = pca_plot, width = 6, height = 5, dpi = 300)

graph2ppt(pca_plot, file=paste(celltype,contrast,".pptx", sep = "_"), width=6, height=5) 



#### Plot sample to sample distance (hierarchical clustering) ####

# Extract the matrix of rlog-transformed counts from the rld object

sampleDists <- dist(t(assay(rld)))  

# Calculate pairwise distances between samples using the dist() function with Euclidean distance as the default method. By transposing the matrix with t(), we ensure that samples become rows and genes become columns, so that the dist function computes pairwise distances between samples.

# Convert distances to a matrix

sampleDistMatrix <- as.matrix(sampleDists)

# Set the row and column names of the distance matrix

rownames(sampleDistMatrix) <- paste(rld$condition, rld$replica, sep = "_")

colnames(sampleDistMatrix) <- paste(rld$condition, rld$replica, sep = "_")

# Define a color palette for the heatmap

colors <- colorRampPalette(rev(brewer.pal(9, "Greens")))(255) # function from RColorBrewer package

# Create the heatmap

clustering_plot <- pheatmap(sampleDistMatrix,
                            clustering_distance_rows = sampleDists,
                            clustering_distance_cols = sampleDists,
                            col = colors,
                            fontsize_col = 8,
                            fontsize_row = 8)

# Save the plot
ggsave(paste(celltype,contrast,"clustering_plot.png", sep = "_"), plot = clustering_plot, width = 6, height = 5, dpi = 300)
graph2ppt(clustering_plot, file=paste(celltype,contrast,".pptx", sep = "_"), width=6, height=5, append = TRUE) 
