#### Inspect the normalised counts ####

# Display the first few rows of the normalised counts to inspect the data

head(counts(dds_final, normalized = TRUE))

# Display the first few rows of the raw counts (not normalised) to compare with the normalised counts

head(counts(dds_final))

# Convert the normalised counts from the DESeq2 object to a tibble

normalised_counts <- as_tibble(counts(dds_final, normalized = TRUE))

summary(normalised_counts)
# Add a column for gene names to the normalised counts tibble

normalised_counts$gene <- rownames(counts(dds_final))

# Relocate the gene column to the first position

normalised_counts <- normalised_counts %>%
  relocate(gene, .before = colnames(normalised_counts)[1])

# Save the normalised counts

write.csv(normalised_counts, file = paste(celltype,contrast,"normalised.csv", sep = "_"))


#separating contrasts into tables
#### Extract results table from the dds object ####
condition_contrast <- contrast_list
index <- match(contrast, condition_contrast)
condition_contrast <- condition_contrast[-c(1:index)]


print(paste0(condition_contrast))

results_list <- list()

mart <- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))


for (i in condition_contrast){
  print(i)
  res <- results(dds_final, contrast =c("condition", i, contrast), alpha = 0.05, lfcThreshold = 0.585)
  res$gene <- rownames(res)
  genes <- res$gene
  res <- as_tibble(res) %>%
    relocate(gene, .before = baseMean)
  
  G_list <- getBM(filters = "ensembl_gene_id", 
                  attributes = c("ensembl_gene_id", 'external_gene_name', "description"),
                  values = genes, 
                  mart = mart)
  colnames(G_list) <- c("gene", "gene_name", "Description")
  res<- merge(res, G_list, by = "gene", all.x = TRUE)
  res <- res %>%
    relocate(c(gene_name, Description), .before = baseMean)
  
  results_list <- c(results_list, list(res))
}

names(results_list) <- condition_contrast


write_xlsx(results_list, paste(celltype,contrast,"results.xlsx", sep = "_"))


##not in the DESeq pipeline on nf-core/rnaseq but going to do lfcshrink
res <- results(dds_final, alpha = 0.05, lfcThreshold = 0.585)
condition_coef <- c()
for(i in condition_contrast){
  x <- paste("condition", i, "vs", contrast, sep = "_")
  condition_coef <- c(condition_coef, x)
}

print(paste0(condition_coef))




results_shrink <- list()

for (i in condition_coef){
  print(i)
  res_shrink <- lfcShrink(dds_final, coef = i, type = "ashr")
  res_shrink$gene <- rownames(res_shrink)
  genes <- res_shrink$gene
  res_shrink <- as_tibble(res_shrink) %>%
    relocate(gene, .before = baseMean)
  G_list <- getBM(filters = "ensembl_gene_id", 
                  attributes = c("ensembl_gene_id", 'external_gene_name', "description"),
                  values = genes, 
                  mart = mart)
  colnames(G_list) <- c("gene", "gene_name", "Description")
  res_shrink <- merge(res_shrink, G_list, by = "gene", all.x = TRUE)
  res_shrink <- res_shrink %>%
    relocate(c(gene_name, Description), .before = baseMean)
  results_shrink <- c(results_shrink, list(res_shrink))
}

names(results_shrink) <- c(substr(condition_coef,11, nchar(condition_coef)-(4+nchar(contrast))))

write_xlsx(results_shrink, paste(celltype,contrast,"results_shrink.xlsx", sep = "_"))

#### Extract significant DE genes from the results ####
want <- c(substr(condition_coef,11, nchar(condition_coef)-(4+nchar(contrast))))

results_shrink_sig <- list()

for (i in want){
  print(i)
  index <- which(match(names(results_shrink),i) == 1)
  DESeq <- results_shrink[[index]]
  DESig <- as_tibble(subset(DESeq, padj < 0.05 & abs(log2FoldChange) > 1))
  DESig <- DESig[order(DESig$padj),]
  DESig <- DESig[!grepl("novel transcript|novel protein|uncharacterized", DESig$Description),]
  DESig <- DESig[!(is.na(DESig$Description) | DESig$Description==""), ]
  DESig <- DESig[!(is.na(DESig$gene_name) | DESig$gene_name==""), ]
  
  results_shrink_sig <- c(results_shrink_sig, list(DESig))
}

names(results_shrink_sig) <- want

write_xlsx(results_shrink_sig, paste(celltype,contrast,"DESig_filtered.xlsx", sep = "_"))


