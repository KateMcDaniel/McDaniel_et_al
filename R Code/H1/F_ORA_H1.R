##getting the data as I want it for the ORA

index <- which(match((condition_contrast), want) == 1)
results_shrink <- results_shrink[condition_contrast]
res <- results_shrink[[index]]

#filtering to remove 
res <- res[!grepl("novel transcript|novel protein|uncharacterized", res$Description),]
res <- res[!(is.na(res$Description) | res$Description==""), ]
res <- res[!(is.na(res$gene_name) | res$gene_name==""), ]

#before I do any filtering for significance or log fold change, I'm going to set the background list
background <- pull(res, var = gene_name)

#now I'm going to filter for  p< 0.05 and fold change absolute value 1 or greater
res <- as_tibble(subset(res, padj < 0.05 & abs(log2FoldChange) > 1)) 

#now pulling the gene names - use this for searching pathways
res.names <- pull(res, var = gene_name)


enriched_list <- list()

#ORA for GO and KEGG pathways
BP_enriched <- enricher(
  gene = res.names,
  pvalueCutoff = 0.05,
  pAdjustMethod = "bonferroni", 
  universe = background, 
  TERM2GENE = BP_term2gene,
  TERM2NAME = BP_term2name)

length(BP_enriched$ID)
head(BP_enriched)


enriched_list <- c(enriched_list, BP_enriched)




MF_enriched <- enricher(
  gene = res.names,
  pvalueCutoff = 0.05,
  pAdjustMethod = "bonferroni", 
  universe = background, 
  TERM2GENE = MF_term2gene,
  TERM2NAME = MF_term2name)

length(MF_enriched$ID)
head(MF_enriched)

enriched_list <- c(enriched_list, MF_enriched)


CC_enriched <- enricher(
  gene = res.names,
  pvalueCutoff = 0.05,
  pAdjustMethod = "bonferroni", #CHECK THAT THIS IS THE ONE YOU SHOULD USE
  universe = background, 
  TERM2GENE = CC_term2gene,
  TERM2NAME = CC_term2name)

length(CC_enriched$ID)
head(CC_enriched)

enriched_list <- c(enriched_list, CC_enriched)


##Kegg legacy
KL_enriched <- enricher(
  gene = res.names,
  pvalueCutoff = 0.05,
  pAdjustMethod = "bonferroni", 
  universe = background, 
  TERM2GENE = KL_term2gene,
  TERM2NAME = KL_term2name)

length(KL_enriched$ID)
head(KL_enriched)

enriched_list <- c(enriched_list, KL_enriched)

#Kegg medicus
KM_enriched <- enricher(
  gene = res.names,
  pvalueCutoff = 0.05,
  pAdjustMethod = "bonferroni", 
  universe = background, 
  TERM2GENE = KM_term2gene,
  TERM2NAME = KM_term2name)

length(KM_enriched$ID)
head(KM_enriched)

enriched_list <- c(enriched_list, KM_enriched)

names(enriched_list) <- c("BP", "MF", "CC", "KL", "KM")
