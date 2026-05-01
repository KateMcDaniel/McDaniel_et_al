#prep for GSEA like before
arrange(res, desc(abs(res$log2FoldChange)))
#sorting by LFC - use this for GSEA
res.LFC <- res %>%
  dplyr::select(gene_name, starts_with("log2FoldChange")) %>%
  arrange(desc(.[2])) 
#don't want to deframe here because I need to continue to manipulate it

#res.LFC


res.names.DOSE <- bitr(res.names, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db", drop = TRUE)
#might lose some genes to mapping

colnames(res.names.DOSE) <- c("gene_name", "entrezID")
res.names.DOSE <- merge(res.LFC, res.names.DOSE, by = "gene_name")

res.DOSE <- res.names.DOSE %>%
  dplyr::select(entrezID, log2FoldChange) %>%
  arrange(desc(.[2])) %>%
  deframe
res.DOSE

HDO.GSEA <- gseDO(res.DOSE,
                  ont = "HDO",
                  organism = "hsa",
                  exponent = 1,
                  minGSSize = 5,
                  maxGSSize = 500,
                  pvalueCutoff = 0.05,
                  pAdjustMethod = "BH",
                  verbose = TRUE, 
                  seed = 511,
                  by = "fgsea")

gsea_list <- c(gsea_list, HDO.GSEA)

HPO.GSEA <- gseDO(res.DOSE,
                  ont = "HPO",
                  organism = "hsa",
                  exponent = 1,
                  minGSSize = 5,
                  maxGSSize = 500,
                  pvalueCutoff = 0.05,
                  pAdjustMethod = "BH",
                  verbose = TRUE, 
                  seed = 511,
                  by = "fgsea")

#neither of these pull anything

gsea_list <- c(gsea_list, HPO.GSEA)

NCG.GSEA <- gseNCG(res.DOSE,
                   organism = "hsa",
                   exponent = 1,
                   minGSSize = 5,
                   maxGSSize = 500,
                   pvalueCutoff = 0.05,
                   pAdjustMethod = "BH",
                   verbose = TRUE, 
                   seed = 511,
                   by = "fgsea")

gsea_list <- c(gsea_list, NCG.GSEA)

DGN.GSEA <- gseDGN(res.DOSE,
                   organism = "hsa",
                   exponent = 1,
                   minGSSize = 5,
                   maxGSSize = 500,
                   pvalueCutoff = 0.05,
                   pAdjustMethod = "BH",
                   verbose = TRUE, 
                   seed = 511,
                   by = "fgsea")

gsea_list <- c(gsea_list, DGN.GSEA)

names(gsea_list) <- c("BP", "MF", "CC", "KL", "KM", "HDO", "HPO", "NCG", "DGN")

#for Nt vs. nl3fl
#names(gsea_list) <- c("BP", "MF", "CC", "KL", "HDO", "HPO", "DGN")

#for NL3FL vs. NICD
#names(gsea_list) <- c("BP", "MF", "CC", "KL", "DGN")


