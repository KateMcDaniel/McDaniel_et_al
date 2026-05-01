#organize data for GSEA
arrange(res, desc(abs(res$log2FoldChange)))
#sorting by LFC - use this for GSEA
res.LFC <- res %>%
  dplyr::select(gene_name, starts_with("log2FoldChange")) %>%
  arrange(desc(.[2])) %>%
  deframe #converts to list from the two columns in a key:value type setting

#res.LFC

gsea_list <- list()

BP_gsea <- GSEA(
  geneList = res.LFC,
  TERM2GENE = BP_term2gene,
  TERM2NAME = BP_term2name,
  pvalueCutoff = 0.05,
  eps = 1e-99,
  seed = 511)

gsea_list <- c(gsea_list, BP_gsea)

MF_gsea <- GSEA(
  geneList = res.LFC,
  TERM2GENE = MF_term2gene,
  TERM2NAME = MF_term2name,
  pvalueCutoff = 0.05,
  eps = 1e-99,
  seed = 511)

gsea_list <- c(gsea_list, MF_gsea)

CC_gsea <- GSEA(
  geneList = res.LFC,
  TERM2GENE = CC_term2gene,
  TERM2NAME = CC_term2name,
  pvalueCutoff = 0.05,
  eps = 1e-99,
  seed = 511)

gsea_list <- c(gsea_list, CC_gsea)

KL_gsea <- GSEA(
  geneList = res.LFC,
  TERM2GENE = KL_term2gene,
  TERM2NAME = KL_term2name,
  pvalueCutoff = 0.05,
  eps = 1e-99,
  seed = 511)

gsea_list <- c(gsea_list, KL_gsea)

#does not run for NT vs. NL3 FL
KM_gsea <- GSEA(
  geneList = res.LFC,
  TERM2GENE = KM_term2gene,
  TERM2NAME = KM_term2name,
  pvalueCutoff = 0.05,
  eps = 1e-99,
  seed = 511)

gsea_list <- c(gsea_list, KM_gsea)


names(gsea_list) <- c("BP", "MF", "CC", "KL", "KM")

#for NT vs. Nl3 FL and NL3 FL vs. ICD
#names(gsea_list) <- c("BP", "MF", "CC", "KL")
