#ontologies: HDO - human diseas ontology, HPO - human phenotype ontology, MPO - mouse phenotype ontology
#this is ORA for disease ontology

HDO <- enrichDO(gene = res.names.DOSE,
                ont = "HDO",
                pvalueCutoff = 0.05,
                pAdjustMethod = "BH",
                universe = names(background.DOSE),
                minGSSize = 5,
                maxGSSize = 500,
                qvalueCutoff = 0.05,
                readable = TRUE)

head(HDO)

enriched_list <- c(enriched_list, HDO)

HPO <- enrichDO(gene = res.names.DOSE,
                ont = "HPO",
                pvalueCutoff = 0.05,
                pAdjustMethod = "BH",
                universe = names(background.DOSE),
                minGSSize = 5,
                maxGSSize = 500,
                qvalueCutoff = 0.05,
                readable = TRUE)

head(HPO) 

enriched_list <- c(enriched_list, HPO)

#tried MPO and I think it doesn't work because entrezIDs are species specific

#network of cancer genes

NCG <- enrichNCG(gene = res.names.DOSE,
                 pvalueCutoff = 0.05,
                 pAdjustMethod = "BH",
                 universe = names(background.DOSE),
                 minGSSize = 5,
                 maxGSSize = 500,
                 qvalueCutoff = 0.05,
                 readable = TRUE)

head(NCG)

enriched_list <- c(enriched_list, NCG)
##dgn is the disease gene network and has gene / snp associations to disease - can't do SNPs because you don't have that data

DGN <- enrichDGN(gene = res.names.DOSE,
                 pvalueCutoff = 0.05,
                 pAdjustMethod = "BH",
                 universe = names(background.DOSE),
                 minGSSize = 5,
                 maxGSSize = 500,
                 qvalueCutoff = 0.05,
                 readable = TRUE)


head(DGN)

enriched_list <- c(enriched_list, DGN)

names(enriched_list) <- c("BP", "MF", "CC", "KL", "KM", "HDO", "HPO", "NCG", "DGN")
