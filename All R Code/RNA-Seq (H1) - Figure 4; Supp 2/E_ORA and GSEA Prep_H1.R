#visualizing all of the collections that you can pull from
#C5 is the GO pathways and also Human phenotype ontology
collections <- as_tibble(msigdbr_collections())
#print(collections,n=25)

#creating datasets to get each of the comparisons
#GO biological processes (BP), molecular function (MF) and cellular compartment (CC)
GOBP <- msigdbr(collection = "C5", subcollection = "GO:BP", species = "human")
GOMF <- msigdbr(collection = "C5", subcollection = "GO:MF", species = "human")
GOCC <- msigdbr(collection = "C5", subcollection = "GO:CC", species = "human")

#there are two KEGG pathways - legacy is pre 2011 and medicus is since then and also smaller I think? I'm going to try them both
kegg_legacy <- msigdbr(collection = "C2", 
                       subcollection = "CP:KEGG_LEGACY", 
                       species = "human")


#there are two KEGG pathways - legacy is pre 2011 and medicus is since then and also smaller I think? I'm going to try them both
kegg_medicus <- msigdbr(collection = "C2", 
                        subcollection = "CP:KEGG_MEDICUS", 
                        species = "human")



#pulling the things you want 
BP_term2gene <- distinct(GOBP, gs_exact_source, gene_symbol)
BP_term2name <- distinct(GOBP, gs_exact_source, gs_name)

MF_term2gene <- distinct(GOMF, gs_exact_source, gene_symbol)
MF_term2name <- distinct(GOMF, gs_exact_source, gs_name)

CC_term2gene <- distinct(GOCC, gs_exact_source, gene_symbol)
CC_term2name <- distinct(GOCC, gs_exact_source, gs_name)

KL_term2gene <- distinct(kegg_legacy, gs_exact_source, gene_symbol)
KL_term2name <- distinct(kegg_legacy, gs_exact_source, gs_name)

KM_term2gene <- distinct(kegg_medicus, gs_exact_source, gene_symbol)
KM_term2name <- distinct(kegg_medicus, gs_exact_source, gs_name)