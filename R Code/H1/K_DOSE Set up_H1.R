#DOSE uses entrezIDs so I have to use that here

#could have used bioMart here but this maps multiple entrezIDs to each ensemble which causes problems with my GSEA above, so doing this with bitr here with hopefully better luck

background.DOSE <- bitr(background, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db", drop = TRUE)
#some genes might not be mapped

background.DOSE <- pull(background.DOSE, var = ENTREZID)

res.names.DOSE <- bitr(res.names, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db", drop = TRUE)
#some genes might not be mapped

res.names.DOSE <- pull(res.names.DOSE, var = ENTREZID)
