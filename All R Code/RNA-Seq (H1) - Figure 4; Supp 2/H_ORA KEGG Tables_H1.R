KEGG_df <- data.frame()

if (length(KL_enriched$ID >= 1)){
  thing <- "KL"
  KL_table <- KL_enriched %>%
    as_tibble() %>%
    add_column(., Section = "KL", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("KEGG", ""))
  
  KEGG_df<- rbind(KEGG_df, KL_table)
  
  KL_table <- KL_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(KL_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  KL_table$`Fold Enrichment` <- KL_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  KL_table$padj <- KL_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  KL_table$`Gene ID` <- gsub("/", ", ", KL_table$`Gene ID`)
  
  KL <- kbl(KL_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(KL_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(KL_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    KL,
    unit = "in",
    guess_size = TRUE,
    alt = ""
  )
  
  
  width<- dim(img)[,1]
  height<- dim(img)[,2]
  
  if (width > 8){
    x = 8/width
    height = x*height
    width = x*width
  }
  
  if (height > 5.5){
    x = 5.5/height
    width = x*width
    height = x*height
  }
  
  left = 5 - (width*.5)
  
  ph_with.external_img(pptx, value = img, location = ph_location_template(left = left,
                                                                          top = 1.5,
                                                                          width = width,
                                                                          height = height))
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "ORA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}



if (length(KM_enriched$ID >= 1)){
  thing <- "KM"
  KM_table <- KM_enriched %>%
    as_tibble() %>%
    add_column(., Section = "KM", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("KEGG MEDICUS", ""))
  
  KEGG_df<- rbind(KEGG_df, KM_table)
  
  KM_table <- KM_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(KM_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  KM_table$`Fold Enrichment` <- KM_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  KM_table$padj <- KM_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  KM_table$`Gene ID` <- gsub("/", ", ", KM_table$`Gene ID`)
  
  KM <- kbl(KM_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(KM_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(KM_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    KM,
    unit = "in",
    guess_size = TRUE,
    alt = ""
  )
  
  
  width<- dim(img)[,1]
  height<- dim(img)[,2]
  
  if (width > 8){
    x = 8/width
    height = x*height
    width = x*width
  }
  
  if (height > 5.5){
    x = 5.5/height
    width = x*width
    height = x*height
  }
  
  left = 5 - (width*.5)
  
  ph_with.external_img(pptx, value = img, location = ph_location_template(left = left,
                                                                          top = 1.5,
                                                                          width = width,
                                                                          height = height))
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "ORA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}


if(length(KEGG_df) > 0){
  write_xlsx(KEGG_df,  path = paste(want,"vs",contrast,"KEGG enriched.xlsx", sep = "_"))
}