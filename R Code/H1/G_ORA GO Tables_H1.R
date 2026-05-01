GO_df <- data.frame()

if (length(BP_enriched$ID >= 1)){
  thing <- "BP"
  
  BP_table <- BP_enriched %>%
    as_tibble() %>%
    add_column(., Section = "BP", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("GOBP", ""))
  
  GO_df<- rbind(GO_df, BP_table)
  
  BP_table <- BP_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(BP_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  BP_table$`Fold Enrichment` <- BP_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  BP_table$padj <- BP_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  BP_table$`Gene ID` <- gsub("/", ", ", BP_table$`Gene ID`)
  
  BP <- kbl(BP_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(BP_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(BP_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    BP,
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


if (length(MF_enriched$ID >= 1)){
  thing <- "MF"
  
  MF_table <- MF_enriched %>%
    as_tibble() %>%
    add_column(., Section = "MF", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("GOMF", ""))
  
  GO_df<- rbind(GO_df, MF_table)
  
  MF_table <- MF_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(MF_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  MF_table$`Fold Enrichment` <- MF_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  MF_table$padj <- MF_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  MF_table$`Gene ID` <- gsub("/", ", ", MF_table$`Gene ID`)
  
  MF <- kbl(MF_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(MF_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(MF_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    MF,
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

if (length(CC_enriched$ID >= 1)){
  thing <- "CC"
  
  CC_table <- CC_enriched %>%
    as_tibble() %>%
    add_column(., Section = "CC", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("GOCC", ""))
  
  GO_df<- rbind(GO_df, CC_table)
  
  CC_table <- CC_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(CC_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  CC_table$`Fold Enrichment` <- CC_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  CC_table$padj <- CC_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  CC_table$`Gene ID` <- gsub("/", ", ", CC_table$`Gene ID`)
  
  CC <- kbl(CC_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(CC_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(CC_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    CC,
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


if(length(GO_df) > 0){
  write_xlsx(GO_df,  path = paste(want,"vs",contrast,"GO enriched.xlsx", sep = "_"))
}
