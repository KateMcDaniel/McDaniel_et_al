DO_df <- data.frame()

if (length(HDO$ID >= 1)){
  thing <- "HDO" 
  
  HDO_table <- HDO %>%
    as_tibble() %>%
    add_column(., Section = "HDO", .after = "Description")
  DO_df <- rbind(DO_df, HDO_table)

  HDO_table <- HDO_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(HDO_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  HDO_table$`Fold Enrichment` <- HDO_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  HDO_table$padj <- HDO_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  HDO_table$`Gene ID` <- gsub("/", ", ", HDO_table$`Gene ID`)
  
  HDO <- kbl(HDO_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(HDO_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(HDO_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"DOSE ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    HDO,
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
  
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE ORA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))  
  
}

if (length(HPO$ID >= 1)){
  thing <- "HPO"
  HPO_table <- HPO %>%
    as_tibble() %>%
    add_column(., Section = "HPO", .after = "Description")
  DO_df <- rbind(DO_df, HPO_table)
  
  HPO_table <- HPO_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(HPO_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  HPO_table$`Fold Enrichment` <- HPO_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  HPO_table$padj <- HPO_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  HPO_table$`Gene ID` <- gsub("/", ", ", HPO_table$`Gene ID`)
  
  HPO <- kbl(HPO_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(HPO_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(HPO_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"DOSE ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    HPO,
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
  
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE ORA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
}

if (length(NCG$ID >= 1)){
  thing <- "NCG"
  
  NCG_table <- NCG %>%
    as_tibble() %>%
    add_column(., Section = "NCG", .after = "Description")
  DO_df <- rbind(DO_df, NCG_table)
  
  NCG_table <- NCG_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(NCG_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  NCG_table$`Fold Enrichment` <- NCG_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  NCG_table$padj <- NCG_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  NCG_table$`Gene ID` <- gsub("/", ", ", NCG_table$`Gene ID`)
  
  NCG <- kbl(NCG_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(NCG_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(NCG_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"DOSE ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    NCG,
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
  
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE ORA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
}

if (length(DGN$ID >= 1)){
  thing <- "DGN"
  
  DGN_table <- DGN %>%
    as_tibble() %>%
    add_column(., Section = "DGN", .after = "Description")
  DO_df <- rbind(DO_df, DGN_table)
  
  DGN_table <- DGN_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, FoldEnrichment, p.adjust, GeneRatio, BgRatio, Section, geneID)
  
  colnames(DGN_table) <- c("Description", "Fold Enrichment", "padj", "Gene Ratio", "Background Ratio", "Pathway", "Gene ID")
  
  DGN_table$`Fold Enrichment` <- DGN_table$`Fold Enrichment` %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  DGN_table$padj <- DGN_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  DGN_table$`Gene ID` <- gsub("/", ", ", DGN_table$`Gene ID`)
  
  DGN <- kbl(DGN_table) %>%
    kable_classic(full_width = TRUE, 
                  position = "center", 
                  html_font = "Arial") %>%
    kable_styling(bootstrap_options = c("striped", "bordered")) %>%
    row_spec(0, bold = T, 
             color = "black", 
             font_size = 12,
             align = "c") %>%
    row_spec(1:nrow(DGN_table), 
             color = "black",
             font_size = 10, 
             align = "c") %>%
    column_spec(1:ncol(DGN_table), width_min = "1.5cm") %>%
    save_kable(paste(want,"vs",contrast,thing,"DOSE ORA.png", sep = "_"))
  
  pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
  pptx <- add_slide(pptx, layout = "Title and Content")
  
  img <- external_img(
    DGN,
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
  
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE ORA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
}

if(length(DO_df) > 0){
  write_xlsx(DO_df,  path = paste(want,"vs",contrast,"DO enriched.xlsx", sep = "_"))
}
