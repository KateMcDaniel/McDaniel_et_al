GO_gsea <- data.frame()


if (length(BP_gsea$ID >= 1)){
  thing <- "BP"
  
  BP_table <- BP_gsea %>%
    as_tibble() %>%
    add_column(., Section = "BP", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("GOBP", ""))
  
  GO_gsea<- rbind(GO_gsea, BP_table)
  
  BP_table <- BP_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(BP_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
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
    save_kable(paste(want,"vs",contrast,thing,"GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}


if (length(MF_gsea$ID >= 1)){
  thing <- "MF"
  
  MF_table <- MF_gsea %>%
    as_tibble() %>%
    add_column(., Section = "MF", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("GOMF", ""))
  
  GO_gsea<- rbind(GO_gsea, MF_table)
  
  MF_table <- MF_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  "NES",
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(MF_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
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
    save_kable(paste(want,"vs",contrast,thing,"GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}


if (length(CC_gsea$ID >= 1)){
  thing <- "CC"
  
  CC_table <- CC_gsea %>%
    as_tibble() %>%
    add_column(., Section = "CC", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("GOCC", ""))
  
  GO_gsea<- rbind(GO_gsea, CC_table)
  
  CC_table <- CC_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(CC_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
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
    save_kable(paste(want,"vs",contrast,thing,"GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}


if (length(KL_gsea$ID >= 1)){
  thing <- "KL"
  
  KL_table <- KL_gsea %>%
    as_tibble() %>%
    add_column(., Section = "KL", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("KEGG", ""))
  
  GO_gsea<- rbind(GO_gsea, KL_table)
  
  KL_table <- KL_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(KL_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
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
    save_kable(paste(want,"vs",contrast,thing,"GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}



if (length(KM_gsea$ID >= 1)){
  thing <- "KM"
  
  KM_table <- KM_gsea %>%
    as_tibble() %>%
    add_column(., Section = "KM", .after = "Description") %>%
    mutate(`Description` = `Description` %>%
             str_replace_all("_", " ") %>%
             str_replace_all("KEGG MEDICUS", ""))
  
  GO_gsea<- rbind(GO_gsea, KM_table)
  
  KM_table <- KM_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(KM_table) <- c("Description", "NES",  "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
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
    save_kable(paste(want,"vs",contrast,thing,"GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}



if(length(GO_gsea) > 0){
  write_xlsx(GO_gsea,  path = paste(want,"vs",contrast,"GO & KEGG gsea.xlsx", sep = "_"))
}


