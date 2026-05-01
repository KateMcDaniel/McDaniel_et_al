DO_gsea <- data.frame()

res.names.DOSE

if (length(HDO.GSEA$ID >= 1)){
  thing <- "HDO"
  HDO_table <- HDO.GSEA %>%
    as_tibble() %>%
    add_column(., Section = "HDO", .after = "Description")
  DO_gsea <- rbind(DO_gsea, HDO_table)
  
  
  HDO_table <- HDO_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(HDO_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
  HDO_table$padj <- HDO_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  HDO_table$`Gene ID` <- gsub("/", ", ", HDO_table$`Gene ID`)
  
  list <- unlist(str_split(HDO_table$`Gene ID`, ", "))
  index <- as.numeric(match(list, res.names.DOSE$entrezID))
  gene_name <- paste(as.character(res.names.DOSE[index,1]), collapse = ", ")
  HDO_table$`Gene ID` <- gene_name
  
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
    save_kable(paste(want,"vs",contrast,thing,"DOSE GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}


if (length(HPO.GSEA$ID >= 1)){
  thing <- "HPO"
  HPO_table <- HPO.GSEA %>%
    as_tibble() %>%
    add_column(., Section = "HPO", .after = "Description")
  DO_gsea <- rbind(DO_gsea, HPO_table)
  
  
  HPO_table <- HPO_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(HPO_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
  HPO_table$padj <- HPO_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  HPO_table$`Gene ID` <- gsub("/", ", ", HPO_table$`Gene ID`)
  
  list <- unlist(str_split(HPO_table$`Gene ID`, ", "))
  index <- as.numeric(match(list, res.names.DOSE$entrezID))
  gene_name <- paste(as.character(res.names.DOSE[index,1]), collapse = ", ")
  HPO_table$`Gene ID` <- gene_name
  
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
    save_kable(paste(want,"vs",contrast,thing,"DOSE GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}


if (length(NCG.GSEA$ID >= 1)){
  thing <- "NCG"
  NCG_table <- NCG.GSEA %>%
    as_tibble() %>%
    add_column(., Section = "NCG", .after = "Description")
  DO_gsea <- rbind(DO_gsea, NCG_table)

  NCG_table <- NCG_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(NCG_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
  NCG_table$padj <- NCG_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  NCG_table$`Gene ID` <- gsub("/", ", ", NCG_table$`Gene ID`)
  
  list <- unlist(str_split(NCG_table$`Gene ID`, ", "))
  index <- as.numeric(match(list, res.names.DOSE$entrezID))
  gene_name <- paste(as.character(res.names.DOSE[index,1]), collapse = ", ")
  NCG_table$`Gene ID` <- gene_name


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
    save_kable(paste(want,"vs",contrast,thing,"DOSE GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}


if (length(DGN.GSEA$ID >= 1)){
  thing <- "DGN"
  DGN_table <- DGN.GSEA %>%
    as_tibble() %>%
    add_column(., Section = "DGN", .after = "Description")
  DO_gsea <- rbind(DO_gsea, DGN_table)
  
  
  DGN_table <- DGN_table %>%
    arrange(p.adjust) %>%
    dplyr::select(Description, 
                  NES,
                  p.adjust, 
                  setSize,
                  Section,
                  core_enrichment)
  
  colnames(DGN_table) <- c("Description", "NES", "padj", "Number of Genes Identified", "Pathway", "Gene ID")
  
  
  DGN_table$padj <- DGN_table$padj %>%
    signif(., digits = 3) %>%
    as.character(.)
  
  DGN_table$`Gene ID` <- gsub("/", ", ", DGN_table$`Gene ID`)
  
  list <- unlist(str_split(DGN_table$`Gene ID`, ", "))
  index <- as.numeric(match(list, res.names.DOSE$entrezID))
  gene_name <- paste(as.character(res.names.DOSE[index,1]), collapse = ", ")
  DGN_table$`Gene ID` <- gene_name
  
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
    save_kable(paste(want,"vs",contrast,thing,"DOSE GSEA.png", sep = "_"))
  
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
  ph_with.character(pptx, value = paste(contrast, "vs.", want, thing, "DOSE GSEA", sep = " "), location =
                      ph_location_type(type = "title"))
  
  print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
  
}

if(length(DO_gsea) > 0){
  write_xlsx(DO_gsea,  path = paste(want,"vs",contrast,"DO gsea.xlsx", sep = "_"))
}
