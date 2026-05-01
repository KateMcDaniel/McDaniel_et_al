index <- which(match((condition_contrast), want) == 1)
nice_table <- results_shrink_sig[condition_contrast][[index]]

nice_table <- nice_table %>%
  arrange(padj) %>%
  dplyr::select(gene_name, log2FoldChange, padj) %>%
  slice(1:15)

colnames(nice_table) <- c("Gene", "log2FoldChange", "padj")

nice_table$padj <- nice_table$padj %>%
  signif(., digits = 3) %>%
  as.character(.)

table <- kbl(nice_table) %>%
  kable_classic(full_width = F, 
                position = "center", 
                html_font = "Arial") %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  row_spec(0, bold = T, 
           color = "black", 
           font_size = 18,
           align = "c") %>%
  row_spec(1:nrow(nice_table), 
           color = "black",
           font_size = 15, 
           align = "c") %>%
  column_spec(2, background = ifelse(nice_table$log2FoldChange > 0, "#E4FFD6", "#FFD6D6"))%>%
  save_kable(paste(want,"vs",contrast,"top 15.png", sep = "_"))

pptx <- read_pptx()
pptx <- add_slide(pptx, layout = "Title and Content")

img <- external_img(
  table,
  unit = "in",
  guess_size = TRUE,
  alt = ""
)


width<- dim(img)[,1]
height<- dim(img)[,2]

if (width > 8){
  x = 8/width
  height = x*height
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

ph_with.character(pptx, value = paste(contrast, "vs.", want, sep = " "), location = ph_location_type(type = "title"))

print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))

table_no_color <- kbl(nice_table) %>%
  kable_classic(full_width = F, 
                position = "center", 
                html_font = "Arial") %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  row_spec(0, bold = T, 
           color = "black", 
           font_size = 18,
           align = "c") %>%
  row_spec(1:nrow(nice_table), 
           color = "black",
           font_size = 15, 
           align = "c") %>%
  save_kable(paste(want,"vs",contrast,"top 15 no color.png", sep = "_"))


pptx <- read_pptx(path = paste0(celltype, " ", contrast, " ", want,".pptx"))
pptx <- add_slide(pptx, layout = "Title and Content")

img <- external_img(
  table_no_color,
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

ph_with.character(pptx, value = paste(contrast, "vs.", want, sep = " "), location = ph_location_type(type = "title"))

print(pptx, target = paste0(celltype, " ", contrast, " ", want,".pptx"))
