

#import dds object from star alignment and salmon quant
load(path)

#check things about it
head(counts(dds)) # to check the raw counts

colData(dds) # to check the sample info

design(dds) # to check the design formula


#create metadata to define conditions in dds - can base off of colData(dds)
sample <- colData(dds)$sample

#For H1_total - removing the A_, etc.
#sample <- substring(sample,3)

#for all
condition <- substr(sample,1, nchar(sample)-1)

metadata <- DataFrame(
  sample = sample,
  condition = condition,
  replica = c(rep(c(1,2), number))
  #replica = c(rep(c(1), number), rep(c(2), number))
)
metadata

#name row names
rownames(metadata) <- colnames(counts(dds))

#add metadata to dds - need to do this to make new dataframe because the new column names allow for the change in design
colData(dds) <-metadata

#check that samples match
all(colnames(dds$counts) %in% rownames(metadata)) #must be true
all(colnames(dds$counts) == rownames(metadata)) #must be true


#new DESeq object with correct metadata and design
dds_new  <- DESeqDataSet(dds, design = ~ condition)

# dds inspection
dds_new

head(counts(dds_new)) # to check the raw counts

colData(dds_new) # to check the sample info

design(dds_new) # to check the design formula


#### Pre-filtering ####

#DESeq2 vingette recommends smallest group size - DESeq2 uses three for three treated samples

##after reading a lot about this I think the "smallest group size" refers to the number of replicates, for which I have 2

smallestGroupSize <- 2

# Select genes with a sum counts of at least 10 in 2 samples

keep <- rowSums(counts(dds_new) >= 10) >= smallestGroupSize

# Keep only the genes that pass the threshold

dds_filtered <- dds_new[keep,]

#check on it
head(counts(dds_filtered)) # to check the raw counts

colData(dds_filtered) # to check the sample info

design(dds_filtered) # to check the design formula


#set proper contrasts - this sets "NT" as the initial contrast
dds_filtered$condition <- relevel(dds_filtered$condition, ref = contrast)



#### Run the DESeq2 analysis ####
dds_final <- DESeq(dds_filtered)

#using these size factors because it is after all of the filtering and with the proper design, conditions, etc.

#can also use this code to do it step by step
#### Differential expression analysis step-by-step ####

#dds_final <- estimateSizeFactors(dds_filtered)

#dds_final <- estimateDispersions(dds_final)

#dds_final <- nbinomWaldTest(dds_final)

#### Transform normalised counts for data visualisation ####
# A user can choose among vst and rlog. In this tutorial we will work with rlog transformed data. 

#VST is better for larger datasets, rlog is more robust to outliers and extreme values - run blind (not taking conditions into account) because this is for QC analysis and the model uses the raw counts for the analysis anyways

rld <- rlog(dds_final, blind = TRUE)
