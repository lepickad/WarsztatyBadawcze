# The following function creates a data frame containing the abbreviations of 
# 16 cancer types and their full names
# 
# function arguments:
# directory: directory where the .rda file is saved


cancer_names <- function(directory){
   
   abbreviations <- c("BRCA", "COAD", "COADREAD", "GBM", "GBMLGG", "KIPAN", "KIRC", 
                      "KIRP", "LAML", "LUAD", "LUSC", "OV", "READ", "STAD", "STES", 
                      "UCEC")

   full_names <- c("Breast Invasive Carcinoma", "Colon Adenocarcinoma", 
                   "Colon Adenocarcinoma and Rectum Adenocarcinoma combined", 
                   "Glioblastoma Multiforme", 
                   "Glioblastoma Multiforme and Brain Lower Grade Glioma combined", 
                   "Kidney Renal Clear Cell Carcinoma and Kidney Renal Papillary Cell Carcinoma combined", 
                   "Kidney Renal Clear Cell Carcinoma", 
                   "Kidney Renal Papillary Cell Carcinoma", "Acute Myeloid Leukemia", 
                   "Lung Adenocarcinoma", "Lung Squamous Cell Carcinoma", 
                   "Ovarian Serous Cystadenocarcinoma", "Rectum Adenocarcinoma", 
                   "Stomach Adenocarcinoma", 
                   "Stomach Adenocarcinoma and Esophageal Carcinoma combined", 
                   "Uterine Corpus Endometrial Carcinoma")

   cancer_names <- data.frame(abbreviation = abbreviations, full_name = full_names)
   setwd(directory)
   save(cancer_names, file="cancer_names.rda")

}

