# The following function creates a character vector containing the full names
# of 16 cancer types with their abbreviations (in parentheses)
# 
# function arguments:
# directory: directory where the .rda file is saved

joint_names <- function(directory){
   
   library(stringi)
   
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
   
   joint_names <- stri_paste(full_names, stri_paste("(", abbreviations, ")"), sep = " ")
   setwd(directory)
   save(joint_names, file="joint_names.rda")
   
}

