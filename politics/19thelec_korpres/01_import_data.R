cat("\014")
rm(list=ls())
source("D:/git/header.R")

library(readxl)
filename <- "_19.xlsx"
temp<-read_xlsx(paste0(importdir,"/projects/politics/19thelec_korpres/",filename), 
          sheet = 1, 
          col_names = TRUE)
