# clear console and source header file
cat('\014')
rm(list=ls())
source('D:/git/header.R')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# data is downloaded from:
# https://www.kaggle.com/fivethirtyeight/uber-pickups-in-new-york-city
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#temp
importdir <- 'D:/Users/NCho/data'

# collect all filename
importfolder <- paste0(importdir,'/kaggle/uber-pickups-in-new-york-city')
flist <- list.files(importfolder,'*.csv')
uber14 <- do.call("rbind", lapply(paste0(importfolder,'/', flist[grep("uber.*14", flist)]), 
                            read_csv,
                            col_types = cols(
                            'Date/Time' = col_datetime(format = '%m/%d/%Y %H:%M:%S'))))

uber_all <- lapply(paste0(importfolder,'/', flist), function(x) cbind(read_csv(x), fname = basename(x)))
