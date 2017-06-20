cat("\014")
rm(list=ls())
source("D:/git/header.R")

# import data
load(paste0(dataroot,"/datasets/markets.xts"))

# calculate consecutive increase/decrease days
consec<-rle(sign(as.vector(market_returns[,'GSPC_RET'])))
consec_days <- consec$lengths * consec$values

require(ggplot2)
ggplot()
  +geom_point(aes(x = seq(1,length(consec_days)), y = consec_days))


plot(consec_days[1200:length(consec_days)], type="l")
