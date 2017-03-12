cat("\014")
rm(list=ls())
source("D:/git/header.R")

#Import data
load(paste0(dataroot,"/datasets/markets.xts"))

#Calculate correlation measures
corr <- function(x1,x2) {
  c(cor = cor.test(x1,x2)$estimate,
    pval = cor.test(x1,x2)$p.value)
}

corr(market_returns[,1],market_returns[,2])
cor_roll <- rollapply(market_returns, 25, cor, by.column=F)

#Visualization
pdf(paste0(exportdir,"/projects/finance/roll_corr_sp_dji.pdf"))
plot(cor_roll[,2],main="Rolling Correlation\n S&P500 vs. DJ Industrial")
dev.off()

install.packages("ggplot2")
require(ggplot2)
ggplot(cor_roll,aes(Date,cor_roll[,2]))
