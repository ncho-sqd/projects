# Clear console and remove every object in the environment
cat("\014")
rm(list = ls())

#Install and load packages
package_list=c("quantmod", "ggplot2")
sapply(package_list,require, character.only=T)

#Download market data
market_tickers=c("^GSPC","^DJI","^KS11")
markets<-getSymbols(market_tickers)

market_returns <- do.call(cbind,lapply(markets,function(x) OpCl(get(x))))
names(market_returns)<-paste(markets,"_RET",sep="")


#Store market data
str(ret)
save(ret,file="D:/datasets/markets.xts")


