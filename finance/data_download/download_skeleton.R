#Install and load packages
package_list=c("quantmod")
sapply(package_list,require, character.only=T)

#Download market data
market_tickers=c("^GSPC","^DJI","^KS11")
markets<-getSymbols(market_tickers)
ret<-head(merge(OpCl(GSPC),OpCl(DJI),all=F))

#Store market data
str(ret)
save(ret,file="D:/datasets/markets.xts")
  