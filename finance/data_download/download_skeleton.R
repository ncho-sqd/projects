# Clear console and remove every object in the environment
cat("\014")
rm(list = ls())

#Install and load packages
package_list=c("quantmod", "ggplot2", "Hmisc", "PerformanceAnalytics")
sapply(package_list,require, character.only=T)


#Specify ticker groups
market_tickers=c("^GSPC","^DJI","^KS11", "^RUT", "^VIX",
                 "^FTSE","^GDAXI", "^FCHI","^N225")
etf_tickers=c("SCHX","VOO", "SCHB", "SCHZ", "SCHE")
curr_tickers=c("DEXKOUS")


#Download OpCl return data
markets<-getSymbols(market_tickers)
market_returns <- do.call(cbind,lapply(markets,function(x) OpCl(get(x))))
names(market_returns)<-paste(markets,"_RET",sep="")

etfs<-getSymbols(etf_tickers)
etf_returns <- do.call(cbind,lapply(etfs,function(x) OpCl(get(x))))
names(etf_returns)<-paste(etfs,"_RET",sep="")

currs<-getSymbols(curr_tickers, src="FRED")
curr_returns <- do.call(cbind,lapply(currs,function(x) OpCl(get(x))))
names(curr_returns)<-paste(currs,"_RET",sep="")


#Visualization
barChart(tail(VOO,300))
addMACD()
addBBands()

plot.zoo(tail(market_returns,300))
plot.zoo(tail(etf_returns,300))

plot.zoo(tail(ret,10))
plot(SCHZ)

#Correlation
cor(tail(etf_returns,300),use="complete.obs")
cor(market_returns,use="complete.obs")
pdf("D:/git/corr2.pdf",paper='a4r')
chart.RollingCorrelation(market_returns[,-1],
                         market_returns[,1],
                         width=24,
                         legend.loc="bottomright",
                         colorset=rich8equal,
                         main= "Rolling Correlation wrt SNP500")
dev.off()

#Store market data
str(ret)
save(ret,file="D:/datasets/markets.xts")

kor<-cbind(KS11,DEXKOUS)
cor(tail(kor,100),use="complete.obs")
