# Clear console and remove every object in the environment
cat("\014")
rm(list = ls())

#Load header file
source("D:/git/header.R")


getSymbols("EXJPUS",src = "FRED")
getSymbols("DTB1YR", src = "FRED")
jpnrate_url <-"http://www.mof.go.jp/english/jgbs/reference/interest_rate/historical/jgbcme_all.csv"

jpnrate <- 
  read.csv(jpnrate_url, header = T, skip = 1) %>% 
  mutate(Date=as.Date(Date))

jpnrate <- xts(jpnrate[,-1], order.by = as.Date(jpnrate$Date))

merge <- merge(jpnrate, EXJPUS, DTB1YR, all = T)

getSymbols("USD/JPY", from = "1960-01-01", src="oanda")



#Specify ticker groups
market_tickers=c("^GSPC","^DJI")
etf_tickers=c("SCHX","VOO", "SCHB", "SCHZ", "SCHE")
curr_tickers=c("DEXKOUS")


#Download OpCl return data
markets<-getSymbols(market_tickers, from = '1960-01-01')
market_returns <- do.call(cbind,lapply(markets,function(x) OpCl(get(x))))
names(market_returns)<-paste(markets,"_RET",sep="")

etfs<-getSymbols(etf_tickers, from = "2007-09-01", to = "2017-03-18")
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
save(market_returns,file=paste0(dataroot,"/datasets/markets.xts"))

kor<-cbind(KS11,DEXKOUS)
cor(tail(kor,100),use="complete.obs")
