cat("\014")
rm(list=ls())

library(quantmod)
library(ggplot2)

getSymbols("DGS10", src = "FRED")
getSymbols("^GSPC")
getSymbols("CPIAUCNS", src = "FRED") #CPI


getSymbols("FEDFUNDS", src = "FRED") #Effective FFR
getSymbols("DFEDTARU", src = "FRED") #FFR target upper
getSymbols("DFEDTARL", src = "FRED") #FFR target lower
getSymbols("DFEDTAR", src = "FRED") #FFR target

plot(FEDFUNDS["2004-01-01/"])
lines(DFEDTARU, col="blue")
lines(DFEDTARL, col="blue")
lines(DFEDTAR, col = "blue")

ggplot() +
  geom_line(data = FEDFUNDS, aes(x=Index, y=DFEDTAR)) +
  xlab("date") +
  ylab("value")


#GDP growth rate
getSymbols("DPCERAM1M225NBEA", src="FRED") #Real Personal Consumption Expenditures % Change
getSymbols("UNRATE", src= "FRED") #Civilian Unemployment Rate

UNRATE$diff <- diff(UNRATE)/lag(UNRATE)


plot.zoo(cbind(DPCERAM1M225NBEA,UNRATE$diff))
plot.zoo(UNRATE,DPCERAM1M225NBEA)


combine <- cbind(DPCERAM1M225NBEA,UNRATE$diff)
cor(combine[,1],combine[,2],use="pairwise")
rollapplyr(combine, 21, function(x) cor(x[,1],x[,2]))


