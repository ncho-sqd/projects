#####################
#####DATA IMPORT#####
#####################

#Library call required for data import
library(foreign) 

#Import Price+Demand dta file
mydata=read.dta("C:/Users/Neil/Google Drive/Academics/IO Research/Data/Data/Price+Demand/Data/1999-2012.dta")

#Import Interconnector csv file
mydata2=read.csv("C:/Users/Neil/Google Drive/Academics/IO Research/Data/Data/Interconnector/Interconnectors.csv")

#################
#####PARSING#####
#################

#Parse Interconnector csv file's(mydata2) SETTLEMENTDATE column
date.convert = function (s) {
  s = as.character(s)
  day = substr(s, 1, 2)
  month = substr(s, 3, 5)
  year = substr(s, 6, 9)
  time = substr(s, 11, 18)

  month = switch(month, jan=1, feb=2, mar=3, apr=4, may=5, jun=6, 
                 jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)

  date.str = sprintf("%s-%d-%s %s", year, month, day, time)

  return (as.POSIXct(strptime(date.str, "%Y-%m-%d %H:%M:%S"), 
          origin = "1970-01-01 00:00:00"))
}

new.date = as.POSIXct(sapply(mydata2$SETTLEMENTDATE, date.convert), 
                      origin = "1970-01-01 00:00:00")
mydata2 = data.frame(SETTLEMENTDATE=new.date, mydata2[,-1])

#####################
#####Regressions#####
#####################

##Regression of price to demand and adjacent price
nsw_vic = lm(mydata$nswp ~ mydata$nswd+mydata$vicd)
nsw_qld = lm(mydata$nswp ~ mydata$nswd+mydata$qldd)
vic_tas = lm(mydata$vicp ~ mydata$vicd+mydata$tasd)
vic_sa = lm(mydata$vicp ~ mydata$vicd+mydata$sad)

summary(nsw_vic)
summary(nsw_qld)
summary(vic_tas)
summary(vic_sa)

#Regression with lagged demand & price for nsw_vic only(Simulation for other regions could be replicated by copying the code below)
lagp = 1 #units of days
range = which(!is.na(mydata$nswp) & !is.na(mydata$vicp))
nsw_vic_range1 = min(range):(max(range)-(lagp*48)+1)
nsw_vic_range2 = (min(range)+(lagp*48)-1):max(range)

nsw_vic_l = lm(mydata$nswp[nsw_vic_range2] ~ mydata$nswd[nsw_vic_range1]+mydata$vicp[nsw_vic_range1])
summary(nsw_vic_l)


#average price
install.packages("plyr")
library(plyr)

p = cut(mydata$date[range], 
        breaks = seq(as.POSIXct("1999-01-01"), 
        by = "24 hours", length.out = trunc((max(range)-min(range))/48)))
c(1:10, 10:20)
mean(mydata$nswp[c(1:10, 10:20)])

###

head(mydata$date[range],20)
head(mydata$date[range2],20)
head(mydata$date[range3],100)
head(mydata$date[range4],20)

#1
range = which(!is.na(mydata$nswp) & !is.na(mydata$vicd))
p_avg=NULL
d_avg=NULL
da_avg=NULL

for(i in 20:max(range)) {
if(i < max(range)) {
avgp = mean(mydata$nswp[i:(i+48)])
avgd = mean(mydata$nswd[i:(i+48)])
avgda = mean(mydata$vicd[i:(i+48)])

p_avg=c(p_avg,avgp)
d_avg=c(d_avg,avgd)
da_avg=c(da_avg,avgda)

i = i+48
}
}
nsw_vic = lm(p_avg ~ d_avg + da_avg)

#2
range = which(!is.na(mydata$nswp) & !is.na(mydata$qldd))
p_avg=NULL
d_avg=NULL
da_avg=NULL

for(i in 20:max(range)) {
if(i < max(range)) {
avgp = mean(mydata$nswp[i:(i+48)])
avgd = mean(mydata$nswd[i:(i+48)])
avgda = mean(mydata$qldd[i:(i+48)])

p_avg=c(p_avg,avgp)
d_avg=c(d_avg,avgd)
da_avg=c(da_avg,avgda)

i = i+48
}
}
nsw_qld = lm(p_avg ~ d_avg + da_avg)

#3
range = which(!is.na(mydata$vicp) & !is.na(mydata$tasd))
p_avg=NULL
d_avg=NULL
da_avg=NULL

for(i in 87:max(range)) {
if(i < max(range)) {
avgp = mean(mydata$vicp[i:(i+48)])
avgd = mean(mydata$vicd[i:(i+48)])
avgda = mean(mydata$tasd[i:(i+48)])

p_avg=c(p_avg,avgp)
d_avg=c(d_avg,avgd)
da_avg=c(da_avg,avgda)

i = i+48
}
}
vic_tas = lm(p_avg ~ d_avg + da_avg)

#4
range = which(!is.na(mydata$vicp) & !is.na(mydata$sad))
p_avg=NULL
d_avg=NULL
da_avg=NULL

for(i in 20:max(range)) {
if(i < max(range)) {
avgp = mean(mydata$vicp[i:(i+48)])
avgd = mean(mydata$vicd[i:(i+48)])
avgda = mean(mydata$sad[i:(i+48)])

p_avg=c(p_avg,avgp)
d_avg=c(d_avg,avgd)
da_avg=c(da_avg,avgda)

i = i+48
}
}
vic_sa = lm(p_avg ~ d_avg + da_avg)

summary(nsw_vic)
summary(nsw_qld)
summary(vic_tas)
summary(vic_sa)


#####################
x <- matrix(rnorm(500),100,5)
x[,1] <- abs(x[,1]) * sign(x[,2] * x[,3])
x[,5] <- x[,4]/2 + sqrt(3) * x[,5]/2

library("copula")
d = indepTestSim(100,5)
test = indepTest(x,d)
print(test)
dependogram(test, print = TRUE)

#Range calculation
range_all_num = which(!is.na(mydata$nswp)&
                      !is.na(mydata$qldp)&
	    		    !is.na(mydata$sap)&
			    !is.na(mydata$tasp)&
			    !is.na(mydata$vicp)&
			    !is.na(mydata$snowyp))
min(range_all_num)
max(range_all_num)

range_2007 = which(!is.na(mydata$nswp)&
!is.na(mydata$qldp)&
!is.na(mydata$sap)&
!is.na(mydata$tasp)&
!is.na(mydata$vicp)&
!is.na(mydata$snowyp)&
as.numeric(format(mydata$date,"%Y"))==2007)

length(range_2007)

range_all_num
length(range_all_num)
min(mydata$date[range_all_num])
max(mydata$date[range_all_num])

d=1
test = 1

library("copula")
start = 111710
batch = 1000
x = cbind(mydata$nswp[start:(start+batch-1)], 
          mydata$qldp[start:(start+batch-1)], 
          mydata$sap[start:(start+batch-1)],  
          mydata$tasp[start:start:(start+batch-1)], 
          mydata$vicp[start:start:(start+batch-1)], 
          mydata$snowyp[start:start:(start+batch-1)])
d = indepTestSim(batch,p=6,N=1000)
test = indepTest(x,d)
print(test)
dependogram(test, print = TRUE)

###Available data range detection
range_all_num2 = which(!is.na(mydata$nswp)& #detects row range of dataset where all data for 5 regions are available
			     !is.na(mydata$qldp)&
			     !is.na(mydata$sap)&
			     !is.na(mydata$tasp)&
			     !is.na(mydata$vicp))
start = min(range_all_num2) 
end = max(range_all_num2)
mydata$date[start] #prints start date & time of time interval which all data for 5 regions are available
mydata$date[end] #prints end date & time of time interval which all data for 5 regions are available
(max(range_all_num2)-min(range_all_num2))/(365*48) #how long is that time intervel in unit of year

###Copula Test(price independence test)
library("copula")
setwd("C:/Users/Neil/Google Drive/Academics/IO Research")#SPECIFY
batch = 100 #batch could be in any range btw 1 to 133749 #SPECIFY
copulitr = 1000                                          #SPECIFY
itrno = trunc((end-start+1)/batch)+1 #calculates no. iterations needed to cover the whole data range in units of batch

for(i in 1:itrno){
term = end-((i-1)*batch)

if((init = term-batch+1)<start){ #if last iteration has less than batch amount of data
init = start                     #set the init of the range to start(very beginning of available dataset)
batch = term-init+1
}

x = cbind(mydata$nswp[init:term], 
          mydata$qldp[init:term], 
          mydata$sap[init:term],  
          mydata$tasp[init:term], 
          mydata$vicp[init:term])
d = indepTestSim(batch,p=5,N=copulitr)
test = indepTest(x,d)
save.image(file=paste(batch,",",
                      ncol(x),",",
                      i,".RData")) #automatically saves workspace to above specified working directory
}

print(test)
dependogram(test, print = TRUE)





#Range of mydata
for(i in 1:17){
range = cbind(range,max(mydata[,i])-min(mydata[,i]))
}

#for(i in 1:17){
#nam = toString(colnames(mydata)[i])
#val = range[i,1]
#printf("%s: %d",nam, val)
#}

max(mydata$nswd)-min(mydata$nswd)

###GRAPHS
#graph for interconnector metered flow
S=cbind(mydata2$NSWQLD_METEREDFLOW,mydata2$TASVIC_METEREDFLOW, mydata2$VICSA_METEREDFLOW,mydata2$VICNSW_METEREDFLOW)
par(mfrow=c(2,2))
for(i in 1:4){
plot(mydata2$SETTLEMENTDATE,S[,i],type="b",pch=20,
     xlab="settlementdate", ylab="meteredflow",
     main = colnames(mydata2)[i+1])
}

#graph for interconnector loss
S=cbind(mydata2$NSWQLD_LOSS,mydata2$TASVIC_LOSS, mydata2$VICSA_LOSS,mydata2$VICNSW_LOSS)
par(mfrow=c(2,2))
for(i in 1:4){
plot(mydata2$SETTLEMENTDATE,S[,i],type="b",pch=20,
     xlab="settlementdate", ylab="loss",
     main = colnames(mydata2)[i+5])
}

#graph for loss rate (loss/meteredflow)
par(mfrow=c(2,2))
for(i in 2:5){
lp=mydata2[,i+4]/mydata2[,i]
plot(mydata2$SETTLEMENTDATE,lp, main=mean(lp), pch=20)
}

#vectorize data for dta file
date = mydata[,1]
nswd = mydata[,2]
nswp = mydata[,3]
qldd = mydata[,4]
qldp = mydata[,5]
sad = mydata[,6]
sap = mydata[,7]
tasd = mydata[,8]
tasp = mydata[,9]
vicd = mydata[,10]
vicp = mydata[,11]
snowyd = mydata[,12]
snowyp = mydata[,13]
year = mydata[,14]
month = mydata[,15]
day = mydata[,16]
time = mydata[,17]

#vectorize data for csv file
date2 = mydata2[,1]
NSWQLD = mydata2[,2]
TASVIC = mydata2[,2]
VICSA = mydata2[,2]
VICNSW = mydata2[,2]
NSWQLD_L = mydata2[,2]
TASVIC_L = mydata2[,2]
VICSA_L = mydata2[,2]
VICNSW_L = mydata2[,2]

###Graphs
#Date-quantity graph
par(mfrow=c(3,2))
plot(date,nswd,main=paste("NSW,","mu=",round(mean(nswd)),"sd=",round(sd(nswd)),"cv=",round(sd(nswd)/mean(nswd),2), "\nrange=",round(max(nswd)-min(nswd))))
plot(date,qldd,main=paste("QL,","mu=",round(mean(qldd)),"sd=",round(sd(qldd)),"cv=",round(sd(qldd)/mean(qldd),2),"\nrange=",round(max(qldd)-min(qldd))))
plot(date,sad,main=paste("SA,","mu=",round(mean(sad)),"sd=",round(sd(sad)),"cv=",round(sd(sad)/mean(sad),2),"\nrange=",round(max(sad)-min(sad))))
plot(date,tasd,main=paste("TA,","mu=",round(mean(tasd)),"sd=",round(sd(tasd)),"cv=",round(sd(tasd)/mean(tasd),2),"\nrange=",round(max(tasd)-min(tasd))))
plot(date,vicd,main=paste("VIC,","mu=",round(mean(vicd)),"sd=",round(sd(vicd)),"cv=",round(sd(vicd)/mean(vicd),2),"\nrange=",round(max(vicd)-min(vicd))))
plot(date,snowyd,main=paste("SNOWY,","mu=",round(mean(snowyd)),"sd=",round(sd(snowyd)),"cv=",round(sd(snowyd)/mean(snowyd),2),"\nrange=",round(max(snowyd)-min(snowyd))))

#Date-price graph
par(mfrow=c(3,2))
plot(date,nswp,main=paste("NSW,","mu=",round(mean(nswp)),"sd=",round(sd(nswp)),"cv=",round(sd(nswp)/mean(nswp),2),"\nrange=",round(max(nswp)-min(nswp))))
plot(date,qldp,main=paste("QL,","mu=",round(mean(qldp)),"sd=",round(sd(qldp)),"cv=",round(sd(qldp)/mean(qldp),2),"\nrange=",round(max(qldp)-min(qldp))))
plot(date,sap,main=paste("SA,","mu=",round(mean(sap)),"sd=",round(sd(sap)),"cv=",round(sd(sap)/mean(sap),2),"\nrange=",round(max(sap)-min(sap))))
plot(date,tasp,main=paste("TA,","mu=",round(mean(tasp)),"sd=",round(sd(tasp)),"cv=",round(sd(tasp)/mean(tasp),2),"\nrange=",round(max(tasp)-min(tasp))))
plot(date,vicp,main=paste("VIC,","mu=",round(mean(vicp)),"sd=",round(sd(vicp)),"cv=",round(sd(vicp)/mean(vicp),2),"\nrange=",round(max(vicp)-min(vicp))))
plot(date,snowyp,main=paste("SNOWY,","mu=",round(mean(snowyp)),"sd=",round(sd(snowyp)),"cv=",round(sd(snowyp)/mean(snowyp),2),"\nrange=",round(max(snowyp)-min(snowyp))))

#Price-quantity graph
par(mfrow=c(3,2))
fit=lm(nswp~nswd)
plot(nswd,nswp,main=paste("NSW,","coef=",round(summary(fit)$coefficients[2,1],2),"r2=",round(summary(fit)$r.squared),2))
abline(lm(nswp~nswd), col="red")

fit=lm(qldp~qldd)
plot(qldd,qldp,main=paste("QLD,","coef=",round(summary(fit)$coefficients[2,1],2),"r2=",round(summary(fit)$r.squared),2))
abline(fit, col="red")

fit=lm(sap~sad)
plot(sad,sap,main=paste("SA,","coef=",round(summary(fit)$coefficients[2,1],2),"r2=",round(summary(fit)$r.squared),2))
abline(fit, col="red")

fit=lm(tasp~tasd)
plot(tasd,tasp,main=paste("TAS,","coef=",round(summary(fit)$coefficients[2,1],2),"r2=",round(summary(fit)$r.squared),2))
abline(fit, col="red")

fit=lm(vicp~vicd)
plot(vicd,vicp,main=paste("VIC,","coef=",round(summary(fit)$coefficients[2,1],2),"r2=",round(summary(fit)$r.squared),2))
abline(fit, col="red")

fit=lm(snowyp~snowyd)
plot(snowyd,snowyp,main=paste("SNOWY,","coef=",round(summary(fit)$coefficients[2,1],2),"r2=",round(summary(fit)$r.squared),2))
abline(fit, col="red")

###Price correlation table
A=data.frame(nswp,qldp,sap,tasp,vicp,snowyp)
cor(A)
plot(A)

B=data.frame(nswd,qldd,sad,tasd,vicd,snowyd)
cor(B)
plot(B)

###Price Independece Test(Copula Test)
install.packages("copula")
library("copula")
set.seed(123)
memory.limit(size=1000000000000000000)
test = indepTestSim(nrow(mydata), p=2, N=100)

###Shorter-term graphs
startDate = "2012-01-01" 
endDate = "2013-01-01"
par(mfrow=c(3,2))
j = 1
min = 0
max = 0

for(j in 1:nrow(mydata)){
if (startDate<=mydata[j,1] & endDate>=mydata[j,1] & min==0){
min = mydata[j,1]}
if (max==0){
max = mydata[j,1]}
}

###Shorter-term graphs (shortcut)
row_year = 227902
par(mfrow=c(3,2))
for(i in 1:6){
plot(mydata[c(row_year:max(nrow(mydata))),1],
     mydata[c(row_year:max(nrow(mydata))),2*i],
     main=paste(colnames(mydata)[2*i],"2012 1-year"),pch=20)
}

row_month_start = 238124
row_month_end = 239611
row_day_start = 238124
row_day_end = 238171

par(mfrow=c(3,2))
for(i in 1:6){
plot(mydata[c(row_month_start:row_month_end),1],
     mydata[c(row_month_start:row_month_end),2*i],
     main=paste(colnames(mydata)[2*i],"2012 August-1"),pch=20)
}

#Wind data regression(NSW Sep 13) - adhoc 30 data points
mydata3 = read.csv("C:/Users/Neil/Google Drive/Academics/IO Research/Data/Data/Wind/Sep13 Wind.csv")
mywind_QLD = lm(mydata3$Price ~ mydata3$Demand + mydata3$Avgwind + mydata3$Percent + mydata3$Percent*mydata3$Avgwind + mydata3$Demand_QLD)
wind_QLD = lm(mydata3$Price ~ mydata3$Demand + mydata3$Avgwind + mydata3$Percent + mydata3$Percent*mydata3$Avgwind + mydata3$Demand_VIC)
summary(wind_QLD)
summary(wind_VIC)
