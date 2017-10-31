cat("\014")
rm(list=ls())
source("D:/git/header.R")

# import data
load(paste0(dataroot,"/datasets/markets.xts"))

# create consecutive increase variable
analysis <- market_returns %>%
  as.data.frame() %>% 
  mutate_all(funs(sign=sign(.))) %>% 
  mutate_at(vars(ends_with("sign")),funs(group = cumsum(c(F,diff(.)!=0)))) %>% 
  mutate(date = index(market_returns)) %>% 
  group_by(GSPC_RET_sign_group) %>% 
  mutate(index = row_number() * GSPC_RET_sign)

# summmarize by year
sum_by_year <- analysis %>% 
  group_by(year=year(date), index) %>% 
  summarise(ct_day = n()) %>% 
  spread(index, ct_day)

chart_data <- analysis %>% 
  group_by(year=year(date), index) %>% 
  summarise(ct_day = n())

# visualization
ggplot(chart_data, aes(x=year, y=index, fill=ct_day)) +
  geom_tile() +
  scale_fill_continuous(low="#2171B5", high="red") +
  theme_bw()

# function to create lag variables
add_lag <- function(data, var, lag_start=1, lag_end) {
  #assign(paste0(data,"$",var),1)
  #data$var = 1
  #return(data)
  #print(paste0(data,"$",var))
  a<-lapply(seq(lag_start, lag_end), function(x) data[,paste0(var,"_lag",x)]=lag(data[[var]],x))
  #data[,paste0(var,"_lag",1)]=lag(data[[var]])
  
  return(a)
}

analysis2 <- add_lag(analysis,var="index", 1, 3)


# decision tree with lag1
dt <- rpart(index ~ lag(index) + lag(index,2) + lag(index, 3), 
            data = analysis[analysis$date>='2016-01-01',], method = "class")

dt_all <- rpart(index ~ lag(index) + lag(index,2) + lag(index, 3), 
            data = analysis, method = "class")

rpart.plot(dt)
rpart.plot(dt_all)
library(rpart.plot)

analysis$lag1 = lag(analysis$index)
analysis$lag2 = lag(analysis$index,2)
analysis$lag3 = lag(analysis$index,3)
analysis$lag4 = lag(analysis$index,4)
analysis$lag5 = lag(analysis$index,5)

# apply random forest to predict return direction
library(randomForest)
rf.fit <- randomForest(index~lag1 + lag2 + lag3 + lag4 + lag5, analysis[analysis$date<'2016-01-01',],na.action = na.roughfix, ntree=1000)
rf.predict <- predict(rf.fit, analysis[analysis$date>='2016-01-01',])

combine <- cbind(analysis$index[analysis$date>='2016-01-01'], rf.predict)
mean(sign(combine[,1])==sign(combine[,2]))

# scatter index with lags
pairs(analysis[c('index', 'lag1', 'lag2', 'lag3', 'lag4', 'lag5')])

# examine consecutive days return magnitude
plot(analysis$index, analysis$GSPC_RET)

# distribution of returns
hist(analysis$GSPC_RET, breaks=500)
