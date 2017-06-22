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
