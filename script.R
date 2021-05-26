library(tidyverse)
library(tidyquant)
library(caret)

# btc <- read.csv('btcusd.csv')
# btc$time <- as.integer(rownames(btc))
# 
# ggplot(data=btc,mapping=aes(x=time)) +
#   geom_barchart(mapping=aes(open=open,close=close,high=high,low=low))
#   
# 
# data <- btc %>%
#   mutate(date = floor(time/1440),open_day = NA,close_day= NA, high_day=NA, low_day=NA,volume_day=NA)
#   
# f = function(data) {
#   volume <- 0
#   high <- NA
#   low <- NA
#   for(i in 1:nrow(data)){
#     if(i %% 1440 == 1) {
#       data[i,8] = data[i,2]
#       volume = data[i,6]
#       high = data[i,4]
#       low = data[i,5]
#     }
#     else if(i %% 1440 == 0) {
#       data[i,9] = data[i,3]
#       data[i,10] = high
#       data[i,11] = low
#       data[i,12] = volume
#       
#     } else {
#       volume = volume + data[i,6]
#       high = max(c(data[i,4], high))
#       low = min(c(data[i,5], low))
#     }
#     
#   }
#   return(data)
# }
# 
# data <- f(data)
# real_data <- data %>% 
#   fill(open_day,.direction = 'down') %>% 
#   fill(close_day, .direction='up') %>% 
#   fill(high_day, .direction = 'up') %>% 
#   fill(low_day, .direction = 'up') %>% 
#   fill(volume_day, .direction = 'up') %>% 
#   group_by(date, open_day, close_day)
# 
# x <- seq(1,nrow(data), by =1440)
# real_data <- real_data[x,]
# real_data <- real_data %>% 
#   select(-c(time,open,close,high,low,volume)) %>% 
#   drop_na()

real_data <- read.csv('real_data.csv') %>% drop_na()

ggplot(data=real_data,aes(x = date)) +
  geom_candlestick(aes(open=open_day, close=close_day,high=high_day, low=low_day)) +
  geom_ma(aes(y= close_day,color='red'),ma_fun = SMA, n=25) +
  geom_ma(aes(y= close_day), ma_fun = SMA, n=7) +
  geom_ma(aes(y= close_day,color='red'),ma_fun = EMA, n=25) +
  geom_ma(aes(y= close_day), ma_fun = EMA, n=7) +
  geom_line(aes(y = close_day)) +
  theme_tq() +
  xlim(2050,nrow(real_data))

real_data_2 <- real_data %>% 
  mutate(
    ema7 = EMA(close_day,7),
    ema25 = EMA(close_day,25),
    ema7_m = ema7 - lag(ema7),
    ema25_m = ema25 - lag(ema25),
    buy_rate = ifelse(ema7 >ema25,1,0) + ifelse(close_day > ema7,1,0) + ifelse(ema7_m > 0,1,0),
    should_buy = factor(ifelse(buy_rate == 3, 'yes','no'))
) %>% drop_na()




mid <- nrow(real_data_2) * 0.75
data_train <- real_data_2[1:mid,]
data_test <- real_data_2[(mid+1):nrow(real_data_2),]

bucket_test <- data_test

model <- glm(should_buy ~ close_day + ema7 + ema25,data=data_train, family = binomial)
summary(model)
#
#
#
result <- predict(model, bucket_test, type = 'response')
res_c <-factor(ifelse(result > 0.490, "yes","no"))


#
confusionMatrix(res_c,data_test$should_buy,mode='prec_recall',positive ='yes')

bucket_test$prediction <- res_c

money <- 1000
 # 0 = money coin = 0
coin <- 0
for (i in 1:nrow(bucket_test)){
  if(money != 0 && bucket_test[[i,'prediction']] == 'yes') {
    coin = money / bucket_test[[i,'close_day']]
    money = 0
  }
  if(money == 0 && bucket_test[[i,'prediction']] == 'no'){
    money = coin * bucket_test[[i,'close_day']]
    coin = 0
  }
}













# mid <- floor(0.75 * nrow(real_data))
# data_train <- real_data[1:mid,]
# data_test <- real_data[(mid+1):nrow(real_data),]
# 
# 
# model <- lm(high_day ~ open_day + close_day -date, data_train)
# summary(model)
# bucket_test <- data_test
# bucket_test$high_day <- NA
# 
# result <- predict(model, newdata = bucket_test)
# bucket_test$Prediction <- result
# bucket_test$high_day <- data_test$high_day
# 
# 
# ggplot()+
#   geom_line(data=bucket_test, mapping=aes(x=date,y=high_day,color='blue',alpha=.5)) +
#   geom_line(data=bucket_test, mapping=aes(x=date,y=Prediction,color='red')) +
#   xlim(2200,2300)

