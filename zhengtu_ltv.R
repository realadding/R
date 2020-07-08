rm(list =ls())

library(ggplot2)
library(ggthemes)
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(reshape)
library(tseries)
library(forecast)
library(urca)
library(tensorflow)
library(keras)

set.seed(6)

raw_data <- read.csv("C:/Users/Administrator/Desktop/zhengtu_ltv.csv",sep=',',header = TRUE)

raw_data$ds <- as.Date.character(raw_data$ds)

new_data <- spread(raw_data,day_between,payamount)

ltv_data <- c(1:(ncol(new_data)-2))

for (i in 1:length(ltv_data))
{
  ltv_data[i] <- round(sum(new_data[,i+2],na.rm = TRUE)/sum(new_data[1:(nrow(new_data)-i+1),2]),4)
}



ggplot(data = as.data.frame(log(ltv_data))) +
  geom_line(
    mapping = aes(
      x = c(1:length(ltv_data)),
      y = ltv_data)) +
  geom_point(
    mapping = aes(
      x = c(1:length(ltv_data)),
      y = ltv_data))
  


###############################ARIMA######################



test <-ts(ltv_data,frequency = 7)

results <- decompose(test)

results$seasonal[1:7]

ltv_data <- ltv_data-results$seasona

plot.ts(ltv_data)

train_data <-ltv_data[10:60]
test_data <- ltv_data[(length(train_data)+7):length(ltv_data)]

adf.test(train_data)
adf.test(diff(train_data,1))

summary(ur.df(diff(train_data,1)))

#tests <-HoltWinters(ltv_data,beta = FALSE,gamma = FALSE)

par(mfrow=c(1,1))

acf(diff(train_data,1),lag.max = 20) #2
pacf(diff(train_data,1),lag.max = 60) #1


auto.arima(train_data,trace = TRUE)


fit <- Arima(train_data,order=c(2,1,1),xreg = (1:length(train_data)))



fit
tsdiag(fit)

plot(fit$residuals)

qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box")

forecast <- forecast(fit,h=50,xreg=(length(train_data):(length(train_data)+50)))

forecast_date <-forecast$mean

forecast_date<- as.vector(forecast_date)

forecast_date[which(forecast_date<0)]<-0

plot.ts(forecast_date)

df <- data.frame(
  index = 1:length(ltv_data),
  value = as.numeric(ltv_data),
  type = 'raw') %>%
  rbind(
    data.frame(
      index = 1:length(forecast_date)+(length(train_data)),
      value = as.numeric(forecast_date),
      type = 'forecast'))

ggplot(data = df) +
  geom_line(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_point(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_vline(
    xintercept = length(train_data) + 0.5) 



################################LM###############################



n<- 8

y <- ltv_data[n:length(ltv_data)]

x1<-ltv_data[(n-1):(n-1+length(y)-1)]
x2<-ltv_data[(n-2):(n-2+length(y)-1)]
x3<-ltv_data[(n-3):(n-3+length(y)-1)]
x4<-ltv_data[(n-4):(n-4+length(y)-1)]
x5<-ltv_data[(n-5):(n-5+length(y)-1)]
x6<-ltv_data[(n-6):(n-6+length(y)-1)]
x7<-ltv_data[(n-7):(n-7+length(y)-1)]
x8<-c(n:length(ltv_data))

ltv_data_new <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8)


train_data <- ltv_data_new[8:21,]
test_data <-ltv_data_new[(nrow(train_data)+1):nrow(ltv_data_new),]


fit <- lm(y ~.,data = train_data)

summary(fit)
library(carData)
library(car)

vif(fit)

library(MASS)

fit2 <- lm.ridge(y~.,lambda = seq(0,150,length.out = 151),data = train_data,model = TRUE)

names(fit2)

plot(fit$residuals)

par(mfrow =c(2,2))
plot(fit)

hist(fit$residuals)

qqnorm(fit$residuals)
qqline(fit$residuals)


trainPredict <-predict(fit)

testPredict <- predict(fit,test_data,interval = "prediction",level = 0.95)


df <- data.frame(
  index = 1:length(ltv_data),
  value = ltv_data,
  type = 'raw') %>%
  rbind(
    data.frame(
      index = 1:length(trainPredict)+7,
      value = trainPredict,
      type = 'train')) %>%
  rbind(
    data.frame(
      index = 1:nrow(testPredict)+length(trainPredict)+7,
      value = testPredict[,1],
      type = 'test'))

ggplot(data = df) +
  geom_line(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_point(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_vline(
    xintercept = length(trainPredict) + 7.5) 

#############LSTM############################

dataset<-ltv_data

###LSTM

create_dataset <- function(dataset,
                           look_back)
{
  l <- length(dataset)
  
  dataX <- array(dim = c(l - look_back, look_back))
  
  for (i in 1:ncol(dataX))
  {
    dataX[, i] <- dataset[i:(l - look_back + i - 1)]
  }
  
  dataY <- array(
    data = dataset[(look_back + 1):l],
    dim = c(l - look_back, 1))
  
  return(
    list(
      dataX = dataX,
      dataY = dataY))
}


train_size <- 21
test_size <- length(dataset) - train_size

train <- dataset[1:train_size]
test <- dataset[(train_size + 1):length(dataset)]

cat(length(train), length(test))


look_back <- 1
trainXY <- create_dataset(train, look_back)
testXY <-  create_dataset(test, look_back)

dim_train <- dim(trainXY$dataX)
dim_test <- dim(testXY$dataX)

dim(trainXY$dataX) <- c(dim_train[1], 1, dim_train[2])
dim(testXY$dataX) <- c(dim_test[1], 1, dim_test[2])

model <- keras_model_sequential()

model %>%
  layer_lstm(
    units = 4,
    input_shape = c(1, look_back)) %>%
  layer_dense(
    units = 1) %>%
  compile(
    loss = 'mean_squared_error',
    optimizer = 'adam') %>%
  fit(trainXY$dataX,
      trainXY$dataY,
      epochs = 100,
      batch_size = 1,
      verbose = 2)


trainScore <- model %>%
  evaluate(
    trainXY$dataX,
    trainXY$dataY,
    verbose = 2)

testScore <- model %>%
  evaluate(
    testXY$dataX,
    testXY$dataY,
    verbose = 2)


trainPredict <- model %>%
  predict(
    trainXY$dataX,
    verbose = 2)
testPredict <- model %>%
  predict(
    testXY$dataX,
    verbose = 2)



df <- data.frame(
  index = 1:length(dataset),
  value = dataset,
  type = 'raw') %>%
  rbind(
    data.frame(
      index = 1:length(trainPredict) + look_back,
      value = trainPredict,
      type = 'train')) %>%
  rbind(
    data.frame(
      index = 1:length(testPredict) + look_back + length(train),
      value = testPredict,
      type = 'test'))

ggplot(data = df) +
  geom_line(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_point(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_vline(
    xintercept = length(train) + 0.5) +
  theme_economist() +
  scale_color_economist()
