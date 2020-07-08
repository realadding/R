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

set.seed(6)

##游戏及分组参数填写
game = 'mg'
group = '200'

##读取处理数据
test_data <- read.csv(paste("C:/Users/Administrator/Desktop/lc_",game,"_",group,".csv",sep =""),sep=',',header = TRUE)

#head(test_data,10)

#str(test_data)

test_data$ds <- as.Date.character(test_data$ds)

test_data$login_ds <- as.Date.character(test_data$login_ds)

#fix(test_data)

weight <- filter(test_data,days == 1)

date_seq <- data.frame(unique(test_data$ds))

date_seq <- arrange(date_seq,date_seq[,1])
colnames(date_seq) <- 'date'

AR_ALL_CL <- data.frame(c(rep(0,nrow(date_seq))))
colnames(AR_ALL_CL) <- 'AR_ALL_CL'
AR_PAR_CL <- data.frame(c(rep(0,nrow(date_seq))))
colnames(AR_PAR_CL) <- 'AR_PAR_CL'
LM_PAR_CL <- data.frame(c(rep(0,nrow(date_seq))))
colnames(LM_PAR_CL) <- 'LM_PAR_CL'
rm(result_output)
result_output <- cbind(date_seq,AR_ALL_CL,AR_PAR_CL,LM_PAR_CL)

####数据预测

for (i in 1:nrow(date_seq)){
  
c_data <- filter(test_data,ds == date_seq[i,1])
  
#print(c_data)
  
c_data <- arrange(c_data,c_data[,3])

new_seq <- c(c_data[1,5],c_data[2:nrow(c_data),4])
  
#print(new_seq)  
  
##AR all

#length(new_seq)

x <- new_seq[1:(length(new_seq)-1)]

y <- new_seq[2:length(new_seq)]

p <- sum(x*y)/sum(x*x)

p_data <- rep(0,1000)

for (j in 1:1000){p_data[j] <- new_seq[length(new_seq)]*(p^j)}

p_data_f <- floor(p_data)

result_output[i,2] <- ceiling((sum(p_data_f) + sum(new_seq))/new_seq[1])

##AR 2/3

b <- ceiling(length(new_seq)*1/2)

x2 <- new_seq[b:(length(new_seq)-1)]

y2 <- new_seq[(b+1):length(new_seq)]

p <- sum(x2*y2)/sum(x2*x2)

p_data_2 <- rep(0,1000)

for (k in 1:1000){p_data_2[k] <- new_seq[length(new_seq)]*(p^k)}

p_data_f_2 <- floor(p_data)

result_output[i,3] <- ceiling((sum(p_data_f_2) + sum(new_seq))/new_seq[1])

##LM 2/3

y3 <- new_seq[b:length(new_seq)]

x3 <-c(b:length(new_seq))

#plot(x3,y3)

fit <- lm(y3~x3)

#fit$coefficients[1] + fit$coefficients[2] * n = 0

#n = floor(-(fit$coefficients[1] /fit$coefficients[2]))

p_data_3 <- rep(0,1000)

for (m in 1:1000){p_data_3[i] <- fit$coefficients[1] + fit$coefficients[2] * (length(new_seq)+i) }

result_output[i,4] <- ceiling((sum(p_data_3[p_data_3 > 1]) + sum(new_seq))/new_seq[1])
}

#year(result_output$date)

#mean(result_output$AR_ALL_CL)
#tapply(result_output$AR_ALL_CL, list(year(result_output$date), month(result_output$date)), mean)

##数据处理准备计算平均值

month <-format(result_output$date,format='%Y-%m')

result_update_1 <- cbind(month,result_output)

result_update_2 <- merge(result_update_1,weight,by.x = "date",by.y = "ds", all.x = TRUE)

result_update_2$new_user[which(is.na(result_update_2$new_user))] = floor(median(result_update_2$new_user,na.rm = TRUE))

result_update_2 <- subset(result_update_2,select = c(-login_ds,-days,-users))

month_seq <- data.frame(unique(result_update_2$month))

month_seq <- arrange(month_seq,month_seq[,1])

colnames(month_seq) <- 'month'

ar1_mean <- data.frame(c(rep(0,nrow(month_seq))))
colnames(ar1_mean) <- 'ar1_mean'
ar2_mean <- data.frame(c(rep(0,nrow(month_seq))))
colnames(ar2_mean) <- 'ar2_mean'
lm_mean <- data.frame(c(rep(0,nrow(month_seq))))
colnames(lm_mean) <- 'lm_mean'
ar1_w_mean <- data.frame(c(rep(0,nrow(month_seq))))
colnames(ar1_w_mean) <- 'ar1_w_mean'
ar2_w_mean <- data.frame(c(rep(0,nrow(month_seq))))
colnames(ar2_w_mean) <- 'ar2_w_mean'
lm_w_mean <- data.frame(c(rep(0,nrow(month_seq))))
colnames(lm_w_mean) <- 'lm_w_mean'

lc_result <- cbind(month_seq,ar1_mean,ar2_mean,lm_mean,ar1_w_mean,ar2_w_mean,lm_w_mean)

##计算算术平均&加权平均

for (i in 1:nrow(lc_result)){
  
  mean_compute_data <- filter(result_update_2,result_update_2$month == lc_result$month[i] )
  
  lc_result$ar1_mean[i] <- ceiling(mean(mean_compute_data$AR_ALL_CL))
  lc_result$ar2_mean[i] <- ceiling(mean(mean_compute_data$AR_PAR_CL))
  lc_result$lm_mean[i]  <- ceiling(mean(mean_compute_data$LM_PAR_CL))
  
  
  lc_result$ar1_w_mean[i] <- ceiling(weighted.mean(mean_compute_data$AR_ALL_CL,mean_compute_data$new_user))
  lc_result$ar2_w_mean[i] <- ceiling(weighted.mean(mean_compute_data$AR_PAR_CL,mean_compute_data$new_user))
  lc_result$lm_w_mean[i]  <- ceiling(weighted.mean(mean_compute_data$LM_PAR_CL,mean_compute_data$new_user))
  }

lc_result_all <- data.frame(month = 'all',
                            ar1_mean = ceiling(mean(result_update_2$AR_ALL_CL)),
                            ar2_mean = ceiling(mean(result_update_2$AR_PAR_CL)),
                            lm_mean  = ceiling(mean(result_update_2$LM_PAR_CL)),
                            ar1_w_mean = ceiling(weighted.mean(result_update_2$AR_ALL_CL,result_update_2$new_user)),
                            ar2_w_mean = ceiling(weighted.mean(result_update_2$AR_PAR_CL,result_update_2$new_user)),
                            lm_w_mean  = ceiling(weighted.mean(result_update_2$LM_PAR_CL,result_update_2$new_user)))
lc_result_total <- rbind(lc_result,lc_result_all)

##数据导出

write.csv(lc_result_total,paste("C:/Users/Administrator/Desktop/result_",game,"_",group,".csv",sep = ""))

print(lc_result_total)