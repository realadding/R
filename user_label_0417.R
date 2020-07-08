rm(list = ls())

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(reshape)
library(proxy)
library(dtw)
library(rpart)

#载入数据
user_labels <- read_csv("C:/Users/Administrator/Desktop/user_labels.csv",locale = locale(encoding = "GB2312"))

#fix(user_labels)

# 缺失数据处理&类型转换
user_labels$is_stay[user_labels$is_stay == 'null'] <- 0

user_labels$is_pay[user_labels$is_pay == 'null'] <- 0

user_labels$pay_amount[user_labels$pay_amount == 'null'] <- 0

user_labels$is_stay <- as.numeric(user_labels$is_stay)

user_labels$is_pay <- as.numeric(user_labels$is_pay)

user_labels$pay_amount <- as.numeric(user_labels$pay_amount)

#reshape
#user_labels <- filter(user_labels,app_id == 21)

test_data <- user_labels %>% separate_rows(labels, sep = "\\|")

rank_num <- c(1:nrow(test_data))

test_data <- cbind(test_data,rank_num)

test_data <- filter(test_data, did != '00-00-00-00-00-00-00-00-00-00-00-00-00-00-00-00'& did != '00:00:00:00:00:00'& did != '00000000000' & did != '000000000000000')

test_data_1 <- (
  test_data %>%
    group_by(did) %>%
    arrange(did,rank(rank_num))   %>%
    mutate(score = rank(rank_num), ties.method = "first" ))

test_data_1$score <- as.numeric(test_data_1$score)

test_data_1 <- filter(test_data_1,score <= 10)

table(test_data_1$score)

#filter(test_data_1,score ==12)
#filter(user_labels,did =='862538031767151')

test_data_1$score <- 11 - test_data_1$score

test_data_1 <- filter(test_data_1,score >= 7)

#test_data_1$score[test_data_1$score ==10] <- 1
#test_data_1$score[test_data_1$score ==9] <- 0.75
#test_data_1$score[test_data_1$score ==8] <- 0.5
#test_data_1$score[test_data_1$score ==7] <- 0.25

test_data_1$score[test_data_1$score >=7] <- 1

#table(test_data_1$score)

test_data_2 <- select(test_data_1,-rank_num,-ties.method)

##test_data_2 <- test_data_2[-43935,]

test_data_3 <- spread(test_data_2,labels,score)

#head(test_data_3,1)

test_data_3[is.na(test_data_3)] <- 0
#test_data_3[,8:ncol(test_data_3)] <- as.double(unlist(test_data_3[,8:ncol(test_data_3)]))

#fix(test_data_3)

#### TGI INDEX

options(digits=4)
options(max.print=10000)

#PAY_RATE_TGI

label<- c(1:(ncol(test_data_3)-7))
s_value<- label 
c_value<- label
s_percent <- label
c_percent <- label
lift_value  <- label

label_pay <- data.frame(label,s_value,c_value,s_percent,c_percent,lift_value)

for (i in 1:nrow(label_pay)){
  label_pay[i,1]<- names(test_data_3)[i+7]
  label_pay[i,2]<-sum(ifelse(test_data_3[,i+7]>0,1,0))
  label_pay[i,3]<-sum(ifelse(test_data_3$is_pay == 1&test_data_3[,i+7]>0,1,0))}

label_pay[,4] <- paste(round((label_pay[,2]/length(unique(test_data_3$did)))*100,2),'%',sep ='')
label_pay[,5] <- paste(round(label_pay[,3]/(label_pay[,2])*100,2),'%',sep ='')
label_pay[,6] <- paste(
  round(
    ( 
      (label_pay[,3]/label_pay[,2])
      /
        ((length(unique(filter(test_data_3,is_pay == 1)$did)))/length(unique(test_data_3$did)))
    )
    *100,2),'%',sep ='')

#print(label_pay)


#STAY_RATE_TGI

label_stay <- data.frame(label,s_value,c_value,s_percent,c_percent,lift_value)

for (i in 1:nrow(label_stay)){
  label_stay[i,1]<- names(test_data_3)[i+7]
  label_stay[i,2]<-sum(ifelse(test_data_3[,i+7]>0,1,0))
  label_stay[i,3]<-sum(ifelse(test_data_3$is_stay == 1&test_data_3[,i+7]>0,1,0))}

label_stay[,4] <- paste(round((label_stay[,2]/length(unique(test_data_3$did)))*100,2),'%',sep ='')
label_stay[,5] <- paste(round(label_stay[,3]/(label_stay[,2])*100,2),'%',sep ='')
label_stay[,6] <- paste(
  round(
    ( 
      (label_stay[,3]/label_stay[,2])
      /
        ((length(unique(filter(test_data_3,is_stay == 1)$did)))/length(unique(test_data_3$did)))
    )
    *100,2),'%',sep ='')


#print(label_stay)

#ARPU_TGI

arpu <- label

label_arpu <- data.frame(label,s_value,c_value,s_percent,arpu,lift_value)

for (i in 1:nrow(label_arpu)){
  label_arpu[i,1]<- names(test_data_3)[i+7]
  label_arpu[i,2]<-sum(ifelse(test_data_3[,i+7]>0&test_data_3$is_pay == 1,1,0))
  label_arpu[i,3]<-sum(ifelse(test_data_3$pay_amount >0 & test_data_3[,i+7]>0,test_data_3$pay_amount,0))}

label_arpu[,4] <- paste(round((label_arpu[,2]/length(unique(filter(test_data_3,is_pay == 1)$did)))*100,2),'%',sep ='')
label_arpu[,5] <- round(label_arpu[,3]/(label_arpu[,2]),2)
label_arpu[,6] <- paste(
  round(
    ( 
      (label_arpu[,3]/label_arpu[,2])
      /
        (sum(test_data_3$pay_amount)/(length(unique(filter(test_data_3,is_pay == 1)$did))))
    )
    *100,2),'%',sep ='')


#print(label_arpu)
#filter(label_arpu,s_percent>= 1%,lift_value>= 100%)
write.csv(filter(label_pay,s_percent>= '1%'),file="C:/Users/Administrator/Desktop/label_pay.csv")
write.csv(filter(label_stay,s_percent>='1%'),file="C:/Users/Administrator/Desktop/label_stay.csv")
write.csv(filter(label_arpu,s_percent>= '1%'),file="C:/Users/Administrator/Desktop/label_arpu.csv")
