rm(list =ls())
##xyy_data <- read.table("C:/Users/Administrator/Desktop/t.txt",sep = '\t')

xyy_data <- read.csv("C:/Users/Administrator/Desktop/xyy_pay.csv",sep=',',header = TRUE)

##把表中所有null的值设置为空值
for (i in 1:ncol(xyy_data))
{
  xyy_data[which(xyy_data[,i] == 'NULL'),i] <- NA
}

xyy_data$ouid <- as.character(xyy_data$ouid)
xyy_data$max_payamount_3m <- as.numeric(xyy_data$max_payamount_3m)
xyy_data$min_payamount_3m <- as.numeric(xyy_data$min_payamount_3m)
xyy_data$max_payamount_2m <- as.numeric(xyy_data$max_payamount_2m)
xyy_data$min_payamount_2m <- as.numeric(xyy_data$min_payamount_3m)
xyy_data$max_payamount_1m <- as.numeric(xyy_data$max_payamount_1m)
xyy_data$min_payamount_1m <- as.numeric(xyy_data$min_payamount_1m)
xyy_data$max_payamount_2w <- as.numeric(xyy_data$max_payamount_2w)
xyy_data$min_payamount_2w <- as.numeric(xyy_data$min_payamount_2w)
xyy_data$max_payamount_1w <- as.numeric(xyy_data$max_payamount_1w)
xyy_data$min_payamount_1w <- as.numeric(xyy_data$min_payamount_1w)
xyy_data$max_day_payamount_3m <- as.numeric(xyy_data$max_day_payamount_3m)
xyy_data$min_day_payamount_3m <- as.numeric(xyy_data$min_day_payamount_3m)
xyy_data$max_day_payamount_2m <- as.numeric(xyy_data$max_day_payamount_2m)
xyy_data$min_day_payamount_2m <- as.numeric(xyy_data$min_day_payamount_2m)
xyy_data$max_day_payamount_1m <- as.numeric(xyy_data$max_day_payamount_1m)
xyy_data$min_day_payamount_1m <- as.numeric(xyy_data$min_day_payamount_1m)
xyy_data$max_day_payamount_2w <- as.numeric(xyy_data$max_day_payamount_2w)
xyy_data$min_day_payamount_2w <- as.numeric(xyy_data$min_day_payamount_2w)
xyy_data$max_day_payamount_1w <- as.numeric(xyy_data$max_day_payamount_1w)
xyy_data$min_day_payamount_1w <- as.numeric(xyy_data$min_day_payamount_1w)

xyy_data$first_ds <- as.Date(xyy_data$first_ds)
xyy_data$first_pay_ds <- as.Date(xyy_data$first_pay_ds)

##sample test

# set.seed(1234)
# 
# test <- sample(nrow(xyy_data),0.5*nrow(xyy_data)) 
# 
# tdata <- xyy_data[test,] 

#as.Date('2020-07-31') - tdata$first_ds

#tdata <- subset(xyy_data,his_payamount > 10)

tdata <- xyy_data

#nrow(tdata)

##varible choose

##pay his_pays his_times pays_2w_in_3m

##act reg_days days_3m_in_reg days_2w_in_3m

#head(tdata)

w <- as.integer(Sys.Date()-as.Date('2020-05-01')) / as.integer(Sys.Date()-as.Date('2020-07-15'))

weight <- (as.integer(Sys.Date()-tdata$first_ds)/as.integer(Sys.Date()-as.Date('2020-05-01'))) * w

weight[which(tdata$first_ds <= '2020-05-01')] <- w

weight[which(tdata$first_ds >= '2020-07-15')] <- 1.0

weight <- as.data.frame(weight)

tdata <- cbind(tdata,weight)

pay_tdata <- data.frame(ouid = tdata$ouid,
                        his_pays = tdata$his_payamount,
#                       his_times = tdata$his_paytimes,
#                       pays_2w_in_3m = (tdata$all_payamount_2w/(tdata$all_payamount_3m + 0.000001)),
                        pays_2w_in_3m = ((tdata$all_payamount_2w/(tdata$all_payamount_3m + 0.000001))*tdata$weight),
#                       reg_days = as.integer(Sys.Date() - tdata$first_ds),
#                       days_3m_in_reg = tdata$login_days_3m/as.integer((Sys.Date() - tdata$first_ds) + 1),
                        days_2w_in_3m = ((tdata$login_days_2w/(tdata$login_days_3m + 0.000001))*tdata$weight)
                        )


# pay_tdata$pays_2w_in_3m[pay_tdata$pays_2w_in_3m > 1] <- 1
# pay_tdata$days_2w_in_3m[pay_tdata$days_2w_in_3m > 1] <- 1

#head(pay_tdata,10)

#subset(tdata, ouid == '100577646')

#names(pay_tdata) <- c('ouid','his','3m','2m','1m','2w','1w')

rownames(pay_tdata) <- pay_tdata$ouid


#head(pay_tdata)

s_pay_tdata <- scale(pay_tdata[,2:ncol(pay_tdata)])

#s_pay_tdata <- pay_tdata

#head(s_pay_tdata)

# install.packages('factoextra')
# 
# library(ggplot2)
# library(factoextra)

##fviz_nbclust(s_pay_tdata, kmeans, method = "wss") ##+ geom_vline(xintercept = 4, linetype = 2)

##act his

e <- rep(0,14)

for (k in 2:15){
  
  km_result <- kmeans(s_pay_tdata,k,iter.max = 20)
  
  e[k-1] <- km_result$betweenss/km_result$totss
}

e

plot.ts(e)

km_result <- kmeans(s_pay_tdata,8,iter.max = 20)

#km_result

result <- cbind(pay_tdata,cluster = km_result$cluster)

# head(result)

aggregate(cbind(his_pays,pays_2w_in_3m,days_2w_in_3m) ~ cluster,result,"mean")

#km_result$size/sum(km_result$size)

cbind(aggregate(cbind(his_pays,pays_2w_in_3m,days_2w_in_3m) ~ cluster,result,"mean"),
      aggregate(cbind(his_pays) ~ cluster,result,"sum"),
      pays_percent = aggregate(cbind(his_pays) ~ cluster,result,"sum")$his_pays/sum(aggregate(cbind(his_pays) ~ cluster,result,"sum")$his_pays),
      users_percent = as.vector(km_result$size/sum(km_result$size))
      )



#mean(pay_tdata$days_2w_in_3m[pay_tdata$his_pays<=10],na.rm = TRUE)

#sum(km_result$size)
