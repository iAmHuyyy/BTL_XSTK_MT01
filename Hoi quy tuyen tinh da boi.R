#Doc du lieu
data <- read.csv("/Users/nguyennhathuy/Downloads/ALL_GPUs.csv.xls")
head(data)
View(data)

#Chon loc du lieu
new_DF <- data[c("Name", "Best_Resolution", "Core_Speed", "Manufacturer" ,"Memory", "Memory_Speed", "Memory_Bandwidth",  "Release_Date", "Process","Memory_Bus","L2_Cache")]
head(new_DF)

#Xu li dinh dang du lieu
new_DF$Core_Speed <- as.numeric(sub(" MHz", "", new_DF$Core_Speed))
new_DF$Memory <- as.numeric(sub(" MB", "", new_DF$Memory))
new_DF$Memory_Speed <- as.numeric(sub(" MHz", "", new_DF$Memory_Speed))
new_DF$Memory_Bandwidth <- as.numeric(gsub("\\s*GB/sec","", new_DF$Memory_Bandwidth))
new_DF$Process <- as.numeric(sub("nm", "", new_DF$Process))
new_DF$Memory_Bus <- as.numeric(sub(" Bit", "", new_DF$Memory_Bus))
new_DF$L2_Cache <- as.numeric(gsub("KB\\(.*\\)|KB", "", new_DF$L2_Cache))
View(new_DF)

#Lam sach du lieu
apply(is.na(new_DF),2,sum)
apply(is.na(new_DF),2,which)
apply(is.na(new_DF),2,mean)

new_DF <- subset(new_DF,!is.na(Memory_Speed) & !is.na(Memory_Bandwidth) & !is.na(Process))

median_memory <- median(new_DF$Memory, na.rm = TRUE)
new_DF$Memory <- ifelse(is.na(new_DF$Memory), median_memory,new_DF$Memory)

median_speed <- median(new_DF$Core_Speed, na.rm = TRUE)
new_DF$Core_Speed <- ifelse(is.na(new_DF$Core_Speed), median_speed, new_DF$Core_Speed)

median_process <- median(new_DF$Process, na.rm = TRUE)
new_DF$Process <- ifelse(is.na(new_DF$Process), median_process, new_DF$Process)
apply(is.na(new_DF),2,sum)

library(caret)
#ta dùng biến mục tiêu là ’Memory_Bandwidth’
set.seed(123)
train_index <- createDataPartition(new_DF$Memory_Bandwidth, p = 0.7, list = FALSE)
# Tập huấn luyện
train_data <- new_DF[train_index, ]
# Tập kiểm tra
test_data <- new_DF[-train_index, ]

model_1<-lm(Memory_Bandwidth ~ Memory + Process + Memory_Speed + L2_Cache +
              Memory_Bus,train_data)
summary((model_1))

par(mfrow = c(2, 2))
plot(model_1)

library(lmtest)
bptest(model_1)

train_data$residuals <- residuals((model_1)) # thêm cột residuals (sai số) vào train_data
mean_x <- mean(train_data$residuals)
sd_x <- sd(train_data$residuals)
n_x <-length(train_data$residuals)
z= (mean_x)/(sd_x)*sqrt(n_x)
z_alphachia2 =qnorm(p=0.05/2,lower.tail=FALSE)
data.frame(mean_x,sd_x,n_x,z,z_alphachia2)

test_data$Memory_Bandwidth_Predict<-predict(model_1, test_data)
head(test_data,5)
rmse <- sqrt(mean((test_data$Memory_Bandwidth -
                     test_data$Memory_Bandwidth_Predict) ^ 2))
print(paste("RMSE:", rmse))
r_squared <- 1 - (sum((test_data$Memory_Bandwidth -
                      test_data$Memory_Bandwidth_Predict) ^ 2) / sum((test_data$Memory_Bandwidth
                                                        -mean(test_data$Memory_Bandwidth)) ^ 2))
print(paste("R-squared:", r_squared))

train_data_log <-train_data
test_data_log<-test_data
train_data_log$Memory<-log(train_data_log$Memory +1)
train_data_log$Memory_Bus<-log(train_data_log$Memory_Bus +1)
train_data_log$Memory_Bandwidth<-log(train_data_log$Memory_Bandwidth +1)
train_data_log$Memory_Speed<-log(train_data_log$Memory_Speed +1)
train_data_log$L2_Cache<-log(train_data_log$L2_Cache +1)
train_data_log$Process<-log(train_data_log$Process +1)
test_data_log$Memory<-log(test_data_log$Memory +1)
test_data_log$Memory_Bus<-log(test_data_log$Memory_Bus +1)
test_data_log$Memory_Bandwidth<-log(test_data_log$Memory_Bandwidth +1)
test_data_log$Memory_Speed<-log(test_data_log$Memory_Speed +1)
test_data_log$L2_Cache<-log(test_data_log$L2_Cache +1)
test_data_log$Process<-log(test_data_log$Process +1)

model_2<-lm(Memory_Bandwidth ~ Memory + Process + Memory_Speed + L2_Cache +
              Memory_Bus, train_data_log)
summary(model_2)

plot(model_2,which = 2)

test_data_log$Memory_Bandwidth_Predict<-predict(model_2, test_data_log)
head(test_data_log,5)
rmse <- sqrt(mean((test_data_log$Memory_Bandwidth -
                    test_data_log$Memory_Bandwidth_Predict) ^ 2))
print(paste("RMSE:", rmse))
r_squared <- 1 - (sum((test_data_log$Memory_Bandwidth - 
                      test_data_log$Memory_Bandwidth_Predict) ^ 2) /
                      sum((test_data_log$Memory_Bandwidth -
                      mean(test_data_log$Memory_Bandwidth)) ^ 2))
print(paste("R-squared:", r_squared))

