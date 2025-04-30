All_GPUs <- read.csv("C:/Users/trana/Downloads/Bài Tập Lớn XSTK/Data/All_GPUs.csv")
View(All_GPUs)
head(df)
head(df)
str(df)
summary(df)
names(df)
colnames(df)
class(df)
print(df)
View(All_GPUs)
View(All_GPUs)
View(All_GPUs)
View(All_GPUs)
View(All_GPUs)
View(All_GPUs)
names(df)
colnames(df)
readLines("gpus.csv", n = 5)  # Đọc 5 dòng đầu tiên của tệp
readLines("ALL_GPUs.csv", n = 5)  
readLines("All_GPUs.csv", n = 5) 
readLines("ALL_GPUs", n = 5)  
readLines("ALL_GPUs.csv", n = 5)  
getwd()
readLines("ALL_GPUs.csv", n = 5)  
names(df)
colnames(df)
head(df)
View(All_GPUs)
View(All_GPUs)
All_GPUs <- read.csv("~/All_GPUs.csv")
View(All_GPUs)
rm(list = ls())
rm(list = ls())
All_GPUs <- read.csv("~/All_GPUs.csv")
View(All_GPUs)
All_GPUs <- read.csv("~/All_GPUs.csv")
View(All_GPUs)
#Chon loc du lieu
new_DF <- data[c("Name", "Best_Resolution", "Core_Speed", "Manufacturer" ,"Memory", "Memory_Speed", "Memory_Bandwidth",  "Release_Date", "Process","Memory_Bus","L2_Cache")]
# Đọc dữ liệu
data <- read.csv("C:/Users/hohai/Downloads/All_GPUs.csv")
# Đọc dữ liệu
data <- read.csv("E:/OneDrive/Documents/All_GPUs.csv")
head(data)
View(data)
# Chọn lọc dữ liệu
new_DF <- data[c("Name", "Best_Resolution", "Core_Speed", "Manufacturer", "Memory",
"Memory_Speed", "Memory_Bandwidth", "Release_Date", "Process",
"Memory_Bus", "L2_Cache")]
head(new_DF)
# Xử lý định dạng dữ liệu
new_DF$Core_Speed <- as.numeric(sub(" MHz", "", new_DF$Core_Speed))
new_DF$Memory <- as.numeric(sub(" MB", "", new_DF$Memory))
new_DF$Memory_Speed <- as.numeric(sub(" MHz", "", new_DF$Memory_Speed))
new_DF$Memory_Bandwidth <- as.numeric(gsub("\\s*GB/sec\\s*", "", new_DF$Memory_Bandwidth))
new_DF$Process <- as.numeric(sub("nm", "", new_DF$Process))
new_DF$Memory_Bus <- as.numeric(sub(" Bit", "", new_DF$Memory_Bus))
new_DF$L2_Cache <- as.numeric(gsub("KB\\s*\\(.*\\)|KB", "", new_DF$L2_Cache))
View(new_DF)
# Làm sạch dữ liệu
# Kiểm tra NA
apply(is.na(new_DF), 2, sum)  # Tổng số NA trên mỗi cột
apply(is.na(new_DF), 2, mean)  # Tỷ lệ NA trên mỗi cột
# Loại bỏ các hàng có NA ở Memory_Speed, Memory_Bandwidth, Process
new_DF <- subset(new_DF, !is.na(Memory_Speed) & !is.na(Memory_Bandwidth) & !is.na(Process))
# Thay thế NA bằng trung vị cho các cột còn lại
median_memory <- median(new_DF$Memory, na.rm = TRUE)
new_DF$Memory <- ifelse(is.na(new_DF$Memory), median_memory, new_DF$Memory)
median_speed <- median(new_DF$Core_Speed, na.rm = TRUE)
new_DF$Core_Speed <- ifelse(is.na(new_DF$Core_Speed), median_speed, new_DF$Core_Speed)
median_process <- median(new_DF$Process, na.rm = TRUE)
new_DF$Process <- ifelse(is.na(new_DF$Process), median_process, new_DF$Process)  # Sửa lỗi median_memoryBus
# Kiểm tra lại NA
apply(is.na(new_DF), 2, sum)
printf (manufacturer_table <- table(my_data$manufacturer))
print (manufacturer_table <- table(my_data$manufacturer))
printf (manufacturer_table <- table(All_GPUs$manufacturer))
print (manufacturer_table <- table(All_GPUs$manufacturer))
print (Manufacturer_table <- table(All_GPUs$Manufacturer))
All_GPUs_filtered <- All_GPUs[All_GPUs$Manufacturer %in% names(Manufacturer_table[Manufacturer_table > 300]), ]
table(All_GPUs_filtered$Manufacturer)
All_GPUs_filtered <- All_GPUs[All_GPUs$Manufacturer %in% names(Manufacturer_table[Manufacturer_table > 300]), ]> table(All_GPUs_filtered$Manufacturer)
All_GPUs_filtered <- All_GPUs[All_GPUs$Manufacturer %in% names(Manufacturer_table[Manufacturer_table > 100]), ]
# Lọc dữ liệu cho AMD
AMD_data <- subset(All_GPUs, Manufacturer == "AMD")
# Vẽ QQ-plot để kiểm tra phân phối chuẩn bằng đồ thị cho AMD
qqnorm(AMD_data$Core_Speed, main = "QQ-plot for AMD Core Speed")
# Lọc dữ liệu cho AMD
AMD_data <- subset(All_GPUs_filtered, manufacturer == "AMD")
# Lọc dữ liệu cho AMD
AMD_data <- subset(new_DF_filtered, manufacturer == "AMD")
View(All_GPUs_filtered)
All_GPUs <- read.csv("~/All_GPUs.csv")
View(All_GPUs)
#Doc du lieu
data <- read.csv("E:/OneDrive/Documents/All_GPUs.csv")
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
new_DF$Process <- ifelse(is.na(new_DF$Process), median_memoryBus, new_DF$Process)
apply(is.na(new_DF),2,sum)
print(Manufacturer_table <- table(new_DF$Manufacturer))
#Lọc dữ liệu với số lượng quan sát theo tên nhiều hơn 200
new_DF_filtered <- new_DF [new_DF$Manufacturer %in% names (Manufacturer_table [Manufacturer_table > 200]), ]
#In ra kết quả
table (new_DF_filtered $Manufacturer)
#Vẽ đồ thị hàm phân phối chuẩn cho AMD
AMD_data <- subset (new_DF_filtered, new_DF_filtered$Manufacturer=="AMD")
qqnorm (AMD_data$Core_Speed_value)
#Vẽ đồ thị hàm phân phối chuẩn cho AMD
AMD_data <- subset (new_DF_filtered, new_DF_filtered$Manufacturer=="AMD")
qqnorm (AMD_data$Core_Speed_value)
#Vẽ đồ thị hàm phân phối chuẩn cho AMD
AMD_data <- subset (new_DF_filtered, new_DF_filtered$Manufacturer=="AMD")
qqnorm (AMD_data$Core_Speed)
qqline (AMD_data$Core_Speed)
View(new_DF_filtered)
View(new_DF_filtered)
summary(AMD_data$Core_Speed)
# Loại bỏ NA trước khi vẽ
AMD_data_clean <- subset(AMD_data, !is.na(Core_Speed))
# Vẽ lại Q-Q plot
qqnorm(AMD_data_clean$Core_Speed, main = "QQ-plot for AMD Core Speed (Cleaned)")
qqline(AMD_data_clean$Core_Speed)
View(AMD_data_clean)
#Vẽ đồ thị hàm phân phối chuẩn cho AMD
AMD_data <- subset (new_DF_filtered, new_DF_filtered$Manufacturer=="AMD")
qqnorm (AMD_data$Core_Speed)
qqline (AMD_data$Core_Speed)
#Kiểm tra phân phối chuẩn cho AMD bằng hàm shapiro.test
shapiro.test(AMD_data$Core_Speed)
#Vẽ đồ thị hàm phân phối chuẩn cho Nvidia
Nvidia_data <- subset (new_DF_filtered, new_DF_filtered$Manufacturer=="Nvidia")
qqnorm (Nvidia_data$Core_Speed)
qqline (Nvidia_data$Core_Speed)
shapiro.test(Nvidia_data$Core_Speed)
View(Nvidia_data)
View(AMD_data)
View(Nvidia_data)
View(AMD_data_clean)
View(AMD_data)
View(AMD_data_clean)
View(AMD_data_clean)
View(data)
View(new_DF)
View(new_DF_filtered)
View(Nvidia_data)
View(Nvidia_data)
library("car")
install.packages("car")
install.packages("car")
library("car")
leveneTest(Core_Speed~as.factor(Manufacturer),new_DF_filtered)
boxplot(Core_Speed ~ Manufacturer, data = new_DF_filtered, main = "Boxplot of Core Speed by Manufacturer")
anova_model <- aov(Core_Speed ~ Manufacturer, data = new_DF_filtered)
summary(anova_model)
# Lưu kết quả nhận được vào anova_model để tái sử dụng lại
anova_model <- aov(Core_Speed ~ Manufacturer, data = new_DF_filtered)
#In kết quả
summary(anova_model)
# Lưu hàm TukeyHSD vào tukey_res để tái sử dụng
tukey_res <- TukeyHSD(anova_model, "Manufacturer")
tukey_res
# Vẽ đồ thị so sánh bội giữa các nhóm
plot(tukey_res)
q()
