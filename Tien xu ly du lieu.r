#Doc du lieu
data <- read.csv("C:/Users/hohai/Downloads/All_GPUs.csv")
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