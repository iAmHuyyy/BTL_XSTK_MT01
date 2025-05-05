setwd("C:/R/test")
readFile <- read.csv("/Users/nguyennhathuy/Downloads/ALL_GPUs.csv.xls")

#install.packages("tidyr")
#install.packages("dplyr")
library(tidyr)
#library(dplyr)


#data <- readFile[,c("Name", "Best_Resolution", "Core_Speed", "Max_Power", "Memory", "Memory_Bandwidth", "Memory_Speed")]
data <- readFile[, c("Core_Speed", "Max_Power", "Memory", "Memory_Bandwidth", "Memory_Bus", "Memory_Speed", "Pixel_Rate", "Process", "ROPs", "TMUs", "Texture_Rate")]

data[data == ""] <- NA







data$Core_Speed <- gsub("\n-", "", data$Core_Speed)
data$Core_Speed <- gsub(" MHz", "", data$Core_Speed)
data$Core_Speed <- as.numeric(data$Core_Speed)


data$Max_Power <- gsub("Watts", "", data$Max_Power)
data$Max_Power <- as.numeric(data$Max_Power)


data$Memory <- gsub("MB", "", data$Memory)
data$Memory <- as.numeric(data$Memory)


data$GB_temp <- data$Memory_Bandwidth  # Copy the original data
data$GB_temp <- ifelse(grepl("MB/sec", data$GB_temp), NA, data$GB_temp)  # Remove 'MB/sec' values (set to NA)
data$GB_temp <- as.numeric(gsub("GB/sec", "", data$GB_temp))

data$MB_temp <- data$Memory_Bandwidth  # Copy the original data
data$MB_temp <- ifelse(grepl("GB/sec", data$MB_temp), NA, data$MB_temp)  # Remove 'GB/sec' values (set to NA)
data$MB_temp <- as.numeric(gsub("MB/sec", "", data$MB_temp)) / 1024  # Convert 'MB' to GB

data$Memory_Bandwidth <- ifelse(!is.na(data$GB_temp), data$GB_temp, data$MB_temp)

#View(data)
data$GB_temp <- NULL # clear temp column
data$MB_temp <- NULL


data$Memory_Bus <- gsub("Bit", "", data$Memory_Bus)
data$Memory_Bus <- as.numeric(data$Memory_Bus)


data$Memory_Speed <- gsub("MHz", "", data$Memory_Speed)
data$Memory_Speed <- as.numeric(data$Memory_Speed)


data$Pixel_Rate <- gsub("GPixel/s", "", data$Pixel_Rate)
data$Pixel_Rate <- as.numeric(data$Pixel_Rate)


data$Process <- gsub("nm", "", data$Process)
data$Process <- as.numeric(data$Process)


data$ROPs <- gsub("\\(x[0-9]+\\)", "", data$ROPs)
data$ROPs <- as.numeric(data$ROPs)


data$TMUs <- as.numeric(data$TMUs)


data$Texture_Rate <- gsub("GTexel/s", "", data$Texture_Rate)
data$Texture_Rate <- as.numeric(data$Texture_Rate)


# remove same value row
data <- data %>% dplyr::distinct()











#View(data)


# Mean
aver <- apply(data, 2, mean, na.rm = TRUE)

# Standard deviation (SD)
s <- apply(data, 2, sd, na.rm = TRUE)

# Quantiles (Q1, Q2, Q3)
Q1 <- apply(data, 2, function(x) quantile(x, probs = 0.25, na.rm = TRUE))
Q2 <- apply(data, 2, function(x) quantile(x, probs = 0.5, na.rm = TRUE)) # Median
Q3 <- apply(data, 2, function(x) quantile(x, probs = 0.75, na.rm = TRUE))

# Min and Max
min_val <- apply(data, 2, min, na.rm = TRUE)
max_val <- apply(data, 2, max, na.rm = TRUE)

# Create a data frame
result <- round(data.frame(aver, s, Q1, Q2, Q3, min_val, max_val), 2)
print(result)
















# sample hist test
#windows()
#hist(data$Core_Speed, main = "Core speed histogram", xlab = "MHz", ylab = "Frequency", xlim = c(0, 2000), ylim = c(0, 2000), col = "blue", border = "black", label= TRUE)

windowed_hist <- function(data, name, xlab, save){
if(save == 1){
jpeg(
paste( gsub(" ", "_", name), "_histogram.jpg" ),
width = 800,
height = 600
)
}

data <- na.omit(data)  # Remove NA values from the data
  

if(length(data) == 0) {
stop("The data is empty or contains only NA values.")
}
  
# Define custom x-axis breaks
x_axis <- seq(floor(min(data)), max(data) * 1.1, by = floor((max(data) * 1.1 - min(data)) / 20))
  
# precompute hist
hist_data <- hist(
data, 
plot = FALSE,  # Don't plot yet, just calculate the data
breaks = x_axis
)

# Compute dynamic y-axis limit (max y value * 1.1)
max_y <- max(hist_data$counts)
ylim_values <- c(0, max_y * 1.1)
  
# Plot the histogram with custom x-axis breaks and dynamic ylim
hist(
data, 
main = paste(name, "Histogram"),  # Title dynamically changes based on 'name'
xlab = xlab,                      # Label for the x-axis (provided by user)
xaxt = "n",                       # Remove the default x-axis
ylab = "Amount",                  # Label for the y-axis
breaks = hist_data$breaks,        # Set custom breaks for histogram bins
col = "lightblue",                # Color for the histogram bars
border = "black",                 # Color for the border of the bars
ylim = ylim_values,               # Set dynamic ylim based on max y value
label = TRUE                      # Display bar labels
)

# Custom x-axis based on x_axis defined earlier
axis(1, at = hist_data$breaks)

# If saving the plot, close the graphics device
if(save == 1){
dev.off()  # Turn off the graphic device (close the jpeg file)
cat("Histogram saved as", paste(gsub(" ", "_", name), "_histogram.jpg"), "\n")
}
}












png("processor_group_histogram.png", width = 1600, height = 1200, res = 150)
par(mfrow = c(3, 1))
windowed_hist(data$Core_Speed, "Core Speed", "MHz", 0)
windowed_hist(data$Process, "Process", "nm", 0)
windowed_hist(data$Max_Power, "Max power", "Watts", 0)
dev.off()
cat("Histogram saved as", "processor_group_histogram.png", "\n")




png("memory_group_histogram.png", width = 1600, height = 1200, res = 120)
par(mfrow = c(2, 2))
windowed_hist(data$Memory, "Memory", "MB", 0)
windowed_hist(data$Memory_Bandwidth, "Memory bandwidth", "GB/sec", 0)
windowed_hist(data$Memory_Bus, "Memory bus", "Bit", 0)
windowed_hist(data$Memory_Speed, "Memory speed", "MHz", 0)
dev.off()
cat("Histogram saved as", "memory_group_histogram.png", "\n")




png("texture_process_group_histogram.png", width = 1600, height = 1200, res = 120)
par(mfrow = c(2, 2))
windowed_hist(data$Pixel_Rate, "Pixel rate", "GPixel/s", 0)
windowed_hist(data$ROPs, "ROPs", "ROPs count", 0)
windowed_hist(data$TMUs, "TMUs", "TMUs count", 0)
windowed_hist(data$Texture_Rate, "Texture rate", "GTexel/s", 0)
dev.off()
cat("Histogram saved as", "texture_process_group_histogram.png", "\n")








# bell curve draw
windowed_hist <- function(data, name, xlab, save){
data <- na.omit(data)  # Remove NA values from the data

if(length(data) == 0) {
stop("The data is empty or contains only NA values.")
}

# Define custom x-axis breaks
x_axis <- seq(floor(min(data)), max(data) * 1.1, by = floor((max(data) * 1.1 - min(data)) / 20))

# Plot the histogram with custom x-axis breaks
hist(data, 
probability = TRUE,          # Set y-axis to probability density (scaled)
main = paste(name, "Histogram"), # Title dynamically changes based on 'name'
xlab = xlab,                # Label for the x-axis (provided by user)
xaxt = "n",                 # Remove the default x-axis
ylab = "%",            # Label for the y-axis
breaks = x_axis,            # Set custom breaks for histogram bins
col = "lightblue",          # Color for the histogram bars
border = "black",           # Color for the border of the bars
)

# Custom x-axis based on x_axis defined earlier
axis(1, at = x_axis)

# Add a bell-shaped curve (normal distribution) to the histogram
curve(
dnorm(x, mean = mean(data, na.rm = TRUE), sd = sd(data, na.rm = TRUE)), 
col = "red",             # Set curve color
lwd = 2,                 # Set line width
add = TRUE               # Add the curve to the existing plot (histogram)
)

}




png("bell_processor_group_histogram.png", width = 1600, height = 1200, res = 150)
par(mfrow = c(3, 1))
windowed_hist(data$Core_Speed, "Core Speed", "MHz", 0)
windowed_hist(data$Process, "Process", "nm", 0)
windowed_hist(data$Max_Power, "Max power", "Watts", 0)
dev.off()
cat("Histogram saved as", "bell_processor_group_histogram.png", "\n")




png("bell_memory_group_histogram.png", width = 1600, height = 1200, res = 120)
par(mfrow = c(2, 2))
windowed_hist(data$Memory, "Memory", "MB", 0)
windowed_hist(data$Memory_Bandwidth, "Memory bandwidth", "GB/sec", 0)
windowed_hist(data$Memory_Bus, "Memory bus", "Bit", 0)
windowed_hist(data$Memory_Speed, "Memory speed", "MHz", 0)
dev.off()
cat("Histogram saved as", "bell_memory_group_histogram.png", "\n")



png("bell_texture_process_group_histogram.png", width = 1600, height = 1200, res = 120)
par(mfrow = c(2, 2))
windowed_hist(data$Pixel_Rate, "Pixel rate", "GPixel/s", 0)
windowed_hist(data$ROPs, "ROPs", "ROPs count", 0)
windowed_hist(data$TMUs, "TMUs", "TMUs count", 0)
windowed_hist(data$Texture_Rate, "Texture rate", "GTexel/s", 0)
dev.off()
cat("Histogram saved as", "bell_texture_process_group_histogram.png", "\n")








# overview
data_numeric <- data[, sapply(data, is.numeric)]
data_clean <- na.omit(data_numeric)

#windows()
png("scatterplot_of_all_value.png", width = 1600, height = 1200, res = 120)
pairs(data_clean, main = "Pairs Plot of Numeric Data", pch = 1) # pch 20 is filled circle, 1 is hollow circle
dev.off()
cat("Histogram saved as", "scatterplot_of_all_value.png", "\n")






png("scatterplot_core_speed_memory_speed.png", width = 1600, height = 1200, res = 300)
                               
plot(
data$Memory_Speed,
data$Core_Speed,
main = "Core speed vs Memory speed",
xlab = "Memory speed (MHz)",                   # x-axis label
ylab = "Core Speed (MHz)",          # y-axis label
pch = 20,                                    # Solid circle
col = rgb(0, 0, 0, 0.5),                     # Blue with transparency
cex = 1.2                                    # Point size
)




# Fit a linear model for the relationship between Core Speed and Memory Speed
model <- lm(Core_Speed ~ Memory_Speed, data = data)

# Add the regression line to the plot
abline(model, col = "red", lwd = 2)  # Red line, width 2
abline(model, col = rgb(0.5, 0.5, 0.5, 0.5), lwd = 40)  # Transparent gray, thicker line

dev.off()

cat("Scatter plot saved as 'scatterplot_core_speed_memory_speed.png' \n")



png("scatterplot_memory_bandwidth_texture_rate.png", width = 1600, height = 1200, res = 300)
                               
# Create the scatter plot
plot(
  data$Memory_Bandwidth,                  # X-axis data: Memory Bandwidth
  data$Texture_Rate,                      # Y-axis data: Texture Rate
  main = "Memory Bandwidth vs Texture Rate",  # Title
  xlab = "Memory Bandwidth (GB/sec)",        # X-axis label
  ylab = "Texture Rate (GTexel/s)",          # Y-axis label
  pch = 20,                                # Solid circle
  col = rgb(0, 0, 0, 0.5),                 # Blue with transparency
  cex = 1.2                                # Point size
)

# Fit a linear model for the relationship between Memory Bandwidth and Texture Rate
model <- lm(Texture_Rate ~ Memory_Bandwidth, data = data)

# Add the regression line to the plot
abline(model, col = "red", lwd = 2)  # Red line, width 2

# Add a thicker, transparent gray line on top
abline(model, col = rgb(0.5, 0.5, 0.5, 0.5), lwd = 40)  # Transparent gray, thicker line

dev.off()

cat("Scatter plot saved as 'scatterplot_memory_bandwidth_texture_rate.png' \n")




png("scatterplot_memory_bandwidth_max_power.png", width = 1600, height = 1200, res = 300)

# Create the scatter plot
plot(
  data$Memory_Bandwidth,                  # X-axis data: Memory Bandwidth
  data$Max_Power,                         # Y-axis data: Max Power (Watts)
  main = "Memory Bandwidth vs Max Power",  # Title
  xlab = "Memory Bandwidth (GB/sec)",        # X-axis label
  ylab = "Max Power (Watts)",                # Y-axis label
  pch = 20,                                # Solid circle
  col = rgb(0, 0, 0, 0.5),                 # Blue with transparency
  cex = 1.2                                # Point size
)

# Fit a linear model for the relationship between Memory Bandwidth and Max Power
model <- lm(Max_Power ~ Memory_Bandwidth, data = data)

# Add the regression line to the plot
abline(model, col = "red", lwd = 2)  # Red line, width 2

# Add a thicker, transparent gray line on top
abline(model, col = rgb(0.5, 0.5, 0.5, 0.5), lwd = 40)  # Transparent gray, thicker line

dev.off()

cat("Scatter plot saved as 'scatterplot_memory_bandwidth_max_power.png' \n")



########################################################
# Tính toán IQR cho từng cột số trong new_DF
IQR_values <- sapply(new_DF[sapply(new_DF, is.numeric)], IQR)

# Tính toán giới hạn dưới (lower bounds) và giới hạn trên (upper bounds) cho các cột số
lower_bounds <- sapply(new_DF[sapply(new_DF, is.numeric)], quantile, probs = 0.25) - 1.5 * IQR_values
upper_bounds <- sapply(new_DF[sapply(new_DF, is.numeric)], quantile, probs = 0.75) + 1.5 * IQR_values

# Xác định các điểm ngoại lai (outliers)
outliers <- lapply(names(new_DF), function(i) {
  if (is.numeric(new_DF[[i]])) {
    new_DF[[i]] < lower_bounds[i] | new_DF[[i]] > upper_bounds[i]
  } else {
    rep(FALSE, length(new_DF[[i]]))
  }
})

# In ra số lượng các điểm ngoại lai cho từng biến
outlier_counts <- sapply(outliers, sum)
print(outlier_counts)

########################################################
# Tính toán IQR cho từng cột số trong new_DF
IQR_values <- sapply(new_DF[sapply(new_DF, is.numeric)], IQR)

# Tính toán giới hạn dưới (lower bounds) và giới hạn trên (upper bounds) cho các cột số
lower_bounds <- sapply(new_DF[sapply(new_DF, is.numeric)], quantile, probs = 0.25) - 1.5 * IQR_values
upper_bounds <- sapply(new_DF[sapply(new_DF, is.numeric)], quantile, probs = 0.75) + 1.5 * IQR_values

# Xác định các điểm ngoại lai (outliers)
outliers <- lapply(names(new_DF), function(i) {
  if (is.numeric(new_DF[[i]])) {
    new_DF[[i]] < lower_bounds[i] | new_DF[[i]] > upper_bounds[i]
  } else {
    rep(FALSE, length(new_DF[[i]]))
  }
})

# In ra số lượng các điểm ngoại lai cho từng biến
outlier_counts <- sapply(outliers, sum)
print(outlier_counts)

########################################################
# Tính toán IQR cho từng cột số trong new_DF
IQR_values <- sapply(new_DF[sapply(new_DF, is.numeric)], IQR)

# Tính toán giới hạn dưới (lower bounds) và giới hạn trên (upper bounds) cho các cột số
lower_bounds <- sapply(new_DF[sapply(new_DF, is.numeric)], quantile, probs = 0.25) - 1.5 * IQR_values
upper_bounds <- sapply(new_DF[sapply(new_DF, is.numeric)], quantile, probs = 0.75) + 1.5 * IQR_values

# Xác định các điểm ngoại lai (outliers)
outliers <- lapply(names(new_DF), function(i) {
  if (is.numeric(new_DF[[i]])) {
    new_DF[[i]] < lower_bounds[i] | new_DF[[i]] > upper_bounds[i]
  } else {
    rep(FALSE, length(new_DF[[i]]))
  }
})
names(outliers) <- names(new_DF)

# In ra số lượng các điểm ngoại lai cho từng biến
outlier_counts <- sapply(outliers, sum)
print(outlier_counts)

# Tạo dataframe Boolean cho từng hàng: TRUE nếu hàng đó có ít nhất một ngoại lai
outlier_matrix <- as.data.frame(outliers)
rows_with_outliers <- apply(outlier_matrix, 1, any)

# Vẽ boxplot cho tất cả các biến số trong new_DF
boxplot(new_DF[sapply(new_DF, is.numeric)], main = "Boxplot of Numeric Variables", col = "lightblue", border = "black")

# Nếu muốn vẽ boxplot riêng biệt cho từng biến
for (col in names(new_DF[sapply(new_DF, is.numeric)])) {
  boxplot(new_DF[[col]], main = paste("Boxplot of", col), col = "lightgreen", border = "black")
}














