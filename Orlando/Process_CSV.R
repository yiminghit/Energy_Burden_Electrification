# Install and load the "readr" package for reading CSV files


# Clear all variables from the global environment
rm(list = ls())
library(readr)

city_name <- "Orlando"
routine1 <- "D:/Umich_Project/Experiments/"

# Specify the file paths
file_path1 <- paste0(routine1, city_name, "/ACSST1Y2021.S2503-2023-06-26T003026.csv")
file_path2 <- paste0(routine1, city_name, "/AHS TABLE3 6_25_2023.csv")
file_path3 <- paste0(routine1, city_name, "/AHS TABLE2 6_25_2023.csv")


# Read the CSV file into a data frame
df1 <- read_csv(file_path1)

# Read the CSV file into a data frame
df2 <- read_csv(file_path2)
df3 <- read_csv(file_path3)

# Display the structure of the data frame
subset_df1 <- df1[3:13, 1:2]
subset_df2 <- df2[c( 67, 69,72), 1:2]
subset_df3 <- df3[48:56, 1:2]


names(subset_df1) <- c("income", "number")

names(subset_df2) <- c("type", "number")

names(subset_df3) <- c("Room Size", "number")
subset_df3$number <- as.numeric(subset_df3$number)

agg_number1 <- subset_df1$number[1]+subset_df1$number[2]+subset_df1$number[3]

agg_number2 <- subset_df1$number[4]+subset_df1$number[5]+subset_df1$number[6]

agg_number3 <- subset_df1$number[7]+subset_df1$number[8]

agg_number4 <- subset_df1$number[9]

agg_number5 <- subset_df1$number[10] + subset_df1$number[11]

# Create a data frame for aggregated numbers
agg_df <- data.frame(
  income = c("<15k", "15k-34999", "35k-74999", "75k-99999", ">100k"),
  number = c(agg_number1, agg_number2, agg_number3, agg_number4, agg_number5)
)

total <- sum(agg_df$number)

# Calculate percentages
agg_df$percentage <- (agg_df$number / total) 

agg2_number1 <- as.numeric(subset_df2$number[1])

agg2_number2 <- as.numeric(subset_df2$number[2])

agg2_number3 <- as.numeric(subset_df2$number[3])

agg_df2 <- data.frame(
  Type_Room = c("Central", "Room", "None"),
  number = c(agg2_number1, agg2_number2, agg2_number3)
)

total2 <- sum(agg_df2$number)

# Calculate percentages
agg_df2$percentage <- (agg_df2$number / total2) 

# Display the subset of the data frame

agg3_number1 <- subset_df3$number[1]+subset_df3$number[2]+subset_df3$number[3]+subset_df3$number[4]

agg3_number2 <- subset_df3$number[5]+subset_df3$number[6]

agg3_number3 <- subset_df3$number[7]+0.5*subset_df3$number[8]

agg3_number4 <- 0.5*subset_df3$number[8]+subset_df3$number[9]

agg_df3 <- data.frame(
  Room_Size = c("0-1499", "1500-2499", "2500-3499", "3500+"),
  number = c(agg3_number1, agg3_number2, agg3_number3,agg3_number4)
)

total3 <- sum(agg_df3$number)

# Calculate percentages
agg_df3$percentage <- (agg_df3$number / total3) 
#income
print(paste(paste("'",agg_df$income,"'",":", agg_df$percentage, sep = ""), collapse = ", "))
#print AC type
print(paste(paste("'",agg_df2$Type_Room,"'",":", agg_df2$percentage, sep = ""), collapse = ", "))
#print Room Size
print(paste(paste("'",agg_df3$Room_Size,"'",":", agg_df3$percentage, sep = ""), collapse = ", "))