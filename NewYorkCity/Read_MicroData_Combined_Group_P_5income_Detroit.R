# Load the required library
library(readr)

city_name <- "NewYorkCity"
routine1 <- "D:/Umich_Project/Experiments/"

# Specify the file paths
file_path1 <- paste0(routine1, city_name, "/NewYorkCity_income_ini.rds")
# Read the CSV file
data <- read_csv("C:/Users/yimin/Dropbox/RCM_June/2017_nationaldata.csv")
region_code <- "'35620'"
#12580 baltimore
#42660 Seattle
#C:/Users/yangl11/R_Project/ACS_DATA/MicroData
# Specify the columns to extract
columns_to_extract <- c("UNITSIZE", "HEATFUEL", "YRBUILT", "ACPRIMARY", "TENURE", "OMB13CBSA", "HINCP")

# Extract the specified columns from the data
extracted_data <- data[, columns_to_extract]

# Filter rows where OMB13CBSA is equal to 36420
filtered_data <- extracted_data[extracted_data$OMB13CBSA == region_code, ]

# Remove rows where HINCP is less than 0
filtered_data <- filtered_data[filtered_data$HINCP >= 0, ]

#remove all the entry with -9
filtered_data <- filtered_data[filtered_data$UNITSIZE !="'-9'", ]
filtered_data <- filtered_data[filtered_data$TENURE !="'3'", ]

# Change YRBUILT values
filtered_data$YRBUILT[filtered_data$YRBUILT >= 2010] <- "2010 or later"
filtered_data$YRBUILT[filtered_data$YRBUILT >= 2000 & filtered_data$YRBUILT <= 2009] <- "2000-2009"
filtered_data$YRBUILT[filtered_data$YRBUILT >= 1990 & filtered_data$YRBUILT <= 1999] <- "1990-1999"
filtered_data$YRBUILT[filtered_data$YRBUILT >= 1980 & filtered_data$YRBUILT <= 1989] <- "1980-1989"
filtered_data$YRBUILT[filtered_data$YRBUILT >= 1970 & filtered_data$YRBUILT <= 1979] <- "1970-1979"
filtered_data$YRBUILT[filtered_data$YRBUILT >= 1960 & filtered_data$YRBUILT <= 1969] <- "1960-1969"
filtered_data$YRBUILT[filtered_data$YRBUILT >= 1950 & filtered_data$YRBUILT <= 1959] <- "1950-1959"
filtered_data$YRBUILT[filtered_data$YRBUILT >= 1940 & filtered_data$YRBUILT <= 1949] <- "1940-1949"
filtered_data$YRBUILT[filtered_data$YRBUILT <= 1939] <- "1939 or ealier"


filtered_data$HINCP <- as.numeric(filtered_data$HINCP)  

filtered_data$HINCP[as.numeric(filtered_data$HINCP) >= 100000 ] <- 'Total >100k'
filtered_data$HINCP[as.numeric(filtered_data$HINCP) >= 75000 & as.numeric(filtered_data$HINCP) < 100000 ] <- 'Total 75k-99999'
filtered_data$HINCP[as.numeric(filtered_data$HINCP) >= 35000 & as.numeric(filtered_data$HINCP) < 75000  ] <- 'Total 35k-74999'
filtered_data$HINCP[as.numeric(filtered_data$HINCP) >= 15000 & as.numeric(filtered_data$HINCP) < 35000 ] <- 'Total 15k-34999'
filtered_data$HINCP[as.numeric(filtered_data$HINCP) < 15000 ] <-'Total <15k'

filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'1'"] <- "0-1499"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'2'"] <- "0-1499"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'3'"] <- "0-1499"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'4'"] <- "0-1499"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'5'"] <- "1500-2499"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'6'"] <- "1500-2499"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'7'"] <- "2500-3499"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'8'"] <- "3k-3999"
filtered_data$UNITSIZE[filtered_data$UNITSIZE == "'9'"] <- "3500+"

filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'01'"] <- "Electricity"
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'02'"] <- "Utility Gas"
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'03'"] <- "LP Gas"
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'04'"] <- "Fuel oil, kerosene, etc."
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'05'"] <- "Fuel oil, kerosene, etc."
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'06'"] <- "Coal of Coke"
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'07'"] <- "Wood"
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'08'"] <- "Solar"
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'09'"] <- "Other"
filtered_data$HEATFUEL[filtered_data$HEATFUEL == "'10'"] <- "No Fuel"

filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'01'"] <- "Central"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'02'"] <- "Central"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'03'"] <- "Central"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'04'"] <- "Central"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'05'"] <- "Room"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'06'"] <- "Room"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'07'"] <- "Room"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'08'"] <- "Room"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'09'"] <- "Room"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'10'"] <- "Room"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'11'"] <- "Room"
filtered_data$ACPRIMARY[filtered_data$ACPRIMARY == "'12'"] <- "None"

columns_to_extract <- c("UNITSIZE", "HEATFUEL", "YRBUILT", "ACPRIMARY", "HINCP")

# Extract the specified columns from the data
#filtered_data_final <- filtered_data[, columns_to_extract]
filtered_data_final <- filtered_data
#combination_freq <- xtabs(~ UNITSIZE + HEATFUEL + YRBUILT +ACPRIMARY + HINCP, data = filtered_data_final)

dimensions <- c(4, 3, 5)

# Create the tensor with all zeros
tensorM <- array(0, dim = dimensions)


set2 <- c( "0-1499","1500-2499","2500-3499","3500+")
set3 <- c("Central", "Room","None")
set5 <- c( 'Total <15k', 'Total 15k-34999', 'Total 35k-74999', 'Total 75k-99999', 'Total >100k')

ii=0
for (i in 1:nrow(filtered_data_final)) {
  # Iterate over each column in the current row
  
  # Access the current entry
  entry <- filtered_data_final[i, 1]
  
  
  if("3k-3999" == filtered_data_final[i, "UNITSIZE"]$UNITSIZE){
    ii <- ii+1
  }
}
iii=0
# Iterate over each row in filtered_data using nested loops
for (i in 1:nrow(filtered_data_final)) {
  # Iterate over each column in the current row
  
  # Access the current entry
  entry <- filtered_data_final[i, 1]
  
  if("3k-3999" == filtered_data_final[i, "UNITSIZE"]$UNITSIZE){
    if(iii<round(ii/2)){
      filtered_data_final[i, "UNITSIZE"]$UNITSIZE = "2500-3499"
    } else{
      filtered_data_final[i, "UNITSIZE"]$UNITSIZE = "3500+" 
    }
    iii <- iii+1
  } 
  i2 <- which(set2 == filtered_data_final[i, "UNITSIZE"]$UNITSIZE)
  i3 <- which(set3 == filtered_data_final[i, "ACPRIMARY"]$ACPRIMARY)
  i5 <- which(set5 == filtered_data_final[i, "HINCP"]$HINCP)
  tensorM[i2,i3,i5] <- tensorM[i2,i3,i5]+1
}
tensorM[tensorM == 1] =0
tensorM[tensorM == 2] =0
count_sum <-sum(tensorM)
count_2 <- sum(tensorM == 2)
CCC <- sum(tensorM)
for (i in 1:900) {
  count_i <- sum(tensorM == i)
  if (count_i>0){cat("Value:", i, "Count:", count_i, "\n")}
  
}
#tensorM <- tensorM/ nrow(filtered_data_final)
tensorM <- 0.9*tensorM/ sum(tensorM)
#tensorM[tensorM == 0] <- 0.00000000001


set.seed(42)  # For reproducibility
random_numbers <- runif(sum(tensorM == 0))  # Generating random numbers from uniform distribution
normalized_random_numbers <- 1* random_numbers / sum(random_numbers)
# Replace zero entries in tensorM with the generated random numbers
tensorM[tensorM == 0] <- 0.1*normalized_random_numbers

u_name <- list(
  c('0-1499', '1500-2499',  '2500-3499', '3500+'),
  c('Central', 'Room', 'None'),
  c('Total <15k', 'Total 15k-34999', 'Total 35k-74999', 'Total 75k-99999', 'Total >100k')
)


u_name1_list <- list()
u_name2_list <- list()
tensor_list1 <- list()
tensor_list2 <- list()
tensor_list3 <- list()
tensor_list4 <- list()
tensor_list5 <- list()


tensor4_sum_list <- list()

for (index1 in 1:dim(tensorM)[1]) {
  u_name1 <- u_name[[1]][index1]
  tensor1 <- tensorM[index1,,]
      for (index4 in 1:dim(tensorM)[2]) {
        u_name2 <- u_name[[2]][index4]
        tensor4 <- tensor1[index4,]
        
        u_name1_list[[length(u_name1_list) + 1]] <- u_name1
        u_name2_list[[length(u_name2_list) + 1]] <- u_name2
        tensor4[tensor4 < 0.00001] <- 0
        tensor_list1[[length(tensor_list1) + 1]] <- tensor4[1]
        tensor_list2[[length(tensor_list2) + 1]] <- tensor4[2]
        tensor_list3[[length(tensor_list3) + 1]] <- tensor4[3]
        tensor_list4[[length(tensor_list4) + 1]] <- tensor4[4]
        tensor_list5[[length(tensor_list5) + 1]] <- tensor4[5]

        tensor4_sum <- sum(tensor4)
        tensor4_sum_list[[length(tensor4_sum_list) + 1]] <- tensor4_sum
  }
}

# Convert list_final to a data frame
data_df <- data.frame(
  'House Size' = unlist(u_name1_list),
  'AC Type' = unlist(u_name2_list),
  '<15k'=unlist(tensor_list1), 
  '15k-34999'=unlist(tensor_list2), 
  '35k-74999'=unlist(tensor_list3), 
  '75k-99999'=unlist(tensor_list4), 
  '>100k'=unlist(tensor_list5), 
  'Sum' = unlist(tensor4_sum_list)
)

saveRDS(tensorM, file_path1)