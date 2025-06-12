# Load required libraries
library(ggplot2)
library(gridExtra)

# Define city and paths
city <- "Baltimore"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <- paste(routine_path, city, '/', sep="")

# Read the RDS files
loaded_data1 <- readRDS(file.path(output_path, "CHHW_one_year.rds"))
loaded_data2 <- readRDS(file.path(output_path, "FHHW_one_year.rds"))
loaded_data3 <- readRDS(file.path(output_path, "CHFW_one_year.rds"))
loaded_data4 <- readRDS(file.path(output_path, "FHFW_one_year.rds"))


file_path_top10<- file.path(output_path,  "Figures",  paste(city,"_top_10_box_1year.png", sep=""))

calculate_kde_cdf <- function(data) {
  # Calculate KDE
  kde <- density(data, bw = "bcv")
  
  # Normalize the density values
  norm_factor <- sum(kde$y * diff(kde$x))
  kde$y <- kde$y / norm_factor
  
  # Compute the CDF
  cdf <- cumsum(kde$y * diff(kde$x))
  
  return(list(kde = kde, cdf = cdf))
}

result_loaded_data1 <- (loaded_data1)
result_loaded_data2 <- (loaded_data3[['2050']]$rcp45cooler)
result_loaded_data3 <- (loaded_data3[['2050']]$rcp85hotter)

result_loaded_data4 <- (loaded_data2)
result_loaded_data5 <- (loaded_data4[['2050']]$rcp45cooler)
result_loaded_data6 <- (loaded_data4[['2050']]$rcp85hotter)

# Create a list to store individual boxplots
boxplot_list <- list()

# Combine all datasets into a single list
all_loaded_data <- list(
  result_loaded_data1,
  result_loaded_data2,
  result_loaded_data3,
  result_loaded_data4,
  result_loaded_data5,
  result_loaded_data6
)

name <- c("CHHW", "CHFW_cool", "CHFW_hot", "FHHW","FHFW_cool", "FHFW_hot")

# Create a list to store individual boxplots for the top 10%
top10_plot_list <- list()

# Loop through each scenario (1 to 6)
for (i in 1:6) {
  # Extract current dataset
  current_data <- all_loaded_data[[i]]
  
  # Calculate the 90th percentile
  quantile_value <- quantile(current_data, 0.9)
  
  # Filter the data to only include the top 10%
  top_10_data <- current_data[current_data > quantile_value]
  
  # Convert the top 10% data to a data frame
  boxplot_data <- data.frame(scenario = factor(name[i], levels = name), Value = top_10_data)
  
  median_value <- median(top_10_data)
  cat(paste("Scenario:", name[i], "- Top 10% Median Energy Burden:", median_value, "\n"))
  
  # Create a boxplot for the top 10% data and store it in the list
  top10_plot_list[[i]] <- ggplot(boxplot_data, aes(x = scenario, y = Value)) +
    geom_boxplot(fill = "red", color = "darkred") +
    #geom_boxplot(fill = "red", color = "darkred", outlier.shape = NA) +
    #ggtitle(paste(name[i], "Top 10%")) +
    xlab("") +
    ylab("Tail Energy Burden") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 0.8))
}


# Combine the top 10% boxplots into a single figure
top10_combined_plot <- grid.arrange(grobs = top10_plot_list, ncol = 6)  # Adjust ncol according to your preference

# Save the combined plot for the top 10% as a PNG with high resolution
ggsave(file_path_top10, plot = top10_combined_plot, width = 12, height = 4, dpi = 300)
