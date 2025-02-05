library(lidR)
library(mapview)
library(sf)
library(RCSF)

# Function to format time in hours, minutes, and seconds
format_time <- function(time) {
  seconds <- as.numeric(time, units = "secs")
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  remaining_seconds <- seconds %% 60
  return(sprintf("%02d:%02d:%02.1f", hours, minutes, remaining_seconds))
}

# Function to process a .las file and export the results to CSV
process_las_file <- function(las_file) {
  start_time <- Sys.time()  # Start time of processing
  
  # Read the .las file
  lidar_raw <- readLAS(las_file)
  
  # Noise cleaning
  lidar <- classify_noise(lidar_raw, ivf(5, 2))
  
  # Ground points classification using CSF
  mycsf <- csf(sloop_smooth = FALSE, class_threshold = 0.1, cloth_resolution = 0.5, rigidness = 3)
  lidar <- classify_ground(lidar, mycsf)
  
  # View ground points in 3D
  ground_points <- filter_ground(lidar)
  
  # Normalization excluding noise points and 3D plot
  lidar_norm <- normalize_height(lidar, knnidw())
  
  # Calculate Canopy Height Model (if needed, uncomment this part)
  # chm <- rasterize_canopy(lidar_norm, 0.5, pitfree(subcircle = 0.2))
  
  # Variable window sizes
  f <- function(x) { x * 0.1 + 3 }
  heights <- seq(0, 30, 5)
  ws <- f(heights)
  
  # Adjust parameters to improve tree detection
  treetops <- locate_trees(lidar_norm, lmf(ws = 4))
  
  # Export
  df_treetops <- as.data.frame(treetops)
  csv_filename <- sub("\\.las$", ".csv", basename(las_file))
  write.csv2(df_treetops, file = paste0("C:\\USERDIRECTORY\\testitera\\", csv_filename))
  
  # End time and calculate the difference for this file
  end_time <- Sys.time()
  processing_time <- end_time - start_time
  
  # Write processing time information to the log.txt file
  formatted_processing_time <- format_time(processing_time)
  write(paste("File:", csv_filename, "- Processing time:", formatted_processing_time), 
        file = "C:\\USERDIRECTORY\\testitera\\log.txt", append = TRUE)
  write("\n", file = "C:\\USERDIRECTORY\\testitera\\log.txt", append = TRUE) # New line
  
  return(processing_time)  # Return the processing time for this file
}

# Directory where the .las files are located
directory <- "C:\\USERDIRECTORY\\testitera\\"

# Get the list of .las files in the directory
las_files <- list.files(directory, pattern = "\\.las$", full.names = TRUE)

# Initialize total processing time
total_time <- 0

# Loop to process each .las file
for (file in las_files) {
  file_processing_time <- process_las_file(file)
  total_time <- total_time + file_processing_time
}

# Directory where the exported CSV files are located
export_directory <- "C:\\USERDIRECTORY\\testitera\\"

# Get the list of CSV files in the export directory
csv_files <- list.files(export_directory, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store data from all CSV files
data_list <- list()

# Loop to load and combine all CSV files into one
for (csv_file in csv_files) {
  csv_data <- read.csv2(csv_file)
  data_list <- append(data_list, list(csv_data))
}

# Combine all data into a single data frame
total_data <- do.call(rbind, data_list)

# Export the total data to a CSV file
write.csv2(total_data, file = "C:\\USERDIRECTORY\\testitera\\all_the_data.csv")

# Write the total processing time to the log.txt file
formatted_total_time <- format_time(total_time)
write(paste("Total processing time for all files:", formatted_total_time), 
      file = "C:\\USERDIRECTORY\\testitera\\log.txt", append = TRUE)

# Print completion message
cat("All data has been combined and exported into a single CSV file.\n")
