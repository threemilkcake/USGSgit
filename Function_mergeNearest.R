

# converting 'dateTime' columns to POSIXct to handle time differences
chlA_LO$dateTime <- as.POSIXct(chlA_LO$dateTime)
fCHL_LO$dateTime <- as.POSIXct(fCHL_LO$dateTime)

# Define maximum allowed time difference in this case 10 min
max_diff <- as.difftime(15, units = "mins")

# Create an empty list to store merged rows
merged_rows <- list()

# Loop over each row in chlA_LO
for (i in 1:nrow(chlA_LO)) {
  
  # Get the current time in the left dataset
  time_left <- chlA_LO$dateTime[i]
  
  # Calculate the absolute time differences with all rows in the right dataset
  time_diffs <- abs(fCHL_LO$dateTime - time_left)
  
  # Find the row in the right dataset with the smallest time difference
  min_diff <- min(time_diffs)
  
  # If the smallest difference is within the allowed limit, merge the rows
  if (min_diff <= max_diff) {
    # Get the index of the closest time
    closest_index <- which.min(time_diffs)
    
    # Combine the two rows
    merged_row <- cbind(chlA_LO[i, ], fCHL_LO[closest_index, ])
    
    # Append the merged row to the list
    merged_rows[[i]] <- merged_row
  }
}

# Combine the list of merged rows into a data frame
result <- do.call(rbind, merged_rows)

View(result)
