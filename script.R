# Load the CSV file
df <- read.csv("merged_data.csv", stringsAsFactors = FALSE)

# Convert the 'rating' column from string to numeric
df$rating <- as.numeric(df$rating)

# Check the result
str(df)

# Optionally, save the updated CSV
write.csv(df, "rating_numeric.csv", row.names = FALSE)
