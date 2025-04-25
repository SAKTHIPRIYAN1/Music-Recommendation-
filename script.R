# Load necessary libraries
library(dplyr)

# Step 1: Load the song data (metadata)
song_data <- read.csv("data_2.csv")

# Step 2: Generate random user IDs (U1 to U10)
user_ids <- paste("U", 1:10, sep = "")

# Step 3: Select the first 50 songs (sid from s1 to s50)
song_data_subset <- song_data %>% filter(sid %in% paste("s", 1:50, sep = ""))

# Step 4: Create a dataframe to store the ratings
ratings <- data.frame(user_id = character(), sid = character(), rating = numeric())

# Step 5: Generate random ratings (each user rates only 12 out of 50 songs)
set.seed(123)  # For reproducibility

for (user in user_ids) {
  # Randomly choose 12 songs for this user to rate
  rated_songs <- sample(song_data_subset$sid, 12)
  
  for (song in rated_songs) {
    # Generate a random rating between 1 and 5
    rating <- sample(1:5, 1)
    
    # Append the user, song, and rating to the ratings dataframe
    ratings <- rbind(ratings, data.frame(user_id = user, sid = song, rating = rating))
  }
}

# Step 6: Merge the ratings with the song metadata
merged_data <- ratings %>%
  left_join(song_data_subset, by = "sid")

# Step 7: Add a random play_count (just for the sake of having it in the merged data)
merged_data$play_count <- sample(1:100, nrow(merged_data), replace = TRUE)

# Step 8: Save the merged data to a CSV file
write.csv(merged_data, "merged_data.csv", row.names = FALSE)

# Check the first few rows of the merged data
head(merged_data)
