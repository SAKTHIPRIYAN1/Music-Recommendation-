# ==========================
# Load Required Libraries
# ==========================
library(recommenderlab)
library(reshape2)
library(dplyr)

# ==========================
# Step 1: Load the Dataset
# ==========================
data <- read.csv("merged_data.csv")

# Select relevant columns
ratings <- data %>% select(user_id, sid, rating)


song_lookup <- data %>% select(sid, name) %>% distinct()

# Ensure rating is numeric
ratings$rating <- as.numeric(ratings$rating)

# ==========================
# Step 2: Convert to User-Item Matrix
# ==========================
# Convert to wide format (user-item matrix)
rating_matrix <- dcast(ratings, user_id ~ sid, value.var = "rating")

# Save user_id as row names and remove from matrix
user_ids <- rating_matrix$user_id
rating_matrix <- as.matrix(rating_matrix[,-1])
rownames(rating_matrix) <- user_ids

# Convert to realRatingMatrix
rrm <- as(rating_matrix, "realRatingMatrix")

# ✅ Print the rating matrix (subset for readability)
cat("🎵 Rating Matrix (first few rows):\n")
print(as(rrm, "matrix")[1:5, 1:5])  # Display a subset of the matrix

# ==========================
# Step 3: Train or Load UBCF Model
# ==========================
if (file.exists("ubcf_model.rds")) {
  ubcf_model <- readRDS("ubcf_model.rds")
  cat("✅ Loaded existing UBCF model.\n")
} else {
  ubcf_model <- Recommender(rrm, method = "UBCF", parameter = list(nn = 30, method = "cosine"))
  saveRDS(ubcf_model, "ubcf_model.rds")
  cat("✅ Trained and saved new UBCF model.\n")
}

# ==========================
# Step 4: Train or Load IBCF Model
# ==========================
if (file.exists("ibcf_model.rds")) {
  ibcf_model <- readRDS("ibcf_model.rds")
  cat("✅ Loaded existing IBCF model.\n")
} else {
  ibcf_model <- Recommender(rrm, method = "IBCF", parameter = list(k = 30))
  saveRDS(ibcf_model, "ibcf_model.rds")
  cat("✅ Trained and saved new IBCF model.\n")
}


# Function to get the name of a song given its sid
get_song_name <- function(sid, song_lookup) {
  # Filter the song_lookup table for the given sid
  song_name <- song_lookup %>% filter(sid == !!sid) %>% select(name) %>% pull()
  
  # Return the song name or a default message if not found
  if (length(song_name) == 0) {
    return("Unknown Song")
  }
  return(song_name)
}


# ==========================
# Step 5: Recommendation Functions
# ==========================

# Function to get fallback recommendations
get_fallback_recommendations <- function(user_id, n = 10) {
  user_index <- which(rownames(rrm) == user_id)
  user_ratings <- as(rrm[user_index, ], "matrix")

  # Get the IDs of songs the user has rated
  rated_songs <- colnames(user_ratings)[!is.na(user_ratings)]

  if (length(rated_songs) == 0) {
    # If the user has no ratings, return globally popular songs
    popular_songs <- colnames(rrm)[order(colCounts(rrm), decreasing = TRUE)[1:n]]
    return(popular_songs)
  }

  # Use item-based collaborative filtering to find similar songs
  predicted <- predict(ibcf_model, rrm[user_index, ], n = n)
  fallback_items <- as(predicted, "list")[[1]]

  if (length(fallback_items) == 0) {
    # If no fallback recommendations are found, return globally popular songs
    popular_songs <- colnames(rrm)[order(colCounts(rrm), decreasing = TRUE)[1:n]]
    return(popular_songs)
  }

  return(fallback_items)
}

# Function to get recommendations for a user
# Function to get recommendations for a user
get_recommendations <- function(user_id, n = 10) {
  if (!(user_id %in% rownames(rrm))) {
    return(data.frame("Error" = paste("User ID", user_id, "not found in the dataset.")))
  }
  
  # Predict top N recommendations using UBCF
  predicted <- predict(ubcf_model, rrm[user_id, ], n = n)
  recommended_items <- as(predicted, "list")[[1]]
  
  if (length(recommended_items) == 0) {
    return(data.frame("Error" = "No recommendations available for this user."))
  }
  
  # Retrieve song names and IDs for the recommended items
  recommendations <- data %>%
    filter(sid %in% recommended_items) %>%
    select(sid, name) %>%
    distinct()
  
  return(recommendations)  # Return both SID and Name
}

# Loop through user IDs and return recommendations with song names and SIDs
get_all_recommendations <- function(n_users = 200, n_recommendations = 10) {
  all_recommendations <- list()  # Store recommendations for all users
  
  for (i in 1:n_users) {
    user_id <- paste0("U", i)  # Generate user ID (e.g., U1, U2, ..., U200)
    
    # Get recommendations for the user
    recommendations <- get_recommendations(user_id, n = n_recommendations)
    
    if (!is.null(recommendations) && nrow(recommendations) > 0) {
      all_recommendations[[user_id]] <- recommendations  # Store recommendations in the list
    } else {
      all_recommendations[[user_id]] <- data.frame("Error" = "No recommendations available")
    }
  }
  
  return(all_recommendations)  # Return the list of recommendations
}

# ==========================
# Step 6: Generate Recommendations for All Users
# ==========================
# Loop through user IDs from U1 to U200
# for (i in 1:200) {
#   user_id <- paste0("U", i)  # Generate user ID (e.g., U1, U2, ..., U200)
#   cat("\n============================\n")
#   cat("🎧 Recommendations for User:", user_id, "\n")
#   cat("============================\n")
#   get_recommendations(user_id, n = 10)
# }

