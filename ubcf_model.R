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

# Ensure ratings are numeric and check for missing ratings
data$rating <- as.numeric(data$rating)
sum(is.na(data$rating))  # Count missing ratings
data <- data %>% filter(!is.na(rating))  # Remove rows with missing ratings

# View user_id, sid, and rating for the first few rows to check
head(data %>% select(user_id, sid, rating))

# ==========================
# Step 2: Create User-Item Matrix
# ==========================
ratings <- data %>% select(user_id, sid, rating)
song_lookup <- data %>% select(sid, name) %>% distinct()

rating_matrix <- dcast(ratings, user_id ~ sid, value.var = "rating")
user_ids <- rating_matrix$user_id
rating_matrix <- as.matrix(rating_matrix[,-1])
rownames(rating_matrix) <- user_ids
rrm <- as(rating_matrix, "realRatingMatrix")

# Print out the rating matrix (first few rows and columns)
cat("Rating Matrix (first few rows and columns):\n")
print(as(rrm, "matrix")[1:9, 1:9])

# ==========================
# Step 3: Train or Load UBCF Model
# ==========================
if (file.exists("ubcf_model.rds")) {
  ubcf_model <- readRDS("ubcf_model.rds")
  cat("Loaded existing UBCF model.\n")
} else {
  ubcf_model <- Recommender(rrm, method = "UBCF", parameter = list(nn = 30, method = "cosine"))
  saveRDS(ubcf_model, "ubcf_model.rds")
  cat("Trained and saved new UBCF model.\n")
}

# ==========================
# Step 4: Train or Load IBCF Model
# ==========================
if (file.exists("ibcf_modelU.rds")) {
  ibcf_model <- readRDS("ibcf_modelU.rds")
  cat("Loaded existing IBCF model.\n")
} else {
  ibcf_model <- Recommender(rrm, method = "IBCF", parameter = list(k = 30))
  saveRDS(ibcf_model, "ibcf_modelU.rds")
  cat("Trained and saved new IBCF model.\n")
}

# ==========================
# Helper Function: Get Song Name by SID
# ==========================
get_song_name_by_sid <- function(sid) {
  song_name <- song_lookup %>% filter(sid == !!sid) %>% pull(name)
  if (length(song_name) == 0) return("Unknown Song")
  return(song_name)
}

# ==========================
# IBCF Fallback Function
# ==========================
get_ibcf_fallback_recommendations <- function(user_id, n = 10) {
  user_index <- which(rownames(rrm) == user_id)
  user_ratings <- as(rrm[user_index, ], "matrix")
  rated_songs <- colnames(user_ratings)[!is.na(user_ratings)]

  if (length(rated_songs) == 0) {
    cat("No ratings found for", user_id, "- using top popular songs.\n")
    return(colnames(rrm)[order(colCounts(rrm), decreasing = TRUE)[1:n]])
  }

  predicted <- predict(ibcf_model, rrm[user_index, ], n = n)
  fallback_items <- as(predicted, "list")[[1]]

  if (length(fallback_items) == 0) {
    cat("IBCF failed for", user_id, "- using top popular songs.\n")
    return(colnames(rrm)[order(colCounts(rrm), decreasing = TRUE)[1:n]])
  }

  return(fallback_items)
}

# ==========================
# Main Recommendation Function (UBCF + Filtering)
# ==========================
get_ubcf_recommendations <- function(user_id, n = 10) {
  if (!(user_id %in% rownames(rrm))) {
    return(data.frame("Error" = paste("User ID", user_id, "not found.")))
  }

  predicted <- predict(ubcf_model, rrm[user_id, ], n = n * 2)
  recommended_items <- as(predicted, "list")[[1]]

  if (length(recommended_items) == 0) {
    cat("UBCF failed for", user_id, "- using fallback.\n")
    recommended_items <- get_ibcf_fallback_recommendations(user_id, n = n * 2)
  }

  # Exclude already rated songs
  user_ratings <- as(rrm[user_id, ], "matrix")
  rated_songs <- colnames(user_ratings)[!is.na(user_ratings)]
  filtered_recommendations <- setdiff(recommended_items, rated_songs)

  # Fallback to popular songs if necessary
  if (length(filtered_recommendations) < n) {
    fallback <- setdiff(colnames(rrm)[order(colCounts(rrm), decreasing = TRUE)], rated_songs)
    filtered_recommendations <- unique(c(filtered_recommendations, fallback))[1:n]
  } else {
    filtered_recommendations <- filtered_recommendations[1:n]
  }

  # Return song names
  recommendations <- song_lookup %>%
    filter(sid %in% filtered_recommendations) %>%
    select(sid, name) %>%
    distinct()

  return(recommendations)
}

# ==========================
# Validation Function
# ==========================
validate_recommendations <- function(user_id, recommended_songs, n = 10) {
  if (!(user_id %in% rownames(rrm))) {
    return(data.frame("Error" = paste("User ID", user_id, "not found.")))
  }

  user_ratings <- as(rrm[user_id, ], "matrix")
  rated_songs <- colnames(user_ratings)[!is.na(user_ratings)]

  # Filter out songs that have been rated already
  recommended_songs <- recommended_songs[!(recommended_songs %in% rated_songs)]

  if (length(recommended_songs) == 0) {
    cat("User has already rated all recommended songs. Using top popular songs as fallback.\n")
    recommended_songs <- colnames(rrm)[order(colCounts(rrm), decreasing = TRUE)[1:n]]
  }

  recommendations <- song_lookup %>%
    filter(sid %in% recommended_songs) %>%
    select(sid, name) %>%
    distinct()

  return(recommendations)
}

# ==========================
# Example Usage
# ==========================


# Get and validate UBCF recommendations for user U3
# user_id <- "U3"
# recommended_songs <- get_ubcf_recommendations(user_id, n = 10)
# valid_recommendations <- validate_recommendations(user_id, recommended_songs$sid, n = 10)

# cat("Validated Recommendations:\n")
# print(valid_recommendations)
