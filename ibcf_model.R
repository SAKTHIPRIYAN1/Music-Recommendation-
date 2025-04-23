# ==========================
# Load Required Libraries
# ==========================
library(recommenderlab)
library(reshape2)
library(dplyr)

# ==========================
# Step 1: Load Data
# ==========================
data <- read.csv("merged_data.csv")
data$rating <- as.numeric(data$rating)

# Load content-based features from data_2.csv
content_data <- read.csv("data_2.csv")

# ==========================
# Step 2: Create User-Item Rating Matrix
# ==========================
ratings <- data %>% select(user_id, sid, rating)

# Convert to wide format
rating_matrix <- dcast(ratings, user_id ~ sid, value.var = "rating")
user_ids <- rating_matrix$user_id
rating_matrix <- as.matrix(rating_matrix[, -1])
rownames(rating_matrix) <- user_ids

# Convert to realRatingMatrix
rrm <- as(rating_matrix, "realRatingMatrix")

# ==========================
# Step 3: Train or Load IBCF Model
# ==========================
ibcf_model_path <- "ibcf_model.rds"
ibcf_sim_path <- "ibcf_similarity_matrix.rds"

if (file.exists(ibcf_model_path) && file.exists(ibcf_sim_path)) {
  cat("📦 Loading existing IBCF model and similarity matrix...\n")
  ibcf_model <- readRDS(ibcf_model_path)
  sim_matrix <- readRDS(ibcf_sim_path)
} else {
  cat("🧠 Training new IBCF model...\n")
  ibcf_model <- Recommender(rrm, method = "IBCF", parameter = list(k = 30))
  sim_matrix <- getModel(ibcf_model)$sim
  item_labels <- colnames(rrm)
  rownames(sim_matrix) <- item_labels
  colnames(sim_matrix) <- item_labels

  saveRDS(ibcf_model, ibcf_model_path)
  saveRDS(sim_matrix, ibcf_sim_path)
  cat("✅ IBCF model and similarity matrix saved.\n")
}

# ==========================
# Step 4: Train or Load UBCF Model
# ==========================
ubcf_model_path <- "ubcf_model2.rds"

if (file.exists(ubcf_model_path)) {
  cat("📦 Loading existing UBCF model...\n")
  ubcf_model <- readRDS(ubcf_model_path)
} else {
  cat("🧠 Training new UBCF model...\n")
  ubcf_model <- Recommender(rrm, method = "UBCF", parameter = list(nn = 30))
  saveRDS(ubcf_model, ubcf_model_path)
  cat("✅ UBCF model saved.\n")
}

# ==========================
# Step 5: Recommendation Functions
# ==========================

# Fallback: Recommend based on content features from data_2.csv
recommend_from_content <- function(clicked_sid, n = 10) {
  if (!(clicked_sid %in% content_data$sid)) {
    cat("❌ Song ID not found in the content dataset.\n")
    return(NULL)
  }

  # Get the features of the clicked song
  clicked_song <- content_data %>% filter(sid == clicked_sid) %>%
    select(valence, danceability, energy, acousticness, tempo)

  if (nrow(clicked_song) == 0) {
    cat("❌ No features available for the clicked song.\n")
    return(NULL)
  }

  # Compute similarity based on content features
  content_similarities <- content_data %>%
    select(sid, name, valence, danceability, energy, acousticness, tempo) %>%
    rowwise() %>%
    mutate(similarity = -sum((c(valence, danceability, energy, acousticness, tempo) -
                              as.numeric(clicked_song))^2)) %>%
    ungroup() %>%
    arrange(desc(similarity)) %>%
    filter(sid != clicked_sid) %>%
    head(n)

  return(content_similarities %>% select(sid, name, similarity))
}

# Fallback: Recommend using UBCF
recommend_from_ubcf <- function(user_id, n = 10) {
  if (!(user_id %in% rownames(rrm))) {
    cat("❌ User ID not found in the dataset.\n")
    return(NULL)
  }

  # Predict top N recommendations using UBCF
  predicted <- predict(ubcf_model, rrm[user_id, ], n = n)
  recommended_items <- as(predicted, "list")[[1]]

  if (length(recommended_items) == 0) {
    cat("⚠️ No recommendations found using UBCF.\n")
    return(NULL)
  }

  recommendations <- content_data %>%
    filter(sid %in% recommended_items) %>%
    select(sid, name) %>%
    distinct()

  return(recommendations)
}

recommend_from_song <- function(clicked_sid, user_id, n = 10) {
  # Step 1: Try IBCF
  if (clicked_sid %in% rownames(sim_matrix)) {
    similarities <- sim_matrix[clicked_sid, ]
    sorted_similarities <- sort(similarities, decreasing = TRUE)

    # Exclude the clicked song itself
    top_recs <- head(names(sorted_similarities[sorted_similarities < 1]), n)

    if (length(top_recs) > 0) {
      recommendations <- content_data %>%
        filter(sid %in% top_recs) %>%
        select(sid, name) %>%
        distinct() %>%
        arrange(desc(name)) # Optional: Arrange by name or other criteria

      return(recommendations)
    }
  }

  # Step 2: Fallback to Content-Based Filtering
  cat("⚠️ No similar songs found in IBCF. Falling back to content-based recommendations.\n")
  content_recs <- recommend_from_content(clicked_sid, n)
  if (!is.null(content_recs) && nrow(content_recs) > 0) {
    return(content_recs)
  }

  # Step 3: Fallback to UBCF
  cat("⚠️ No content-based recommendations available. Falling back to UBCF recommendations.\n")
  ubcf_recs <- recommend_from_ubcf(user_id, n)
  if (!is.null(ubcf_recs) && nrow(ubcf_recs) > 0) {
    return(ubcf_recs)
  }

  # No recommendations available
  cat("⚠️ No recommendations available.\n")
  return(NULL)
}

# ==========================
# Step 6: Test the System
# ==========================
# clicked_sid <- "s1405"  # Change this to any valid song ID
# user_id <- "U3"        # Change this to any valid user ID
# cat("🎶 You clicked on:", clicked_sid, "\n\n")

# recommendations <- recommend_from_song(clicked_sid, user_id, n = 10)
# if (!is.null(recommendations)) {
#   cat("🎧 Recommended Songs Based on the Clicked Song:\n")
#   print(recommendations)
# }