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

# Load content-based features
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
rrm <- as(rating_matrix, "realRatingMatrix")

# ==========================
# Step 3: Train or Load IBCF Model
# ==========================
ibcf_model_path <- "ibcf_model.rds"
ibcf_sim_path <- "ibcf_similarity_matrix.rds"

if (file.exists(ibcf_model_path) && file.exists(ibcf_sim_path)) {
  cat("Loading existing IBCF model and similarity matrix...\n")
  ibcf_model_R_model <- readRDS(ibcf_model_path)
  ibcf_model_R_sim_matrix <- readRDS(ibcf_sim_path)
} else {
  cat("Training new IBCF model...\n")
  ibcf_model_R_model <- Recommender(rrm, method = "IBCF", parameter = list(k = 30))
  ibcf_model_R_sim_matrix <- getModel(ibcf_model_R_model)$sim
  item_labels <- colnames(rrm)
  rownames(ibcf_model_R_sim_matrix) <- item_labels
  colnames(ibcf_model_R_sim_matrix) <- item_labels
  
  saveRDS(ibcf_model_R_model, ibcf_model_path)
  saveRDS(ibcf_model_R_sim_matrix, ibcf_sim_path)
  cat("IBCF model and similarity matrix saved.\n")
}

# ==========================
# Step 4: Train or Load UBCF Model
# ==========================
ubcf_model_path <- "ubcf_model2.rds"

if (file.exists(ubcf_model_path)) {
  cat("Loading existing UBCF model...\n")
  ibcf_model_R_ubcf_model <- readRDS(ubcf_model_path)
} else {
  cat("Training new UBCF model...\n")
  ibcf_model_R_ubcf_model <- Recommender(rrm, method = "UBCF", parameter = list(nn = 30))
  saveRDS(ibcf_model_R_ubcf_model, ubcf_model_path)
  cat("UBCF model saved.\n")
}

# ==========================
# Step 5: Recommendation Functions
# ==========================
# Content-Based Fallback
ibcf_model_R_content_recommend <- function(clicked_sid, n = 10) {
  if (!(clicked_sid %in% content_data$sid)) {
    cat("Song ID not found in the content dataset.\n")
    return(NULL)
  }

  clicked_song <- content_data %>% filter(sid == clicked_sid) %>%
    select(valence, danceability, energy, acousticness, tempo)

  if (nrow(clicked_song) == 0) {
    cat("No features available for the clicked song.\n")
    return(NULL)
  }

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

# UBCF Fallback
ibcf_model_R_ubcf_recommend <- function(user_id, n = 10) {
  if (!(user_id %in% rownames(rrm))) {
    cat("User ID not found in the dataset.\n")
    return(NULL)
  }

  predicted <- predict(ibcf_model_R_ubcf_model, rrm[user_id, ], n = n)
  recommended_items <- as(predicted, "list")[[1]]

  if (length(recommended_items) == 0) {
    cat("No recommendations found using UBCF.\n")
    return(NULL)
  }

  return(content_data %>%
           filter(sid %in% recommended_items) %>%
           select(sid, name) %>%
           distinct())
}

# Main Song-Click Based Recommendation
ibcf_model_R_song_click_recommend <- function(clicked_sid, user_id, n = 10) {
  if (clicked_sid %in% rownames(ibcf_model_R_sim_matrix)) {
    similarities <- ibcf_model_R_sim_matrix[clicked_sid, ]
    sorted_similarities <- sort(similarities, decreasing = TRUE)
    top_recs <- head(names(sorted_similarities[sorted_similarities < 1]), n)

    if (length(top_recs) > 0) {
      return(content_data %>%
               filter(sid %in% top_recs) %>%
               select(sid, name) %>%
               distinct() %>%
               arrange(desc(name)))
    }
  }

  cat("No IBCF results — falling back to content-based...\n")
  content_recs <- ibcf_model_R_content_recommend(clicked_sid, n)
  if (!is.null(content_recs) && nrow(content_recs) > 0) {
    return(content_recs)
  }

  cat("No content-based results — falling back to UBCF...\n")
  ubcf_recs <- ibcf_model_R_ubcf_recommend(user_id, n)
  if (!is.null(ubcf_recs) && nrow(ubcf_recs) > 0) {
    return(ubcf_recs)
  }

  cat("No recommendations available.\n")
  return(NULL)
}

# ==========================
# Print the Similarity Matrix
# ==========================
cat("Printing the similarity matrix:\n")
print(ibcf_model_R_sim_matrix)

# ==========================
# Filter Similarity Matrix
# ==========================
# Define the threshold for high similarity
threshold <- 0.7

# Find all pairs with similarity above the threshold
high_similarity_pairs <- which(ibcf_model_R_sim_matrix > threshold, arr.ind = TRUE)

# Print high similarity pairs
cat("\nHigh similarity song pairs (similarity > 0.7):\n")
print(high_similarity_pairs)

# ==========================
# Queue-Based Recommendation Function
# ==========================
Queue_recommendation <- function(clicked_sid, user_id, n = 10) {
  cat("\n Queue Recommendation for song:", clicked_sid, "| user:", user_id, "\n")
  
  # Step 1: Try IBCF
  if (clicked_sid %in% rownames(ibcf_model_R_sim_matrix)) {
    similarities <- ibcf_model_R_sim_matrix[clicked_sid, ]
    sorted_similarities <- sort(similarities, decreasing = TRUE)

    # Exclude the clicked song itself
    top_recs <- head(names(sorted_similarities[sorted_similarities < 1]), n)

    if (length(top_recs) > 0) {
      recommendations <- content_data %>%
        filter(sid %in% top_recs) %>%
        select(sid, name) %>%
        distinct()

      cat("IBCF recommendations found.\n")
      return(recommendations)
    }
  }

  # Step 2: Fallback to UBCF
  cat(" No IBCF recommendations, falling back to UBCF...\n")
  ubcf_recs <- ibcf_model_R_ubcf_recommend(user_id, n)
  if (!is.null(ubcf_recs) && nrow(ubcf_recs) > 0) {
    cat(" UBCF fallback successful.\n")
    return(ubcf_recs)
  }

  # Step 3: Fallback to Content-Based
  cat(" No UBCF fallback, trying content-based recommendations...\n")
  content_recs <- ibcf_model_R_content_recommend(clicked_sid, n)
  if (!is.null(content_recs) && nrow(content_recs) > 0) {
    cat(" Content-based fallback successful.\n")
    return(content_recs)
  }

  # No recommendations available
  cat(" No recommendations found in any method.\n")
  return(NULL)
}
