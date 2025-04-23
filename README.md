#  Music Recommendation System

## Objective
This project aims to build a personalized music recommender system that suggests songs to users based on their preferences, behavior, and selected tracks. It uses User-Based Collaborative Filtering (UBCF), Item-Based Collaborative Filtering (IBCF), and content-based methods and delivers the experience via an interactive Shiny app.

## System Architecture
- **Frontend**: Built using `shiny` and `shinydashboard`, providing a responsive UI where users can input their ID, view personalized song suggestions, and search or click songs to generate a queue.
- **Backend**: Implements recommendation logic using pre-trained models and similarity matrices stored as `.rds` files.
- **Datastore**: CSV files store user interaction data and song metadata.
- **Recommendation Engine**: Combines collaborative filtering and content-based similarity techniques to serve dynamic recommendations.

## Technologies Used
- **Platform**: Windows/Linux
- **Language**: R
- **Libraries**: `shiny`, `shinydashboard`, `recommenderlab`, `proxy`, `dplyr`, `reshape2`, `stringr`, `DT`
- **Data Storage**: CSV files, `.rds` model files

## Recommendation Methods

### 1. User-Based Collaborative Filtering (UBCF)
- Recommends songs by identifying similar users using cosine similarity.
- Trained using `recommenderlab` on a user-song interaction matrix.

### 2. Item-Based Collaborative Filtering (IBCF)
- Recommends songs similar to a clicked song based on co-listening behavior.
- Utilizes cosine similarity between song vectors.

### 3. Content-Based Filtering
- Calculates similarity using audio features such as valence, energy, acousticness, etc.
- Used as a fallback when collaborative data is sparse or unavailable.

### 4. Hybrid Approach
- Combines UBCF and IBCF for robust recommendations.
- Includes content-based fallback for cold-start tracks.

## Dataset Overview
- **merged_data.csv**: Audio features and metadata for each song.
- **user_item_matrix.csv**: Binary interaction matrix (user ID vs. song ID).
- **ubcf_model.rds / ibcf_model.rds / similarity_matrix.rds**: Pre-trained models and matrices used for fast inference.
