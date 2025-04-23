library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Music Recommendation System"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("uid", "Enter User ID:", value = "U1"),
      numericInput("num_recommendations", "Number of Recommendations:", value = 10, min = 1, max = 50),
      actionButton("submit", "Get Recommendations"),
      br(),
      br(),
      textInput("queue_sid", "Enter Song ID for Queue Recommendations:", value = ""),
      actionButton("get_queue", "Get Queue Recommendations"),
      br()
    ),
    
    mainPanel(
      h3("UBCF Recommendations"),
      DTOutput("recommendations_table"),
      br(),
      h3("Queue Recommendations for Selected Song"),
      DTOutput("queue_table")
    )
  )
)