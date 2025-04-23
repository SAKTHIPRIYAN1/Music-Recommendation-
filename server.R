library(shiny)
library(DT)

# Source the UBCF and IBCF model logic
source("ubcf_model.R")
source("ibcf_model.R")

# Define Server
server <- function(input, output, session) {
  # Reactive value to store UBCF recommendations
  ubcf_recommendations <- reactiveVal(NULL)
  
  # Reactive value to store queue recommendations for a clicked song
  queue_recommendations <- reactiveVal(NULL)
  
  # Observe when the "Get Recommendations" button is clicked
  observeEvent(input$submit, {
    user_id <- input$uid
    num_recommendations <- input$num_recommendations  # Get the number of recommendations
    
    # Get recommendations from the UBCF model
    recommendations <- get_recommendations(user_id, n = num_recommendations)
    ubcf_recommendations(recommendations)  # Store UBCF recommendations in reactive value
  })
  
  # Render the UBCF recommendations table
  output$recommendations_table <- renderDT({
    recommendations <- ubcf_recommendations()
    
    if (!is.null(recommendations) && nrow(recommendations) > 0) {
      datatable(
        recommendations,
        selection = "single",  # Enable single row selection
        options = list(dom = "t", pageLength = input$num_recommendations)
      )
    } else {
      datatable(data.frame("Error" = "No recommendations available"), options = list(dom = "t"))
    }
  })
  
  # Observe when a song is clicked in the recommendations table
  observeEvent(input$recommendations_table_rows_selected, {
    # Get the selected row index
    selected_row <- input$recommendations_table_rows_selected
    
    # Check if a row is selected
    if (!is.null(selected_row)) {
      # Extract the Song ID (assuming the table has a column named 'sid')
      recommendations <- ubcf_recommendations()
      selected_sid <- recommendations[selected_row, "sid"]
      
      # Update the queue_sid input box with the selected Song ID
      updateTextInput(session, "queue_sid", value = selected_sid)
    }
  })
  
  # Automatically trigger "Get Queue Recommendations" when queue_sid is updated
  observeEvent(input$queue_sid, {
    clicked_sid <- input$queue_sid  # Get the Song ID from the input field
    user_id <- input$uid  # Use the same user ID
    
    # Validate clicked_sid
    if (!is.null(clicked_sid) && clicked_sid != "") {
      # Get queue recommendations for the entered Song ID
      queue <- recommend_from_song(clicked_sid, user_id, n = input$num_recommendations)
      queue_recommendations(queue)  # Store queue recommendations in reactive value
    } else {
      queue_recommendations(data.frame("Error" = "Invalid or missing Song ID"))
    }
  })
  
  # Render the queue recommendations table
  output$queue_table <- renderDT({
    queue <- queue_recommendations()
    
    if (!is.null(queue) && nrow(queue) > 0) {
      datatable(queue, options = list(dom = "t", pageLength = input$num_recommendations))
    } else {
      datatable(data.frame("Error" = "No queue recommendations available"), options = list(dom = "t"))
    }
  })
}