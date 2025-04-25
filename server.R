# ==========================
# Load Required Libraries
# ==========================
library(shiny)
library(dplyr)

# ==========================
# Source Model Scripts
# ==========================
source("ubcf_model.R")      # For get_ubcf_recommendations()
source("ibcf_model.R")      # For Queue_recommendation()
shinyServer(function(input, output, session) {

  user_data <- reactiveValues(user_id = NULL)

  # On submit, store user ID
  observeEvent(input$submit, {
    user_data$user_id <- input$uid
  })

  # Reactive value to store UBCF recommendations
  ubcf_recs <- reactiveVal()

  # General UBCF Recommendations
  output$recommendations_table <- renderDT({
    req(user_data$user_id)
    recs <- get_ubcf_recommendations(user_data$user_id, n = input$num_recommendations)
    ubcf_recs(recs)  # Save to reactive for use on click
    if (is.null(recs) || nrow(recs) == 0) {
      return(data.frame(Message = "No recommendations found"))
    }
    datatable(recs, selection = "single", options = list(pageLength = 10))
  })

  # Auto-populate queue_sid input and simulate queue generation
  observeEvent(input$recommendations_table_rows_selected, {
    selected_index <- input$recommendations_table_rows_selected
    recs <- ubcf_recs()
    if (length(selected_index) > 0 && !is.null(recs)) {
      selected_sid <- recs$sid[selected_index]
      updateTextInput(session, "queue_sid", value = selected_sid)
      
      # Trigger queue recommendation logic
      uid <- isolate(user_data$user_id)
      queue <- Queue_recommendation(selected_sid, uid, n = input$num_recommendations)
      output$queue_table <- renderDT({
        if (is.null(queue) || nrow(queue) == 0) {
          data.frame(Message = "No queue recommendations found")
        } else {
          datatable(queue, options = list(pageLength = 10))
        }
      })
    }
  })

  # Manual button option still works
  observeEvent(input$get_queue, {
    clicked_sid <- input$queue_sid
    uid <- isolate(user_data$user_id)
    queue <- Queue_recommendation(clicked_sid, uid, n = input$num_recommendations)
    output$queue_table <- renderDT({
      if (is.null(queue) || nrow(queue) == 0) {
        data.frame(Message = "No queue recommendations found")
      } else {
        datatable(queue, options = list(pageLength = 10))
      }
    })
  })

})
