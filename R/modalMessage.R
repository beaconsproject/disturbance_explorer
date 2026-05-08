modalServer <- function(input, output, session, project, rv){
  ################################################################################################
  # Control on tabs
  ################################################################################################
  # Observe tab changes
  observeEvent(input$tabs, {
    
    if (input$tabs != "select" && isTRUE(input$confHerd==0) && input$tabs != "overview") {
      # Show modal message if tabUpload has not been visited
      showModal(modalDialog(
        title = "Action Required",
        "Please visit the 'Select caribou herd' tab to initialize the map and upload the required dataset before proceeding to the next steps.",
        easyClose = TRUE,
        footer = modalButton("Confirm caribou herd")
      ))
      
      # Redirect user back to tabUpload
      updateTabItems(session = getDefaultReactiveDomain(), "tabs", "select")
    }
  })
  
  observe({
    req(input$capture_map)
    if (input$capture_map) {
      updateTabsetPanel(getDefaultReactiveDomain(), "one", selected = "Reporting")
    } 
  })
} 