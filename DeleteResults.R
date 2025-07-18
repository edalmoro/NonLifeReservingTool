observeEvent(input$delete_results, {
  # Retrieve unique version values
  unique_versions <- dbGetQuery(
    db,
    "SELECT DISTINCT Version FROM FinalSelection WHERE LoB = ? AND AggClass = ?",
    params = list(input$dataSelect, input$QuarterSelect)
  )

  appendTab(inputId = "tabs",
            tabPanel(title = "Version to delete",
                     sidebarPanel(
                       selectInput("version", "Select a Version:",
                                   choices = c("",unique_versions$Version)),
                       actionButton("select", "Select Version and delete")
                     ),
                     mainPanel(
                       textOutput("selectedVersion"),
                       textOutput("deletedVersion")
                     )     
            )
  )
  
    output$selectedVersion <- renderText({
      paste("Selected Version:", input$version)
    })
   
    tabIDs(c(tabIDs(), "Version to delete"))  
     
})

observeEvent(input$version, {

    if (input$version != "") {
      # Use a parameterized query to prevent SQL injection
      dbExecute(db, "DELETE FROM  triangle_results WHERE version = ?", params = list(input$version))
      dbExecute(db, "DELETE FROM  BF_results WHERE version = ?", params = list(input$version))
      dbExecute(db, "DELETE FROM  FinalSelection WHERE version = ?", params = list(input$version))
      
      # Update the select input to reflect the deletion
      unique_versions_updated <- dbGetQuery(
        db,
        "SELECT DISTINCT Version FROM FinalSelection WHERE LoB = ? AND AggClass = ?",
        params = list(input$dataSelect, input$QuarterSelect)
      )
      
      updateSelectInput(session, "version", "Select a Version:",
                        choices = c("", unique_versions_updated$Version))
      
      
      output$deletedVersion <- renderText({
        paste("Version deleted:", input$version)
      })
    
      
    }

})
