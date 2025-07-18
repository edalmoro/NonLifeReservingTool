observeEvent(input$Excel_Export, {

  can_convert_to_dataframe <- function(x) {
    result <- tryCatch({
      as.data.frame(x)
    }, error = function(e) {
      return(FALSE)
    })
    if (is.data.frame(result)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  export_to_excel <- function(valeurs, file_path) {
    # Create a new workbook
    wb <- createWorkbook()
    
    # Add each table to a separate sheet
    for (i in seq_along(names(valeurs))) {
      sheet_name <- names(valeurs)[i]
      data <- valeurs[[sheet_name]]

      if (can_convert_to_dataframe(data)) {
        # Add a worksheet with the sheet name
        addWorksheet(wb, sheetName = sheet_name)
        
        # Write the data to the worksheet
        writeData(wb, sheet = sheet_name, x = data, startRow = 2)
        
        # Optionally, add a title to the sheet
        writeData(wb, sheet = sheet_name, x = data.frame(Title = sheet_name), startCol = 1, startRow = 1, rowNames = FALSE)
      }
      
    }
    
    # Save the workbook to a file
    saveWorkbook(wb, file_path, overwrite = TRUE) 
  }
 
  # Define the file path where the Excel file will be saved
  file_path <- file.path("HelpDoc", "tables.xlsx")
  
  # Call the export function
  export_to_excel(valeurs, file_path)
  
  file.copy(file_path, "www/tables.xlsx", overwrite = TRUE)
  
#  output$Excel_Export <- downloadHandler(
  #    filename = function() {
  #    # Define the filename for the downloaded file
  #    "tables.xlsx"
  #  },
  #  content = function(file) {
  #    
  #    # Copy the existing file to the download location
  #    file.copy(file_path, file)
  #  }
  #  )
  
  showNotification(
    "Excel Workbook tables.xlsx created",
    type = "warning",
    duration = NULL,  # Display until manually dismissed
    closeButton = TRUE)
  
  # Provide a download link to the user (optional)
  # You can use the shiny::downloadHandler for this purpose
  
})