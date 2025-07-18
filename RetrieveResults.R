observeEvent(input$retrieve_results, {

  # Retrieve unique version values
  unique_versions <- dbGetQuery(
    db,
    "SELECT DISTINCT Version FROM FinalSelection WHERE LoB = ? AND AggClass = ?",
    params = list(input$dataSelect, input$QuarterSelect)
  )
  
  appendTab(inputId = "tabs",
            tabPanel(title = "Version retrieval",
                     sidebarPanel(
                       selectInput("versionretrieve", "Select a Version:",
                                   choices = c("",unique_versions$Version)),
                       actionButton("selectretrieve", "Select Version to retrieve")
                     ),
                     mainPanel(
                       textOutput("selectedVersion"),
                       DTOutput("Selectio"),
                       DTOutput("BF_LossRatios"),
                       DTOutput("BF_cvu"),
                       DTOutput("TRInc"),
                       DTOutput("TRPaid")
                     )     
            )
  )
  
  output$retrievedVersion <- renderText({
    paste("Selected Version:", input$versionretrieve)
  })
  
    tabIDs(c(tabIDs(), "Version retrieval"))  
     
})

observeEvent(input$selectretrieve, {
  
  # Retrieve unique version values
  version <- input$versionretrieve
  LoB<-input$dataSelect
  quarter_name <- input$QuarterSelect
  
  Selectio <- dbGetQuery(
    db,
    "SELECT DISTINCT (strftime('%Y', UWY)+2024+4661) as year, Value FROM FinalSelection WHERE LoB = ? AND AggClass = ? and Version = ?",
    params = list(input$dataSelect, input$QuarterSelect, input$versionretrieve)
  )
  
  BF_LossRatios <- dbGetQuery(
    db,
    "SELECT DISTINCT (strftime('%Y', UWY)+2024+4661) as year, Value FROM BF_Results WHERE LoB = ? AND AggClass = ? AND Version=? AND DataType='BF LR' ",
    params = list(input$dataSelect, input$QuarterSelect,input$versionretrieve)
  )
  
  BF_cvu <- dbGetQuery(
    db,
    "SELECT DISTINCT (strftime('%Y', UWY)+2024+4661) as year, Value FROM BF_Results WHERE LoB = ? AND AggClass = ? and Version=? AND DataType='BF cvu'",
    params = list(input$dataSelect, input$QuarterSelect,input$versionretrieve)
  )
  
  TRInc_fr <- dbGetQuery(
    db,
    "SELECT DISTINCT UWY, Dev, Value FROM triangle_results WHERE LoB = ? AND AggClass = ? AND Version = ? AND DataType='Incurred Claims' ",
    params = list(input$dataSelect, input$QuarterSelect, input$versionretrieve)
  )
  
  TRInc_fr$annee <- as.integer(format(as.Date(TRInc_fr$UWY), "%Y"))
  TRInc_fr$anneedev <- as.integer(format(as.Date(TRInc_fr$Dev), "%Y"))
  TRInc_fr$anneedevN <- TRInc_fr$anneedev - TRInc_fr$annee
  
  TRInc_fr1 <- as.data.frame(as.triangle(TRInc_fr, origin = "annee", dev = "anneedevN", value = "Value", na.rm = TRUE))
  
  TRInc <- TRInc_fr1 %>% spread(key = anneedevN, value)
  
  TRPaid_fr <- dbGetQuery(
    db,
    "SELECT DISTINCT UWY, Dev, Value FROM triangle_results WHERE LoB = ? AND AggClass = ? AND Version = ? AND DataType='Paid Claims' ",
    params = list(input$dataSelect, input$QuarterSelect, input$versionretrieve)
  )

  TRPaid<-0
  
  if (nrow(TRPaid_fr)!=0) {
    TRPaid_fr$annee <- as.integer(format(as.Date(TRPaid_fr$UWY), "%Y"))
    TRPaid_fr$anneedev <- as.integer(format(as.Date(TRPaid_fr$Dev), "%Y"))
    TRPaid_fr$anneedevN <- TRPaid_fr$anneedev - TRPaid_fr$annee
    
    TRPaid_fr1 <- as.data.frame(as.triangle(TRPaid_fr, origin = "annee", dev = "anneedevN", value = "Value", na.rm = TRUE))
    
    TRPaid <- TRPaid_fr1 %>% spread(key = anneedevN, value)
  }
  
  
   
  # Render the data tables with titles
  output$Selectio <- renderDT({
    datatable(Selectio, caption = "Selection Table")
  })
  
  output$BF_LossRatios <- renderDT({
    datatable(BF_LossRatios, caption = "BF Loss Ratios Table")
  })
  
  output$BF_cvu <- renderDT({
    datatable(BF_cvu, caption = "BF CVU Table")
  })
  
  output$TRInc <- renderDT({
    datatable(TRInc, caption = "Incurred Claims Table")
  })
  
  output$TRPaid <- renderDT({
    datatable(TRPaid, caption = "Paid Claims Table")
  })
  
  
})

