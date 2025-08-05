library(shiny)
library(ChainLadder)
library(DBI)
library(RSQLite)
library(DT)
library(tidyr)
library(shinyjs)
library(rlang)
library(openxlsx)

source("ChainLadder.R")


server <- function(input, output, session) {
  
  # Create a connection to the SQLite database
  db_path <- file.path("HelpDoc", "triangle_data.sqlite")
  db <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Create tables to store the data if they don't exist
  dbExecute(db, "CREATE TABLE IF NOT EXISTS triangle_data (
  UWY DATE,
  Dev DATE,
  AggClass TEXT,
  LoB TEXT,
  DataType TEXT,
  Value REAL
)")
  
  dbExecute(db, "CREATE TABLE IF NOT EXISTS vector_data (
  UWY DATE,
  AggClass TEXT,
  LoB TEXT,
  DataType TEXT,
  Value REAL
)")
  
  
  # Reactive value to keep track of tab IDs
  tabIDs <- reactiveVal(c())
  
  # Reactive value to store excluded cells
  excluded_cellsInc <- reactiveVal(list())
  excluded_cellsPaid <- reactiveVal(list())
  
  # Reactive values to store the variables
  valeurs <- reactiveValues(
    TRInc = NULL,
    TRPaid = NULL,
    Premium = NULL,
    NbClaims = NULL,
    CLcoefInc = NULL
  )
  
  data1 <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath)
    print(head(df))
    return(df)
  })
  
  output$contents1 <- renderTable({
    data1()
  })
  
  output$summary1 <- renderPrint({
    data <- data1()
    if (!is.null(data)) {
      summary(data)
    }
  })
  
  data2 <- reactive({
    req(input$file2)
    inFile <- input$file2
    df <- read.csv(inFile$datapath)
    print(head(df))
    return(df)
  })
  
  output$contents2 <- renderTable({
    data2()
  })
  
  output$summary2 <- renderPrint({
    data <- data2()
    if (!is.null(data)) {
      summary(data)
    }
  })
  
  output$trianglePlot <- renderPlot({
    req(input$calculate)
    triangleData <- data1()
    if (nrow(triangleData) > 0) {
      #      plot(triangleData)
    } else {
      print("No data to plot")
    }
  })
  
  observeEvent(input$calculate, {
    data <- data1()
    triangleDataDF <- as.data.frame(data, stringsAsFactors = FALSE)
    colnames(triangleDataDF) <- c("UWY", "Dev", "AggClass", "LoB", "DataType", "Value")
    triangleDataDF$UWY <- as.Date(triangleDataDF$UWY, "%m/%d/%Y")
    triangleDataDF$Dev <- as.Date(triangleDataDF$Dev, "%m/%d/%Y")
    dbWriteTable(db, "triangle_data", triangleDataDF, append = TRUE, row.names = FALSE)
    
    data <- data2()
    vectorDataDF <- as.data.frame(data, stringsAsFactors = FALSE)
    colnames(vectorDataDF) <- c("UWY", "AggClass", "LoB", "DataType", "Value")
    vectorDataDF$UWY <- as.Date(vectorDataDF$UWY, "%m/%d/%Y")
    dbWriteTable(db, "vector_data", vectorDataDF, append = TRUE, row.names = FALSE)
  })
  
  retrievedData <- eventReactive(input$retrieve, {
    triangleData <- dbReadTable(db, "triangle_data")
    vectorData <- dbReadTable(db, "vector_data")
    list(triangleData = triangleData, vectorData = vectorData)
  })
  
  output$dataSelection <- renderUI({
    req(input$retrieve)
    data <- retrievedData()

    selectInputs <- list(
      selectInput("dataSelect", "Select Data", choices = c("",unique(data$triangleData$LoB))),
      selectInput("QuarterSelect", "Quarter", choices = c("",unique(data$triangleData$AggClass)))
    )
    
    selectInputs
    
  })
  
  output$retrievedData <- renderTable({
    req(input$dataSelect)
    data <- retrievedData()
 #   selectedData <- data$triangleData[data$triangleData$LoB == input$dataSelect, ]
    selectedData <- data$triangleData[ ((data$triangleData$LoB == input$dataSelect) & (data$triangleData$AggClass == input$QuarterSelect)), ]
    # selectedData
  })
  
  observeEvent(input$del_tabs, {
    # Get the current tab IDs
    currentTabIDs <- tabIDs()
    
    session$reload()

  })
  
  
  observeEvent(input$add_tab, {
    dataset_name <- input$dataSelect
    quarter_name <- input$QuarterSelect
    new_tab_titleInc <- paste("Incurred: ", dataset_name)
    new_tab_titlePaid <- paste("Paid: ", dataset_name)
    new_tab_titlePremium <- paste("Premium: ", dataset_name)
    new_tab_titleNbClaims <- paste("NbClaims: ", dataset_name)
    new_tab_titleCLInc <- paste0("Chain Ladder Inc ", dataset_name)
    new_tab_titleGLMInc <- paste0("GLM Inc ", dataset_name)
    new_tab_titleCLPaid <- paste0("Chain Ladder Paid ", dataset_name)
    new_tab_titleGLMPaid <- paste0("GLM Paid ", dataset_name)
    new_tab_titleMunich <- paste0("Munich CL", dataset_name)
    new_tab_titleBFInput <- paste0("BF Input ", dataset_name)
    new_tab_titleBFOutputInc <- paste0("BF Inc ", dataset_name)
    new_tab_titleCapeCodInc <- paste0("Cape-Cod Inc ", dataset_name)
    new_tab_titleSelec <- paste0("Final Selection ", dataset_name)

     # Check if the tab already exists
    tab_titles <- input$tabs
      
    source("GLM.R", local = TRUE)   
    source("CapeCod.R", local = TRUE)   
    source("FinalSelection.R", local = TRUE)   
    source("MunichCL.R", local = TRUE)  
    source("MunichCLVolat.R", local = TRUE)  
    source("BF.R", local = TRUE)   
    source("SaveResults.R", local = TRUE)   
    source("DeleteResults.R", local = TRUE)  
    source("RetrieveResults.R", local = TRUE)  
    source("ExportExcel.R", local = TRUE)  

    if (!(new_tab_titleInc %in% tab_titles)) {
      output[[paste0("valeursOutputTRInc_", dataset_name)]] <- renderDT({
        dataTR <- retrievedData()$triangleData
        dataTR <- dataTR[dataTR$LoB == dataset_name, ]
        dataTR <- dataTR[dataTR$AggClass == quarter_name, ]
        dataTRInc <- dataTR[dataTR$DataType == "Incurred Claims", ]
        
        dataTRInc$annee <- as.integer(format(as.Date(dataTRInc$UWY), "%Y"))
        dataTRInc$anneedev <- as.integer(format(as.Date(dataTRInc$Dev), "%Y"))
        dataTRInc$anneedevN <- dataTRInc$anneedev - dataTRInc$annee
        
        TRInc_fr <- as.data.frame(as.triangle(dataTRInc, origin = "annee", dev = "anneedevN", value = "Value", na.rm = TRUE))
        TRInc <- TRInc_fr %>% spread(key = anneedevN, value = value)
        
        # max_dev <- max(as.numeric(colnames(TRInc)[-1]))
        # TRInc[, (max_dev + 1):max_dev] <- NA
        
        # Exclude selected cells
        excluded <- excluded_cellsInc()
        if (length(excluded[[dataset_name]])>0) {
          for (cell in (1:nrow(excluded[[dataset_name]]))) {
            TRInc[excluded[[dataset_name]][cell,1], excluded[[dataset_name]][cell,2]] <- NA
          }
        }
        
        # Store the variables in reactive values
        valeurs$TRInc <- TRInc

        # Return a data frame to display in the DTOutput
        datatable(TRInc,
                  class = 'triangle-table',
                  selection = list(target = 'cell', mode = 'multiple'),  # Enable cell selection
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    searching = FALSE,
                    ordering = FALSE,
                    info = FALSE,
                    columnDefs = list(list(targets = "_all", className = "dt-right")),
                    select = list(style = 'os', items = 'cell')  # Enable cell selection
                  ),
                  callback = JS("table.on('click', 'td', function() {
                                  var cell = table.cell(this);
                                  var data = cell.data();
                                  Shiny.setInputValue('cell_clicked', {row: cell.index().row, col: cell.index().column, value: data}, {priority: 'event'});
                                });")
        ) %>%
          formatStyle(
            columns = names(TRInc),
            valueColumns = names(TRInc),
            target = 'cell',
            backgroundColor = styleInterval(c(NA), c('yellow', 'white'))
          )
      }, server = FALSE)

      appendTab(inputId = "tabs",
                tabPanel(title = new_tab_titleInc,
                         DTOutput(paste0("valeursOutputTRInc_", dataset_name)),
                         actionButton(paste0("exclude_cellsInc_", dataset_name), "Exclude Selected Cells Inc."),
                         actionButton(paste0("valeursOutputCLInc_", dataset_name), "Chain Ladder Inc."), #Chain Ladder
                         actionButton(paste0("valeursOutputGLMInc_", dataset_name), "GLM Inc."), #GLM
                         actionButton(paste0("valeursOutputMunich_", dataset_name), "Munich CL"), #Munich Chain Ladder
                         actionButton(paste0("BFInput_", dataset_name), "BF Input "), #Bornhuetter Ferguson
                         actionButton(paste0("valeursOutputCapeInc_", dataset_name), "Cape Cod Inc."), #Cape Cod
                         actionButton(paste0("valeursOutputSelec_", dataset_name), "Final Selection") #Cape Cod
                  
                )
      )
      
      # Update the tab_titles vector to include the new tab title
      tabIDs(c(tabIDs(), new_tab_titleInc))
      
      # Observe the exclude button click event
      observeEvent(input[[paste0("exclude_cellsInc_", dataset_name)]], {
        # Get the selected cells
        selected_cells <- input[[paste0("valeursOutputTRInc_", dataset_name, "_cells_selected")]]
        if (!is.null(selected_cells)) {
          # Store the selected cells in the reactive value
          excluded <- excluded_cellsInc()
          excluded[[dataset_name]] <- selected_cells
          excluded_cellsInc(excluded)
         
        # Update the triangle data based on the selected cells
          if (length(selected_cells)>0)
          {
            for (cell in (1:nrow(selected_cells))) {
              valeurs$TRInc[selected_cells[cell,1], selected_cells[cell,2]] <- NA
            }
            
          }
                    
        }
      })
 
     }


      #Add one sheet for Chain ladder calculation
      observeEvent(input[[paste0("valeursOutputCLInc_", dataset_name)]], {
        
        # Check if the tab already exists
        tab_titles<-tabIDs()
        
         if (!(new_tab_titleCLInc %in% tab_titles)) {
          output[[paste0("valeursOutputCLInc_", dataset_name)]] <- renderDT({
          
          # Reshape the data frame back to long format
          TRInc_long <- valeurs$TRInc %>% gather(key = "anneedevN", value = "value", -annee)
          TRInc_long<-TRInc_long[!is.na(TRInc_long$value),]
          
          # Convert back to triangle object
          TRInc_triangle <- as.triangle(TRInc_long, origin = "annee", dev = "anneedevN", value = "value")
          MackCLInc<-apply_mack_chain_ladder(TRInc_triangle)
          CoefCLMatrix<-ata(TRInc_triangle)
          
         # Extract the numeric part from the column names
          numeric_colnames <- sapply(colnames(CoefCLMatrix), function(x) {
            as.numeric(sub("-.*", "", x))
          })
          
          # Assign the numeric column names to the matrix
          colnames(CoefCLMatrix) <- numeric_colnames
          
          CoefCLMatrix_df<-as.data.frame(CoefCLMatrix)
          CoefCLMatrix_df_tri1<-CoefCLMatrix_df %>% spread(key = anneedevN, value = value)
          
          WeightCL <- attr(CoefCLMatrix, "vwtd")
          yy<-c("total",WeightCL)
          
    
          origin_names <- unique(TRInc_long$annee)

          # Identify the missing years
          missing_years <- origin_names[!origin_names %in% CoefCLMatrix_df_tri1$annee]
          
          # Create empty rows for the missing years
          empty_rows <- matrix(" ", nrow = length(missing_years), ncol = ncol(CoefCLMatrix_df_tri1), dimnames = list(missing_years, NULL))
          
          # Assign the values of missing_years to the first column
          empty_rows[, 1] <- missing_years
          
          colnames(empty_rows) <- colnames(CoefCLMatrix_df_tri1)
          
          # Combine the original matrix with the empty rows
          combined_matrix <- rbind(CoefCLMatrix_df_tri1, empty_rows)
          
          # Sort the matrix by row names (years)
          combined_matrix <- combined_matrix[order(combined_matrix$annee), ]
          
          CoefCLMatrix_df_tri1<-rbind(combined_matrix, yy)
          
          Ultimate<-MackCLInc$FullTriangle[,ncol(MackCLInc$FullTriangle)]
          UltSum<-sum(Ultimate)
          UltimateInclTot<-c(Ultimate,UltSum)
          IBNR<-summary(MackCLInc)$ByOrigin["IBNR"]
          IBNRTot<-summary(MackCLInc)$Totals["IBNR",]
          IBNRCum<-c(IBNR$IBNR,IBNRTot)
          MackStDev<-(MackCLInc$Mack.S.E[,ncol(MackCLInc$Mack.S.E)])
          MackStDevTot<-(MackCLInc$Total.Mack.S.E)
          MackStDevCum<-c(MackStDev,MackStDevTot)
          CoV<-MackStDevCum/IBNRCum
          a<-Asymetrie(MackCLInc)
          SkewCum<-c(a$Skewnes,(a$OverSkew/MackStDevTot^3))
          #Claim Development Result (one year)
          CDRCum<-CDR(MackCLInc)[[2]]/IBNRCum
 
          CoefCLMatrix_df_tri<-cbind(CoefCLMatrix_df_tri1,UltimateInclTot,IBNRCum,MackStDevCum,CoV,CDRCum,SkewCum)
          
          valeurs$CLcoefInc<-CoefCLMatrix_df_tri  
          valeurs$MackCLInc<-MackCLInc
          
          datatable(
            CoefCLMatrix_df_tri,
            class = 'display',
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              scrollY = 500,   # Set a fixed height for vertical scrolling
              paging = FALSE,  # Disable paging
              searching = FALSE,  # Disable searching
              ordering = FALSE,   # Disable ordering
              autoWidth = FALSE    # Adjust column widths automatically
            )
            
          ) %>%
            formatRound(columns = 2:ncol(CoefCLMatrix_df_tri), digits = 3)  # Limit digits after the decimal point
          
          
          
          })
           
          appendTab(inputId = "tabs",
                  tabPanel(title = paste0("Chain Ladder Inc ", dataset_name),
                           DTOutput(paste0("valeursOutputCLInc_", dataset_name))    
                  )
           )
          
          # Update the tab_titles vector to include the new tab title
          tabIDs(c(tabIDs(), new_tab_titleCLInc))
          
        }  
      })
      
    if (!(new_tab_titlePaid %in% tab_titles)) {
      output[[paste0("valeursOutputTRPaid_", dataset_name)]] <- renderDT({
        dataTR <- retrievedData()$triangleData
        dataTR <- dataTR[dataTR$LoB == dataset_name, ]
        dataTR <- dataTR[dataTR$AggClass == quarter_name, ]
        dataTRPaid <- dataTR[dataTR$DataType == "Paid Claims", ]
        
        dataTRPaid$annee <- as.integer(format(as.Date(dataTRPaid$UWY), "%Y"))
        dataTRPaid$anneedev <- as.integer(format(as.Date(dataTRPaid$Dev), "%Y"))
        dataTRPaid$anneedevN <- dataTRPaid$anneedev - dataTRPaid$annee
        
        TRPaid_fr <- as.data.frame(as.triangle(dataTRPaid, origin = "annee", dev = "anneedevN", value = "Value", na.rm = TRUE))
        TRPaid <- TRPaid_fr %>% spread(key = anneedevN, value = value)
        
        # max_dev <- max(as.numeric(colnames(TRPaid)[-1]))
        # TRPaid[, (max_dev + 1):max_dev] <- NA
        
        # Exclude selected cells
        excluded <- excluded_cellsPaid()
        if (length(excluded[[dataset_name]])>0) {
          for (cell in (1:nrow(excluded[[dataset_name]]))) {
            TRPaid[excluded[[dataset_name]][cell,1], excluded[[dataset_name]][cell,2]] <- NA
          }
        }
        
          # Store the variables in reactive values
        valeurs$TRPaid <- TRPaid
        
        # Return a data frame to display in the DTOutput
        datatable(TRPaid,
                  class = 'triangle-table',
                  selection = list(target = 'cell', mode = 'multiple'),  # Enable cell selection
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    searching = FALSE,
                    ordering = FALSE,
                    info = FALSE,
                    columnDefs = list(list(targets = "_all", className = "dt-right")),
                    select = list(style = 'os', items = 'cell')  # Enable cell selection
                  ),
                  callback = JS("table.on('click', 'td', function() {
                                  var cell = table.cell(this);
                                  var data = cell.data();
                                  Shiny.setInputValue('cell_clicked', {row: cell.index().row, col: cell.index().column, value: data}, {priority: 'event'});
                                });")
        ) %>%
          formatStyle(
            columns = names(TRPaid),
            valueColumns = names(TRPaid),
            target = 'cell',
            backgroundColor = styleInterval(c(NA), c('yellow', 'white'))
          )
      }, server = FALSE)
      
      
      appendTab(inputId = "tabs",
                tabPanel(title = new_tab_titlePaid,
                         DTOutput(paste0("valeursOutputTRPaid_", dataset_name)),
                         actionButton(paste0("exclude_cellsPaid_", dataset_name), "Exclude Selected Cells Paid"),
                         actionButton(paste0("valeursOutputCLPaid_", dataset_name), "Chain Ladder Paid"),
                         actionButton(paste0("valeursOutputGLMPaid_", dataset_name), "GLM Paid.") #GLM
#                         actionButton(paste0("valeursOutputBFPaid_", dataset_name), "BF Paid."), #Bornhuetter Ferguson
#                         actionButton(paste0("valeursOutputCapePaid_", dataset_name), "Cape Cod Paid.") #Cape Cod
                )
      )
      
      # Update the tab_titles vector to include the new tab title
      tabIDs(c(tabIDs(), new_tab_titlePaid))
      
      observeEvent(input[[paste0("exclude_cellsPaid_", dataset_name)]], {
        # Get the selected cells
        selected_cells <- input[[paste0("valeursOutputTRPaid_", dataset_name, "_cells_selected")]]
        if (!is.null(selected_cells)) {
          # Store the selected cells in the reactive value
          excluded <- excluded_cellsPaid()
          excluded[[dataset_name]] <- selected_cells
          excluded_cellsPaid(excluded)
          
          # Update the triangle data based on the selected cells
          if (length(selected_cells)>0)
          {
            for (cell in (1:nrow(selected_cells))) {
              valeurs$TRPaid[selected_cells[cell,1], selected_cells[cell,2]] <- NA
            }  
          }
          
        }
      })
      
    }  

      
      #Add one sheet for Chain ladder calculation
      observeEvent(input[[paste0("valeursOutputCLPaid_", dataset_name)]], {
        
        tab_titles<-tabIDs()
        
        if (!(new_tab_titleCLPaid %in% tab_titles)) {
          output[[paste0("valeursOutputCLPaid_", dataset_name)]] <- renderDT({
            
            # Reshape the data frame back to long format
            TRPaid_long <- valeurs$TRPaid %>% gather(key = "anneedevN", value = "value", -annee)
            TRPaid_long<-TRPaid_long[!is.na(TRPaid_long$value),]
            
            # Convert back to triangle object
            TRPaid_triangle <- as.triangle(TRPaid_long, origin = "annee", dev = "anneedevN", value = "value")
            MackCLPaid<-apply_mack_chain_ladder(TRPaid_triangle)
            CoefCLMatrix<-ata(TRPaid_triangle)
            
            # Extract the numeric part from the column names
            numeric_colnames <- sapply(colnames(CoefCLMatrix), function(x) {
              as.numeric(sub("-.*", "", x))
            })
            
            # Assign the numeric column names to the matrix
            colnames(CoefCLMatrix) <- numeric_colnames
            
            CoefCLMatrix_df<-as.data.frame(CoefCLMatrix)
            CoefCLMatrix_df_tri1<-CoefCLMatrix_df %>% spread(key = anneedevN, value = value)
            
            WeightCL <- attr(CoefCLMatrix, "vwtd")
            yy<-c("total",WeightCL)
            
            
            origin_names <- unique(TRPaid_long$annee)
            
            # Identify the missing years
            missing_years <- origin_names[!origin_names %in% CoefCLMatrix_df_tri1$annee]
            
            # Create empty rows for the missing years
            empty_rows <- matrix(" ", nrow = length(missing_years), ncol = ncol(CoefCLMatrix_df_tri1), dimnames = list(missing_years, NULL))
            
            # Assign the values of missing_years to the first column
            empty_rows[, 1] <- missing_years
            
            colnames(empty_rows) <- colnames(CoefCLMatrix_df_tri1)
            
            # Combine the original matrix with the empty rows
            combined_matrix <- rbind(CoefCLMatrix_df_tri1, empty_rows)
            
            # Sort the matrix by row names (years)
            combined_matrix <- combined_matrix[order(combined_matrix$annee), ]
            
            CoefCLMatrix_df_tri1<-rbind(combined_matrix, yy)
            
            Ultimate<-MackCLPaid$FullTriangle[,ncol(MackCLPaid$FullTriangle)]
            UltSum<-sum(Ultimate)
            UltimateInclTot<-c(Ultimate,UltSum)
            IBNR<-summary(MackCLPaid)$ByOrigin["IBNR"]
            IBNRTot<-summary(MackCLPaid)$Totals["IBNR",]
            IBNRCum<-c(IBNR$IBNR,IBNRTot)
            MackStDev<-(MackCLPaid$Mack.S.E[,ncol(MackCLPaid$Mack.S.E)])
            MackStDevTot<-(MackCLPaid$Total.Mack.S.E)
            MackStDevCum<-c(MackStDev,MackStDevTot)
            CoV<-MackStDevCum/IBNRCum
            a<-Asymetrie(MackCLPaid)
            SkewCum<-c(a$Skewnes,(a$OverSkew/MackStDevTot^3))
            #Claim Development Result (one year)
            CDRCum<-CDR(MackCLPaid)[[2]]/IBNRCum
            
                                   
            CoefCLMatrix_df_tri<-cbind(CoefCLMatrix_df_tri1,UltimateInclTot,IBNRCum,MackStDevCum,CoV,CDRCum,SkewCum)
            
            valeurs$CLcoefPaid<-CoefCLMatrix_df_tri  
            
            datatable(
              CoefCLMatrix_df_tri,
              class = 'display',
              options = list(
                scrollX = TRUE,  # Enable horizontal scrolling
                scrollY = 500,   # Set a fixed height for vertical scrolling
                paging = FALSE,  # Disable paging
                searching = FALSE,  # Disable searching
                ordering = FALSE,   # Disable ordering
                autoWidth = FALSE    # Adjust column widths automatically
              )
              
            ) %>%
              formatRound(columns = 2:ncol(CoefCLMatrix_df_tri), digits = 3)  # Limit digits after the decimal point
            
            
            
          })
          
          appendTab(inputId = "tabs",
                    tabPanel(title = paste0("Chain Ladder Paid ", dataset_name),
                             DTOutput(paste0("valeursOutputCLPaid_", dataset_name))    
                    )
          )
          
          # Update the tab_titles vector to include the new tab title
          tabIDs(c(tabIDs(), new_tab_titleCLPaid))
          
        }  
      })
      
        
      
    if (!(new_tab_titlePremium %in% tab_titles)) {
      output[[paste0("valeursOutputPremium_", dataset_name)]] <- renderDT({
          dataVEC <- retrievedData()$vectorData
          dataVEC <- dataVEC[dataVEC$LoB == dataset_name, ]
          dataVEC <- dataVEC[dataVEC$AggClass == quarter_name, ]
              dataVECPremium <- dataVEC[dataVEC$DataType == "Premium", ]
          dataVECPremium$annee<-as.integer(format(as.Date(dataVECPremium$UWY),"%Y"))
          Premium<- data.frame(annee=dataVECPremium$annee,Value=dataVECPremium$Value)
          
          valeurs$Premium<-Premium
          
          datatable(Premium,
                    class = 'vector-table',
                    selection = 'none',
                    options = list(
                      dom = 't',
                      paging = FALSE,
                      searching = FALSE,
                      ordering = FALSE,
                      info = FALSE,
                      columnDefs = list(list(targets = "_all", className = "dt-right"))
                    )
          )
          
          
                    
          
      })
    
      appendTab(inputId = "tabs",
                tabPanel(title = new_tab_titlePremium,
                         DTOutput(paste0("valeursOutputPremium_", dataset_name))
                )
      ) 
      
      tabIDs(c(tabIDs(), new_tab_titlePremium))
      
      
      
    }
   
    
    if (!(new_tab_titleNbClaims %in% tab_titles)) {
      output[[paste0("valeursOutputNbClaims_", dataset_name)]] <- renderDT({
        dataVEC <- retrievedData()$vectorData
        dataVEC <- dataVEC[dataVEC$LoB == dataset_name, ]
        dataVEC <- dataVEC[dataVEC$AggClass == quarter_name, ]
        dataVECNb <- dataVEC[dataVEC$DataType == "NbClaims", ]
        dataVECNb$annee<-as.integer(format(as.Date(dataVECNb$UWY),"%Y"))
        NbClaims<- data.frame(annee=dataVECNb$annee,Value=dataVECNb$Value)
        
        valeurs$NbClaims<-NbClaims
        
        datatable(NbClaims,
                  class = 'vector-table',
                  selection = 'none',
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    searching = FALSE,
                    ordering = FALSE,
                    info = FALSE,
                    columnDefs = list(list(targets = "_all", className = "dt-right"))
                  )
        )
        
        
        
        
      })
      
      appendTab(inputId = "tabs",
                tabPanel(title = new_tab_titleNbClaims,
                         DTOutput(paste0("valeursOutputNbClaims_", dataset_name))
                )
      ) 
      
      tabIDs(c(tabIDs(), new_tab_titleNbClaims))
      
    } 
    
  })


  
  output$valeursOutputTRInc <- renderDT({
    datatable(valeurs$TRInc,
              class = 'triangle-table',
              selection = list(target = 'cell', mode = 'multiple'),   # Enable cell selection
              options = list(
                dom = 't',
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE,
                info = FALSE,
                columnDefs = list(list(targets = "_all", className = "dt-right")),
                select = list(style = 'os', items = 'cell')  # Enable cell selection
              ),
              callback = JS("table.on('click', 'td', function() {
                              var cell = table.cell(this);
                              var data = cell.data();
                              Shiny.setInputValue('cell_clicked', {row: cell.index().row, col: cell.index().column, value: data}, {priority: 'event'});
                            });")
    ) %>%
      formatStyle(
        columns = names(valeurs$TRInc),
        valueColumns = names(valeurs$TRInc),
        target = 'cell',
        backgroundColor = styleInterval(c(NA), c('yellow', 'white'))
      )
  }, server = FALSE)
  
  
  
  output$valeursOutputTRPaid <- renderDT({
    datatable(valeurs$TRPaid,
              class = 'triangle-table',
              selection = list(target = 'cell', mode = 'multiple'),   # Enable cell selection
              options = list(
                dom = 't',
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE,
                info = FALSE,
                columnDefs = list(list(targets = "_all", className = "dt-right")),
                select = list(style = 'os', items = 'cell')  # Enable cell selection
              ),
              callback = JS("table.on('click', 'td', function() {
                              var cell = table.cell(this);
                              var data = cell.data();
                              Shiny.setInputValue('cell_clicked', {row: cell.index().row, col: cell.index().column, value: data}, {priority: 'event'});
                            });")
    ) %>%
      formatStyle(
        columns = names(valeurs$TRPaid),
        valueColumns = names(valeurs$TRPaid),
        target = 'cell',
        backgroundColor = styleInterval(c(NA), c('yellow', 'white'))
      )
  }, server = FALSE)
  
 
  output$valeursOutputPremium <- renderDT({
    datatable(valeurs$Premium,
              class = 'vector-table',
              selection = 'none',
              options = list(
                dom = 't',
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE,
                info = FALSE,
                columnDefs = list(list(targets = "_all", className = "dt-right"))
              )
    )
  })
  
  
  output$valeursOutputCLInc <- renderDT({
    
    datatable(
      valeurs$CLcoefInc,
      class = 'display',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        scrollY = 500,   # Set a fixed height for vertical scrolling
        paging = FALSE,  # Disable paging
        searching = FALSE,  # Disable searching
        ordering = FALSE,   # Disable ordering
        autoWidth = FALSE    # Adjust column widths automatically
      )
      
    ) %>%
      formatRound(columns = 2:ncol(valeurs$CLcoefInc), digits = 3)  # Limit digits after the decimal point
    
     })
  
  
  output$valeursOutputCLPaid <- renderDT({
    
    datatable(
      valeurs$CLcoefPaid,
      class = 'display',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        scrollY = 500,   # Set a fixed height for vertical scrolling
        paging = FALSE,  # Disable paging
        searching = FALSE,  # Disable searching
        ordering = FALSE,   # Disable ordering
        autoWidth = FALSE    # Adjust column widths automatically
      )
      
    ) %>%
      formatRound(columns = 2:ncol(valeurs$CLcoefInc), digits = 3)  # Limit digits after the decimal point
    
  })
  
  output$valeursOutputNbClaims <- renderDT({
    datatable(valeurs$NbClaims,
              class = 'vector-table',
              selection = 'none',
              options = list(
                dom = 't',
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE,
                info = FALSE,
                columnDefs = list(list(targets = "_all", className = "dt-right"))
              )
    )
  })
   
  onStop(function() {
    dbDisconnect(db)
  })
}


