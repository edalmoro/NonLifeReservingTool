#Add two sheets for GLM calculation (Incurred and paid)
observeEvent(input[[paste0("valeursOutputGLMInc_", dataset_name)]], {
  
  tab_titles<-tabIDs()
  
  if (!(new_tab_titleGLMInc %in% tab_titles)) {
    output[[paste0("valeursOutputGLMInc_", dataset_name)]] <- renderDT({
      
      # Reshape the data frame back to long format
      TRInc_long <- valeurs$TRInc %>% gather(key = "anneedevN", value = "value", -annee)
      TRInc_long<-TRInc_long[!is.na(TRInc_long$value),]
      
      # Convert back to triangle object
      TRInc_triangle <- as.triangle(TRInc_long, origin = "annee", dev = "anneedevN", value = "value")
      
      NbLignes<-nrow(TRInc_triangle)
      
      for (i in 1:NbLignes) {
        for (j in 1:(NbLignes-i+1)){
          if (is.na(TRInc_triangle[i,j])) { 
            if (j>1) {
              TRInc_triangle[i,j]<-0.5*(TRInc_triangle[i,j-1]+TRInc_triangle[i,j+1])
            }
            else {
              TRInc_triangle[i,j]<-(TRInc_triangle[i,j+1])/summary(chainladder(TRInc_triangle)$Models[[1]])$coef[1]
            }
          }
        }
      }
      
     TRInc_triangle_Incr<-cum2incr(TRInc_triangle)
      
      MackGLMInc<-apply_GLM(TRInc_triangle_Incr)
     
      Ultimate<-MackGLMInc$summary["Ultimate"]
      IBNR<-MackGLMInc$summary["IBNR"]
      CoV<-MackGLMInc$summary["CV"]
     
      Model<-summary(MackGLMInc$model)$coefficients
      Model_factor<-(Model[1:nrow(Ultimate),])
      factor_names<-rownames(Model_factor)
      Model_factor<-cbind(factor_names,Model_factor)
      Model_dev<-rbind((Model[(nrow(Ultimate)+1):nrow(Model),])," ")
      dev_names<-rownames(Model_dev)
      Model_dev<-cbind(dev_names,Model_dev)
      
      GLM_Res_Inc<-cbind(Ultimate,IBNR,CoV,Model_factor,Model_dev)
      
      valeurs$GLM_Res_Inc<-GLM_Res_Inc  
      
      datatable(
        GLM_Res_Inc,
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
        formatRound(columns = setdiff(2:ncol(valeurs$GLM_Res_Inc), c(4, 9)), digits = 3)  # Limit digits after the decimal point
      
      
      
    })

    appendTab(inputId = "tabs",
              tabPanel(title = paste0("GLM Inc ", dataset_name),
                       DTOutput(paste0("valeursOutputGLMInc_", dataset_name))    
              )
    )
    
    # Update the tab_titles vector to include the new tab title
    tabIDs(c(tabIDs(), new_tab_titleGLMInc))
    
  }  
})

 output$valeursOutputGLMInc <- renderDT({
  
  datatable(
    valeurs$GLM_Res_Inc,
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
    formatRound(columns = setdiff(2:ncol(valeurs$GLM_Res_Inc), c(4, 9)), digits = 3)  # Limit digits after the decimal point
  
})



observeEvent(input[[paste0("valeursOutputGLMPaid_", dataset_name)]], {
  
  tab_titles<-tabIDs()
  
  if (!(new_tab_titleGLMPaid %in% tab_titles)) {
    output[[paste0("valeursOutputGLMPaid_", dataset_name)]] <- renderDT({
      
      # Reshape the data frame back to long format
      TRPaid_long <- valeurs$TRPaid %>% gather(key = "anneedevN", value = "value", -annee)
      TRPaid_long<-TRPaid_long[!is.na(TRPaid_long$value),]
      
      # Convert back to triangle object
      TRPaid_triangle <- as.triangle(TRPaid_long, origin = "annee", dev = "anneedevN", value = "value")
      
      NbLignes<-nrow(TRPaid_triangle)
      
      for (i in 1:NbLignes) {
        for (j in 1:(NbLignes-i+1)){
          if (is.na(TRPaid_triangle[i,j])) { 
            if (j>1) {
              TRPaid_triangle[i,j]<-0.5*(TRPaid_triangle[i,j-1]+TRPaid_triangle[i,j+1])
            }
            else {
              TRPaid_triangle[i,j]<-0.5*(TRPaid_triangle[i,j+1])
            }
          }
        }
      }
      
      TRPaid_triangle_Incr<-cum2incr(TRPaid_triangle)
      
      MackGLMPaid<-apply_GLM(TRPaid_triangle_Incr)
      
      #browser()
      
      Ultimate<-MackGLMPaid$summary["Ultimate"]
      IBNR<-MackGLMPaid$summary["IBNR"]
      CoV<-MackGLMPaid$summary["CV"]
      
      Model<-summary(MackGLMPaid$model)$coefficients
      Model_factor<-(Model[1:nrow(Ultimate),])
      factor_names<-rownames(Model_factor)
      Model_factor<-cbind(factor_names,Model_factor)
      Model_dev<-rbind((Model[(nrow(Ultimate)+1):nrow(Model),])," ")
      dev_names<-rownames(Model_dev)
      Model_dev<-cbind(dev_names,Model_dev)
      
      GLM_Res_Paid<-cbind(Ultimate,IBNR,CoV,Model_factor,Model_dev)
      
      valeurs$GLM_Res_Paid<-GLM_Res_Paid  
      
      datatable(
        GLM_Res_Paid,
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
        formatRound(columns = setdiff(2:ncol(GLM_Res_Paid), c(4, 9)), digits = 3)  # Limit digits after the decimal point
      
      
      
    })
    
    appendTab(inputId = "tabs",
              tabPanel(title = paste0("GLM Paid ", dataset_name),
                       DTOutput(paste0("valeursOutputGLMPaid_", dataset_name))    
              )
    )
    
    # Update the tab_titles vector to include the new tab title
    tabIDs(c(tabIDs(), new_tab_titleGLMPaid))
    
  }  
})

output$valeursOutputGLMPaid <- renderDT({
  
  datatable(
    valeurs$GLM_Paid,
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
    formatRound(columns = setdiff(2:ncol(valeurs$GLM_Res_Paid), c(4, 9)), digits = 3)  # Limit digits after the decimal point
  
})