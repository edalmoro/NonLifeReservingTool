
observeEvent(input[[paste0("valeursOutputMunich_", dataset_name)]], {
  
  tab_titles<-tabIDs()
  
  if (!(new_tab_titleMunich %in% tab_titles)) {
    output[[paste0("valeursOutputMunich_", dataset_name)]] <- renderDT({
      

      
      # Reshape the data frame back to long format
      TRInc_long <- valeurs$TRInc %>% gather(key = "anneedevN", value = "value", -annee)
      TRInc_long<-TRInc_long[!is.na(TRInc_long$value),]
      
      # Convert back to triangle object
      TRInc_triangle <- as.triangle(TRInc_long, origin = "annee", dev = "anneedevN", value = "value")
 

      # Reshape the data frame back to long format
      TRPaid_long <- valeurs$TRPaid %>% gather(key = "anneedevN", value = "value", -annee)
      TRPaid_long<-TRPaid_long[!is.na(TRPaid_long$value),]
      
      TRPaid_fr <- (as.triangle(TRPaid_long, origin = "annee", dev = "anneedevN", value = "value", na.rm = TRUE))

      Munich<-MunichChainLadder(TRPaid_fr, TRInc_triangle, est.sigmaP = "Mack", est.sigmaI = "Mack")

      a<-MackChainLadder(TRPaid_fr)
      sigmapaid<-a$sigma*(1+Munich$lambdaP$coefficients)
      sigmapaid[length(sigmapaid)]<-0
      
      b<-MackChainLadder(TRInc_triangle)
      sigmaincurred<-b$sigma*(1+Munich$lambdaI$coefficients)
      sigmaincurred[length(sigmaincurred)]<-0
      
      c<-MackChainLadder1(Triangle1=Munich,est.sigma=sigmapaid,type="paid")
      
      d<-MackChainLadder1(Triangle1=Munich,est.sigma=sigmaincurred,type="incurred")
      
      UltimatePaid<-summary(Munich)$ByOrigin["Ult. Paid"]
      SumUltPaid<-sum(UltimatePaid)
      UltPaid<-rbind(UltimatePaid,Total=SumUltPaid)
      
      VolatPaidInt <- c$Mack.S.E
      TotVolatPaid <- c$Total.Mack.S.E
      VolatPaid<-c(VolatPaidInt,Total=TotVolatPaid)
      
      LatestPaid<-summary(Munich)$ByOrigin["Latest Paid"]
      SumLatPaid<-sum(LatestPaid)
      LatPaid<-rbind(LatestPaid,Total=SumLatPaid)
      
      UltimateInc<-summary(Munich)$ByOrigin["Ult. Incurred"]
      SumUltiInc<-sum(UltimateInc)
      UltInc<-rbind(UltimateInc,Total=SumUltiInc)
      
      VolatIncInt <- d$Mack.S.E
      TotVolatInc <- d$Total.Mack.S.E
      VolatInc<-c(VolatIncInt,Total=TotVolatInc)
      
      LatestInc<-summary(Munich)$ByOrigin["Latest Incurred"]
      SumLatInc<-sum(LatestInc)
      LatInc<-rbind(LatestInc,Total=SumLatInc)
      
      UtlPIRatio<-summary(Munich)$ByOrigin["Ult. P/I Ratio"]
      SumUltPI<-" "
      UltiPI<-rbind(UtlPIRatio,Total=SumUltPI)
      
      IBNRPaid<-UltPaid-LatInc
      colnames(IBNRPaid)<-"IBNRPaid"
      IBNRIncurred<-UltInc-LatInc
      colnames(IBNRIncurred)<-"IBNR Inc."
        
      Munich_res<-cbind(UltPaid, VolatPaid, LatPaid, UltInc, VolatInc, LatInc, UltiPI)
      Munich_res<-cbind(Munich_res, IBNRPaid, IBNRIncurred)
      
      
      valeurs$Munich_res<-Munich_res  
      
      datatable(
        Munich_res,
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
        formatRound(columns = 1:ncol(Munich_res), digits = 3)  # Limit digits after the decimal point
      
      
      
    })
    
    appendTab(inputId = "tabs",
              tabPanel(title = paste0("Munich CL", dataset_name),
                       DTOutput(paste0("valeursOutputMunich_", dataset_name))    
              )
    )
    
    # Update the tab_titles vector to include the new tab title
    tabIDs(c(tabIDs(), new_tab_titleMunich))
    
  }  
})

output$valeursOutputMunich <- renderDT({
  
  datatable(
    valeurs$Munich_res,
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
    formatRound(columns = 1:ncol(valeurs$Munich_res), digits = 3)  # Limit digits after the decimal point
  
})

