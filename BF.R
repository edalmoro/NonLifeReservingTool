
observeEvent(input[[paste0("BFInput_", dataset_name)]], {
  
  tab_titles<-tabIDs()
  
  if (!(new_tab_titleBFInput %in% tab_titles)) {

    # Initialize reactive values for input fields
    TRInc_long <- valeurs$TRInc %>% gather(key = "anneedevN", value = "value", -annee)
    numFields<-as.integer(unique(TRInc_long$annee))
    
    rv <- reactiveValues(
      lossRatios = rep(0, length(numFields)),
      cvu = rep(0, length(numFields))  # Initialize reactive values for CVU
      )
        
    # Reactive data frame to hold the input fields
    inputData <- reactive({

      data.frame(
        ID = numFields[1]:numFields[length(numFields)],
        LossRatio = isolate(rv$lossRatios),
        CVU = isolate(rv$cvu),  # Add CVU column
        stringsAsFactors = FALSE
      )
      
    })

    # Render the table
    output$dynamicTable <- renderDT({

      data <- inputData()
      
       # Create a data frame with editable cells
      datatable(
        data,
        editable = TRUE,
        rownames = FALSE,
        options = list(
          dom = 't',
          autoWidth = FALSE,
          columnDefs = list(
            list(width = '75px', targets = "_all")
          ),
          order = list(list(0, 'desc')) 
        
          )
        )
    }, server = FALSE)
    
    # Proxy to update the table
     proxy <<- dataTableProxy("dynamicTable", deferUntilFlush = FALSE)

    appendTab(inputId = "tabs",
              tabPanel(title = paste0("BF Input ", dataset_name),
                           DTOutput("dynamicTable"),
                           actionButton("calculateBFInc", "Calculate BF Inc")
              )
    )
    
    # Update the tab_titles vector to include the new tab title
    tabIDs(c(tabIDs(), new_tab_titleBFInput))
    

  }  
 
  # Observe changes in the input fields
  observeEvent(input$dynamicTable_cell_edit, {
    info1 <- input$dynamicTable_cell_edit
    
    if (!is.null(info1)) {
      if (info1$col == 1) {  # Assuming LossRatio is the first editable column
        rv$lossRatios[info1$row] <- as.numeric(info1$value)
      } else if (info1$col == 2) {  # Assuming CVU is the second editable column
        rv$cvu[info1$row] <- as.numeric(info1$value)
      }
    }
    
  })
  
  observeEvent(input$calculateBFInc, {

    tab_titles<-tabIDs()
    
    if (!(new_tab_titleBFOutputInc %in% tab_titles)) {
    
    # Access the values from the table
    lossRatios <- rv$lossRatios
    cvu<-rv$cvu
    
    if (any(lossRatios==0)) {
      showNotification(
        "Warning: Uninformed loss ratios are replaced by 100%",
        type = "warning",
        duration = NULL,  # Display until manually dismissed
        closeButton = TRUE)
        
        lossRatios[lossRatios==0]<-1
    }
    
    if (any(cvu==0)) {
      showNotification(
        "Warning: Uninformed CV U are replaced by 10%",
        type = "warning",
        duration = NULL,  # Display until manually dismissed
        closeButton = TRUE)
      
      cvu[cvu==0]<-0.1
    }
 
    valeurs$lossRatios<-lossRatios
    valeurs$cvu<-cvu
     
    PremiumInt<-retrievedData()$vectorData
    PremiumInt <- PremiumInt[PremiumInt$LoB == dataset_name, ]
    PremiumInt <- PremiumInt[PremiumInt$AggClass == quarter_name, ]
    Premium <- PremiumInt[PremiumInt$DataType == "Premium", ]
    a_priori_ultimates <- Premium$Value * lossRatios

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
    
    TRInc_Increm <- cum2incr(TRInc_triangle)
    
    # Calculate the Bornhuetter-Ferguson reserves
    NbLignes<-nrow(TRInc_triangle)
    NbColonnes<-ncol(TRInc_triangle)
 
    RawY <- rep(0, NbLignes)
    RawZ <- rep(0, NbLignes)
    s2 <- rep(0, NbLignes)
    t3 <- rep(0, NbLignes)
    U15<-rep(0, NbLignes)
    IntermSK<-rep(0, NbLignes)
    SKZ<-rep(0, NbLignes)
    SKU<-rep(0, NbLignes)
    SKRiBF<-rep(0, NbLignes)
    SKRi<-rep(0, NbLignes)
    GammaSK<-rep(0, NbLignes)
    seY <- rep(0, NbLignes)
    seY2 <- rep(0, NbLignes)
    seZ <- rep(0, NbLignes)
    msepRi <- rep(0, NbLignes)
    ProcesErr2<-rep(0, NbLignes)
    EstimErr2<-rep(0, NbLignes)
    bi<-rep(0, NbLignes)
    ai<-rep(0, NbLignes)
    
    s2k<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
    t3k<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
    RhoU<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
    RhoZ<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
    RhoZS<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
    CovRij<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
  
    
    for (i in 1:NbLignes) {
      RawY[i]<-sum(TRInc_Increm[1:(NbLignes-i+1),i])/sum(a_priori_ultimates[1:(NbLignes-i+1)])

      U15[i]<-a_priori_ultimates[i]^1.5
      SKU[NbLignes-i+1]<-(3+cvu[i]^2)*cvu[i]*(a_priori_ultimates[i]*cvu[i])^3
      
      if (i==1) {
        RawZ[i]<-RawY[i]
      } else {
        RawZ[i]<-RawZ[i-1]+RawY[i]
      }
    }
    
    bf_ultimates <- rep(0, NbLignes)
    bf_reserves <- rep(0, NbLignes)
    
    for (i in 1:NbLignes) {
       if (!is.na(TRInc_triangle[i, NbLignes-i+1])) {
          bf_ultimates[i] <- TRInc_triangle[i, NbLignes-i+1] + a_priori_ultimates[i]*(1-sum(RawY[1:(NbLignes-i+1)]))
          bf_reserves[i] <-  a_priori_ultimates[i]*(1-sum(RawY[1:(NbLignes-i+1)]))
      }
    }
    
    for (i in 1:NbLignes) {
      for (j in 1:(NbColonnes-i+1)) {
        s2k[i,j]<-(TRInc_Increm[i,j]-a_priori_ultimates[i]*RawY[j])^2 / a_priori_ultimates[i]
        t3k[i,j]<-(TRInc_Increm[i,j]-a_priori_ultimates[i]*RawY[j])^3 / a_priori_ultimates[i]^1.5
      }
    }
   
    for (i in 1:(NbLignes-1)) {
      s2[i]<-sum(s2k[,i])/(NbLignes-i)
      t3[i]<-sum(t3k[,i])/(NbLignes-i)
    }
     

    for (i in 1:(NbLignes-1)) {
      seY[i]<-sqrt(s2[i]/sum(a_priori_ultimates[1:(NbLignes-i+1)]))
      seY2[i]<-seY[i]^2
      
      IntermSK[i]<-t3[i]*sum(U15[1:(NbLignes-i+1)])/sum(a_priori_ultimates[1:(NbLignes-i+1)])^3
      
    }

    for (i in 1:(NbLignes-1)) {
      seZ[i]<-sqrt(min(sum(seY2[1:i]),sum(seY2[(i+1):NbLignes])))
      
      SKZ[i]<-sum(IntermSK[1:i])
      
      SKRiBF[i]<- -SKZ[i]*a_priori_ultimates[NbLignes-i+1]*(3*(a_priori_ultimates[NbLignes-i+1]*cvu[NbLignes-i+1])^2+a_priori_ultimates[NbLignes-i+1]^2) + 6 * a_priori_ultimates[NbLignes-i+1]*(1-RawZ[i])*(a_priori_ultimates[NbLignes-i+1]*cvu[NbLignes-i+1])^2*seZ[i]^2 - SKU[i]*SKZ[i] + SKU[i]*(1-RawZ[i])*(3*seZ[i]+(1-RawZ[i])^2)
      SKRi[i]<-t3[i]*sum(U15[1:(NbLignes-i)])
      
    }
    
    for (i in 1:(NbLignes-1)) {
      msepRi[NbLignes-i+1]<-sqrt(a_priori_ultimates[NbLignes-i+1]*sum(s2[(i+1):NbLignes]) + a_priori_ultimates[NbLignes-i+1]^2*(1+cvu[NbLignes-i+1]^2)*seZ[i]^2 + (a_priori_ultimates[NbLignes-i+1]*cvu[NbLignes-i+1])^2*(1-RawZ[i])^2)
      ProcesErr2[NbLignes-i+1]<-(a_priori_ultimates[NbLignes-i+1]*sum(s2[(i+1):NbLignes]))
      EstimErr2[NbLignes-i+1]<-a_priori_ultimates[NbLignes-i+1]^2*(1+cvu[NbLignes-i+1]^2)*seZ[i]^2 + (a_priori_ultimates[NbLignes-i+1]*cvu[NbLignes-i+1])^2*(1-RawZ[i])^2
      
      GammaSK[NbLignes-i+1]<-(SKRiBF[i]-SKRi[i])/msepRi[NbLignes-i+1]^3
      
    }
    
    i<-NbLignes
    EstimErr2[NbLignes-i+1]<-a_priori_ultimates[NbLignes-i+1]^2*(1+cvu[NbLignes-i+1]^2)*seZ[i]^2 + (a_priori_ultimates[NbLignes-i+1]*cvu[NbLignes-i+1])^2*(1-RawZ[i])^2
    msepRi[NbLignes-i+1]<-bf_reserves[NbLignes-i+1]*cvu[NbLignes-i+1]
    
    for (i in 1:(NbLignes)) {
      if (abs(GammaSK[NbLignes-i+1]/sqrt(8))<=1) { 
      bi[i]<-sqrt(2)*cos(4*pi/3+1/3*acos(-GammaSK[NbLignes-i+1]/sqrt(8)))
      ai[i]<-sqrt(1-2*bi[i]^2)
      }
      else {
        bi[i]<-0.0000000001
        ai[i]<-1.0
      }
      RhoZS[i,i]<-1
    }

    RhoZS[NbLignes, NbLignes]<-1
    
     for (i in 1:(NbLignes-1)) {
      for (j in ((i+1):NbColonnes)) {
        RhoU[i,j]<-1/(1+abs(j-i))
        RhoZ[i,j]<-sqrt(RawZ[NbColonnes-j+1]*(1-RawZ[NbLignes-i+1])/(RawZ[NbLignes-i+1]*(1-RawZ[NbColonnes-j+1])))   # due to 100% as starting point for patterns
        RhoZS[i,j]<-RhoZ[i,j]
        RhoZS[j,i]<-RhoZ[i,j]
      }
    } 

    for (i in 1:(NbLignes-1)) {
      for (j in ((i+1):NbColonnes)) {
        CovRij[i,j]<-RhoU[i,j]*a_priori_ultimates[i]*cvu[i]*a_priori_ultimates[j]*cvu[j]*(1-RawZ[NbLignes+1-i])*(1-RawZ[NbColonnes+1-j])+RhoZ[i,j]*seZ[NbLignes+1-i]*seZ[NbColonnes+1-j]*a_priori_ultimates[j]*a_priori_ultimates[i]
      }
    }   

    TotalSkew<-0
    TotalTest1<-0
    TotalTest2<-0
    
    for (i in 1:(NbLignes)) {
      TotalSkew<-TotalSkew+GammaSK[i]*msepRi[i]^3
    }
 
    for (i in 1:(NbLignes)) {
      for (j in 1:(NbLignes)) {
        if (i != j) {
        TotalTest1<-TotalTest1+3*msepRi[NbLignes-i+1]^2*msepRi[NbLignes-j+1]*2*RhoZS[i,j]*(2*ai[i]*ai[j]*bi[i]+(ai[i]^2+4*bi[i]^2)*bi[j]*RhoZS[i,j])
        }
      }
    }

        
    for (i in 1:(NbLignes)) {
      for (j in 1:(NbLignes)) {
        for (l in 1:NbLignes) {
          if ((i != j) & (i != l) & (j != l)) {
            TotalTest2<-TotalTest2+msepRi[NbLignes-i+1]*msepRi[NbLignes-j+1]*msepRi[NbLignes-l+1]*( 2*(ai[j]*ai[l]*bi[i]*RhoZS[i,j]*RhoZS[i,l]+ai[j]*ai[i]*bi[l]*RhoZS[j,l]*RhoZS[i,l]+ai[i]*ai[l]*bi[j]*RhoZS[i,j]*RhoZS[j,l])+8*bi[i]*bi[j]*bi[l]*RhoZS[i,j]*RhoZS[i,l]*RhoZS[j,l] )
    
        }
      }
     }
    }
    

    TotalSkew<-TotalSkew+TotalTest1+TotalTest2
    
    TotalMsePInter<-sqrt(sum(EstimErr2[1:NbLignes])+sum(CovRij[1:NbLignes,1:NbColonnes])*2)
    TotalMseP<-sqrt(sum(ProcesErr2[1:NbLignes])+TotalMsePInter^2)
        
    BFResInt<-cbind(Bf_Ult=bf_ultimates,BF_Res=bf_reserves,msepRi,Skew=GammaSK)
    BFResTot<-cbind(sum(bf_ultimates),sum(bf_reserves),TotalMseP, TotalSkew/TotalMseP^3)
    BFRes1<-rbind(BFResInt,BFResTot)
    
    numFields<-unique(TRInc_long$annee)
    bbb<-c(numFields, "total")
    BFRes<-cbind(bbb, BFRes1)
    
    valeurs$BFRes<-BFRes  
    
    output[[paste0("valeursOutputBFInc_", dataset_name)]] <- renderDT({
      datatable(
        BFRes,
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
        formatRound(columns = 2:ncol(BFRes), digits = 2)  # Limit digits after the decimal point
    
    })
    
    appendTab(inputId = "tabs",
              tabPanel(title = paste0("BF Inc ", dataset_name),
                       DTOutput(paste0("valeursOutputBFInc_", dataset_name))    
              )
    )  
  
    tabIDs(c(tabIDs(), new_tab_titleBFOutputInc))
  
  }
    
 })

})

output$BFInput <- renderDT({
  
  datatable(
    rv$lossRatios,
    editable = TRUE,
    rownames = FALSE,
    options = list(
      dom = 't',
      autoWidth = FALSE,
      columnDefs = list(
        list(width = '75px', targets = "_all")
      ) 
      
    )
  )
 
})

output$valeursOutputBFInc <- renderDT({
  
  datatable(
    valeurs$BFRes,
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
    formatRound(columns = 1:ncol(valeurs$BFRes), digits = 3)  # Limit digits after the decimal point
  
})
