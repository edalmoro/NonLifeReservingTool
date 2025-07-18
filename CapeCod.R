#Add Cape Cod calculation Incurred
observeEvent(input[[paste0("valeursOutputCapeInc_", dataset_name)]], {
  
  tab_titles<-tabIDs()
  
  if (!(new_tab_titleCapeCodInc %in% tab_titles)) {
    output[[paste0("valeursOutputCapeInc_", dataset_name)]] <- renderDT({
      
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
      
     NbLignes<-nrow(TRInc_triangle)
     NbColonnes<-ncol(TRInc_triangle)
     
     CC_GammaJ <- rep(0, NbLignes)
     CC_zj <- rep(0, NbLignes)
     CC_Ri<- rep(0, NbLignes)
     CC_Ulti<- rep(0, NbLignes)
     CC_qsigma2<- rep(0, NbLignes)
     CC_qsigma3<- rep(0, NbLignes)
     CC_KRi<- rep(0, NbLignes)
     CC_KRiChap_inter<- rep(0, NbLignes)
     CC_KRiChap<- rep(0, NbLignes)
     CC_PVi<- rep(0, NbLignes)
     CC_PEEi_inter<- rep(0, NbLignes)
     CC_PEEi<- rep(0, NbLignes)
     CC_MSEPi<- rep(0, NbLignes)
     CC_SKi<- rep(0, NbLignes)
     bi<-rep(0, NbLignes)
     ai<-rep(0, NbLignes)
     CC_qsigma2Calc<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
     CC_qsigma3Calc<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
     CC_Corr<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
     RhoZ<- matrix(0, nrow=NbLignes, ncol=NbColonnes)
     
     PremiumInt<-retrievedData()$vectorData
     PremiumInt <- PremiumInt[PremiumInt$LoB == dataset_name, ]
     CC_nui_int <- PremiumInt[PremiumInt$DataType == "Premium", ]
     CC_nui<-CC_nui_int$Value
         
     for (i in 1:NbLignes) {
       CC_GammaJ[i]<-sum(TRInc_triangle_Incr[1:(NbLignes-i+1),i])/sum(CC_nui[1:(NbLignes-i+1)])
     }
     
     CC_zj[1]<-CC_GammaJ[1]/sum(CC_GammaJ[1:NbLignes])
     
     for (i in 2:NbLignes) {
       CC_zj[i]<-CC_zj[i-1]+CC_GammaJ[i]/sum(CC_GammaJ[1:NbLignes])
     }

    
          
     for (i in 1:NbLignes) {
       CC_Ri[i]<-(sum(CC_GammaJ[1:NbLignes])-sum(CC_GammaJ[1:i]))*CC_nui[(NbLignes-i+1)]
      }

     for (i in 1:NbLignes) {
       CC_Ulti[i]<-CC_Ri[i]+TRInc_triangle[(NbLignes-i+1),i]
     }
     
     
     for (i in 1:NbLignes) {
       for (j in 1:(NbLignes-i+1)) {
         CC_qsigma2Calc[i,j]<-1/CC_nui[i]*(TRInc_triangle_Incr[i,j]-CC_nui[i]*CC_GammaJ[j])^2
         CC_qsigma3Calc[i,j]<-1/(CC_nui[i]^1.5)*(TRInc_triangle_Incr[i,j]-CC_nui[i]*CC_GammaJ[j])^3
       }
     }
     
     for (i in 1:(NbLignes-1)) {
       CC_qsigma2[i]<-sum(CC_qsigma2Calc[1:(NbLignes-i+1),i])/(NbLignes-i)
       CC_qsigma3[i]<-sum(CC_qsigma3Calc[1:(NbLignes-i+1),i])/(NbLignes-i)
     }
     
     CC_qsigma2[NbLignes]<-CC_qsigma2[NbLignes-1]^2/CC_qsigma2[NbLignes-2]  
     
     for (i in 1:(NbLignes-1)) {
       CC_PVi[i]<-sqrt(sum(CC_qsigma2[(i+1):NbLignes])*CC_nui[NbLignes-i+1])
       CC_KRi[i]<-sum(CC_qsigma3[(i+1):NbLignes])*CC_nui[NbLignes-i+1]^1.5
     }
     
     CC_PVi[NbLignes]<-sqrt(CC_qsigma2[NbLignes]*CC_nui[1])
     
     for (i in 2:NbLignes) {
       CC_PEEi_inter[i]<-CC_qsigma2[i]/sum(CC_nui[1:(NbLignes-i+1)])
       CC_KRiChap_inter[i]<-CC_qsigma3[i]/sum(CC_nui[1:(NbLignes-i+1)])^1.5
     }
     
     for (i in 1:(NbLignes-1)) {
       CC_PEEi[i]<-CC_nui[NbLignes-i+1]*sqrt(sum(CC_PEEi_inter[(i+1):NbLignes]))
       CC_KRiChap[i]<-CC_nui[NbLignes-i+1]^3*sum(CC_KRiChap_inter[(i+1):NbLignes])
     }
     
     CC_PEEi[NbLignes]<-CC_nui[1]*sqrt(CC_PEEi_inter[NbLignes])
     
     for (i in 1:NbLignes) {
       CC_MSEPi[i]<-sqrt(CC_PEEi[i]^2+CC_PVi[i]^2)
     }
     
     for (i in 1:(NbLignes-1)) {
       CC_SKi[i]<-(CC_KRi[i]-CC_KRiChap[i])/CC_MSEPi[i]^3
     }
     
     for (i in 1:(NbLignes-1)) {
       for (j in (i+1):NbLignes) {
         if (j<NbLignes) {
           CC_Corr[i,j]<-CC_nui[NbLignes-i+1]*CC_nui[NbLignes-j+1]*sum(CC_PEEi_inter[(j+1):NbLignes])
         }
         else {
           CC_Corr[i,j]<-CC_nui[NbLignes-i+1]*CC_nui[NbLignes-j+1]*CC_PEEi_inter[NbLignes]
         }
           
       }
     }

     for (i in 1:(NbLignes)) {
       
       if (abs(CC_SKi[NbLignes-i+1]/sqrt(8))<=1) { 
       bi[i]<-sqrt(2)*cos(4*pi/3+1/3*acos(-CC_SKi[NbLignes-i+1]/sqrt(8)))
       ai[i]<-sqrt(1-2*bi[i]^2)
       }
       else {
         bi[i]<-0.0000000001
         ai[i]<-1.0
       }
       
       RhoZ[i,i]<-1
     }
    
     for (i in 2:(NbLignes-1)) {
       for (j in ((i+1):NbColonnes)) {
         if ((CC_zj[NbColonnes-j+1] != 0) &(CC_zj[NbColonnes-j+1]*(1-CC_zj[NbLignes-i+1])/(CC_zj[NbLignes-i+1]*(1-CC_zj[NbColonnes-j+1]))>0)) {
         RhoZ[(i-1),(j-1)]<-sqrt(CC_zj[NbColonnes-j+1]*(1-CC_zj[NbLignes-i+1])/(CC_zj[NbLignes-i+1]*(1-CC_zj[NbColonnes-j+1])))
         }
         else {
           RhoZ[(i-1),(j-1)]<-0
         }
           
         RhoZ[(j-1),(i-1)]<-RhoZ[(i-1),(j-1)]
       }
     } 
     
     TotalSkew<-0
     TotalTest1<-0
     TotalTest2<-0
     
     for (i in 1:(NbLignes)) {
       TotalSkew<-TotalSkew+CC_SKi[i]*CC_MSEPi[i]^3
     }
     
     for (i in 1:(NbLignes)) {
       for (j in 1:(NbLignes)) {
         if (i != j) {
           TotalTest1<-TotalTest1+3*CC_MSEPi[i]^2*CC_MSEPi[j]*2*RhoZ[i,j]*(2*ai[NbLignes-i+1]*ai[NbLignes-j+1]*bi[NbLignes-i+1]+(ai[NbLignes-i+1]^2+4*bi[NbLignes-i+1]^2)*bi[NbLignes-j+1]*RhoZ[i,j])
           
           }
       }
     }
     
     
     for (i in 1:(NbLignes)) {
       for (j in 1:(NbLignes)) {
         for (l in 1:NbLignes) {
           if ((i != j) & (i != l) & (j != l)) {
             TotalTest2<-TotalTest2+CC_MSEPi[i]*CC_MSEPi[j]*CC_MSEPi[l]*( 2*(ai[NbLignes-j+1]*ai[NbLignes-l+1]*bi[NbLignes-i+1]*RhoZ[i,j]*RhoZ[i,l]+ai[NbLignes-j+1]*ai[NbLignes-i+1]*bi[NbLignes-l+1]*RhoZ[j,l]*RhoZ[i,l]+ai[NbLignes-i+1]*ai[NbLignes-l+1]*bi[NbLignes-j+1]*RhoZ[i,j]*RhoZ[j,l])+8*bi[NbLignes-i+1]*bi[NbLignes-j+1]*bi[NbLignes-l+1]*RhoZ[i,j]*RhoZ[i,l]*RhoZ[j,l] )
             
           }
         }
       }
     }
     

     TotalSkew<-TotalSkew+TotalTest1+TotalTest2
     
     Total_MSEP_CC<-sqrt(sum(CC_MSEPi^2)+2*sum(CC_Corr))

     CCResInt<-cbind(CC_Ult=CC_Ulti,CC_Res=CC_Ri,CC_MSEPi,Skew=CC_SKi)
     CCResTot<-cbind(sum(CC_Ulti),sum(CC_Ri),Total_MSEP_CC, TotalSkew/Total_MSEP_CC^3)
     CCRes1<-rbind(CCResInt,CCResTot)
     
     numFields<-sort(unique(TRInc_long$annee), decreasing = TRUE)
     bbb<-c(numFields, "total")
     CC_Res_Inc1<-cbind(bbb, CCRes1)
     CC_Res_Inc<-as.data.frame(CC_Res_Inc1)
     CC_Res_Inc<-CC_Res_Inc[order(CC_Res_Inc$bbb),]

      valeurs$CC_Res_Inc<-CC_Res_Inc  
      
      datatable(
        CC_Res_Inc,
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
        formatRound(columns = 2:ncol(valeurs$CC_Res_Inc), digits = 3)  # Limit digits after the decimal point
      
      
      
    })

    appendTab(inputId = "tabs",
              tabPanel(title = paste0("Cape-Cod Inc ", dataset_name),
                       DTOutput(paste0("valeursOutputCapeInc_", dataset_name))    
              )
    )
    
    # Update the tab_titles vector to include the new tab title
    tabIDs(c(tabIDs(), new_tab_titleCapeCodInc))
    
  }  
})

 output$valeursOutputCapeInc <- renderDT({
  
  datatable(
    valeurs$CC_Res_Inc,
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
    formatRound(columns = setdiff(2:ncol(valeurs$CC_Res_Inc), c(4, 9)), digits = 3)  # Limit digits after the decimal point
  
})


