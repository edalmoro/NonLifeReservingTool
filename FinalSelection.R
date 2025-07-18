#Add Final Selection
observeEvent(input[[paste0("valeursOutputSelec_", dataset_name)]], {
  
  tab_titles<-tabIDs()
  
  if (!(new_tab_titleSelec %in% tab_titles)) {
 #   output[[paste0("valeursOutputSelec_", dataset_name)]] <- renderDT({

     if (!is.null(valeurs$MackCLInc)) {
       IntermCorrel<-Asymetrie(valeurs$MackCLInc)
       CorrelInt<-IntermCorrel$Correlation
       
       correlation<-matrix(0,(nrow(CorrelInt)+1), (ncol(CorrelInt)+1))
       
       for (i in 1:(ncol(CorrelInt)+1)) {
         correlation[i,i]<-1
       }
       
       for (i in 1:(ncol(CorrelInt))) {
         for (j in (i+1):(ncol(CorrelInt)+1)) {
           correlation[i,j]<-CorrelInt[i,(j-1)]
           correlation[j,i]<-correlation[i,j]
         }
       }
       
       
     }
    
      NbMethod<-0
      MethodName<-NULL
      MethodResults<-NULL
      
      if (('CLcoefInc' %in% names(valeurs)) & (!is.null(valeurs$CLcoefInc))) { 
        NbMethod<-NbMethod+1
        MethodName<-c(MethodName,"ChainLad Inc", "CoV CL Inc")
        
        MethodResults<-cbind(MethodResults,valeurs$CLcoefInc$annee,valeurs$CLcoefInc$UltimateInclTot, valeurs$CLcoefInc$CoV)
        colnames(MethodResults) <- c("annee", "UltCLInc", "CoVCLInc")
        MethodResults<-as.data.frame(MethodResults)
      }
      
      if ('GLM_Res_Inc' %in% names(valeurs)) { 
        NbMethod<-NbMethod+1
        MethodName<-c(MethodName,"GLM Inc", "CoV GLM Inc")
        
        IntermGLM<-cbind(rownames(valeurs$GLM_Res_Inc),valeurs$GLM_Res_Inc$Ultimate, valeurs$GLM_Res_Inc$CV)
        colnames(IntermGLM) <- c("annee", "UltGLM", "CoVGLMInc")
        IntermGLM<-as.data.frame(IntermGLM)
        
        if (NbMethod>1) {
          MethodResults<-merge(MethodResults,IntermGLM, by="annee", all=TRUE)
        }
        else {
          MethodResults<-InterGLM
        }
      }
      
      if ('Munich_res' %in% names(valeurs)) { 
        NbMethod<-NbMethod+1
        MethodName<-c(MethodName,"Munich CL")
        IntermMCL<-cbind(rownames(valeurs$Munich_res),valeurs$Munich_res$`Ult. Incurred`)
        colnames(IntermMCL) <- c("annee", "UltMCL")
        IntermMCL<-as.data.frame(IntermMCL)
        row_index <- which(IntermMCL$annee == "Total")
        column_index <- "annee"
        IntermMCL[row_index, column_index] <- "total"
        
        if (NbMethod>1) {
          MethodResults<-merge(MethodResults,IntermMCL, by="annee", all=TRUE)
        }
        else {
          MethodResults<-IntermMCL
        }
      }
      
      if ('BFRes' %in% names(valeurs)) { 
        NbMethod<-NbMethod+1
        MethodName<-c(MethodName,"BF Inc", "CoV BF Inc")

        IntermBF<-cbind(valeurs$BFRes[,1],valeurs$BFRes[,2], as.numeric(valeurs$BFRes[,4])/as.numeric(valeurs$BFRes[,3]))
        colnames(IntermBF) <- c("annee", "UltBF", "CoVBFInc")
        IntermBF<-as.data.frame(IntermBF)
        
        if (NbMethod>1) {
          MethodResults<-merge(MethodResults,IntermBF, by="annee", all=TRUE)
        }
        else {
          MethodResults<-IntermBF
        }
        
      }
      if ('CC_Res_Inc' %in% names(valeurs)) { 
        NbMethod<-NbMethod+1
        MethodName<-c(MethodName,"CC Inc", "CoV CC Inc")

        IntermCC<-cbind(valeurs$CC_Res_Inc[,1],valeurs$CC_Res_Inc[,2],as.numeric(valeurs$CC_Res_Inc[,4])/as.numeric(valeurs$CC_Res_Inc[,3]))
        colnames(IntermCC) <- c("annee", "UltCCInc", "CoVCCInc")
        IntermCC<-as.data.frame(IntermCC)
        
        if (NbMethod>1) {
          MethodResults<-merge(MethodResults,IntermCC, by="annee", all=TRUE)
        }
        else {
          MethodResults<-IntermCC
        }
        
      }
      if ('CLcoefPaid' %in% names(valeurs)) { 
        NbMethod<-NbMethod+1
        MethodName<-c(MethodName,"ChainLad Paid", "CoV CL Paid")

        IntermUltPaid<-cbind(valeurs$CLcoefPaid$annee,valeurs$CLcoefPaid$UltimateInclTot, valeurs$CLcoefPaid$CoV)
        colnames(IntermUltPaid) <- c("annee", "UltCLPaid", "CoVCLPaid")
        IntermUltPaid<-as.data.frame(IntermUltPaid)
        
        if (NbMethod>1) {
          MethodResults<-merge(MethodResults,IntermUltPaid, by="annee", all=TRUE)
        }
        else {
          MethodResults<-InterUltPaid
        }
        
      }
      if ('GLM_Res_Paid' %in% names(valeurs)) { 
        NbMethod<-NbMethod+1
        MethodName<-c(MethodName,"GLM Paid", "CoV GLM Paid")

        IntermGLMP<-cbind(rownames(valeurs$GLM_Res_Paid),valeurs$GLM_Res_Paid$Ultimate, valeurs$GLM_Res_Paid$CV)
        colnames(IntermGLMP) <- c("annee", "UltGLMPaid", "CoVGLMPaid")
        IntermGLMP<-as.data.frame(IntermGLMP)
        
        if (NbMethod>1) {
          MethodResults<-merge(MethodResults,IntermGLMP, by="annee", all=TRUE)
        }
        else {
          MethodResults<-InterGLMP
        }
        
      }
      
      # Ensure MethodResults is a data frame
      MethodResults1 <- as.data.frame(MethodResults[1:(nrow(MethodResults)-1),])
      names(MethodResults1)<-c("annee",MethodName)
      MethodResults1 <- replace(MethodResults1, is.na(MethodResults1), 0)
      
      Choice_names <- MethodName[!grepl("^CoV", MethodName)]
      
      for (i in 1:ncol(MethodResults1)) {
        MethodResults1[[i]]<-as.numeric((MethodResults1[[i]]))
      }
      
       # Create UI for selecting methods for each accident year
      output$selectionUI <- renderUI({
        tagList(
          lapply(1:nrow( MethodResults1), function(i) {
            accidentYear <- MethodResults1$annee[i]
            selectInput(
              inputId = paste0("methodSelect_", accidentYear),
              label = paste("Select Method for Accident Year", accidentYear),
              choices = Choice_names,
              selected = Choice_names[1]
            )
          })
        )
      })

 
      # Reactive expression to get selected ultimates
      selectedUltimates <- reactive({
        # Create a data frame with all methods and the selected ultimate values
        ultimates <- MethodResults1
        
        ultimates$SelectedUltimate <- sapply(1:nrow(ultimates), function(i) {
          accidentYear <- ultimates$annee[i]
          selected_method <- input[[paste0("methodSelect_", accidentYear)]]
          ultimates[i, selected_method]
          
        })
    #    ultimates
    
        # Add a total row
        # Initialize total_row as a named list or vector
        total_row <- as.data.frame(matrix(0,nrow=1,ncol=ncol(ultimates)))
        total_row[1]<-"Total"

        # Calculate the sum for each column starting from the second column
        for (i in 2:(ncol(ultimates))) {
          total_row[i] <- sum(ultimates[[i]])
        }
        
        # Convert the list to a data frame row
        names(total_row)<-c("annee",MethodName,"SelectedUltimate")
        
 
        # If you want to bind this row to your existing data frame
        a<-rbind(ultimates, total_row)
        
        a
        
      })

      # Reactive expression to get selected CoV
      selectedCoV <- reactive({
        # Create a data frame with all methods and the selected ultimate values
        ultimates <- MethodResults1
        
         ultimates$selectedCoV <- sapply(1:nrow(ultimates), function(i) {
          accidentYear <- ultimates$annee[i]
          selected_method <- input[[paste0("methodSelect_", accidentYear)]]
          
          if (selected_method != "Munich CL") {
            current_index <- which(names(ultimates) == selected_method)
            ultimates[i, (current_index+1)]
          }
           else {
             0
           }
        
        })
        

        # Add a total row
        # Initialize total_row as a named list or vector
        total_row <- as.data.frame(matrix(0,nrow=1,ncol=ncol(ultimates)))
        total_row[1]<-"Total"
        msep<-rep(0,(nrow(ultimates)-1))
        
        aa<-selectedUltimates()
        aaa<- aa[,'SelectedUltimate']
        totalUlt<-0
        
        # Calculate the sum for each column starting from the second column
        for (i in 2:ncol(ultimates)) {
          
          if (grepl("^CoV",names(ultimates)[i]) | names(ultimates)[i]=="selectedCoV") {
            for (j in 2:(nrow(ultimates))) {
              if (is.numeric(ultimates[j,i])) {
                if (names(ultimates)[i]=="selectedCoV") {
                  msep[(j-1)]<-ultimates[j,i]*aaa[j]
                  totalUlt<-totalUlt+aaa[j]
                }
                else {
                  msep[(j-1)]<-ultimates[j,i]*ultimates[j,i-1]
                  totalUlt<-totalUlt+ultimates[j,i-1]
                }
              }
            }
            
            
            total_row[i]<-sqrt( t(msep) %*% correlation %*% msep ) / totalUlt
            totalUlt<-0
          }  
          else {
            total_row[i] <- sum(ultimates[[i]]) 
            totalUlt<-0
          }
            
        }
        
#        total_row[ncol(ultimates)]<-0
        
        # Convert the list to a data frame row
        names(total_row)<-c("annee",MethodName,"selectedCoV")
        
        # If you want to bind this row to your existing data frame
        b<-rbind(ultimates, total_row)
        
        b
        
      })
      
     appendTab(inputId = "tabs",
              tabPanel(title = paste0("Final Selection ", dataset_name),
                       sidebarLayout(
                         sidebarPanel(
                           # Dynamic UI for selecting methods
                           uiOutput("selectionUI")
                         ),
                         mainPanel(
                           h3("Selected Ultimate Values"),
                           tableOutput("selectedUltimates"),
                           h3("Selected CoV"),
                           tableOutput("selectedCoV"),
                         )
                       )
              )
    )


    # Update the tab_titles vector to include the new tab title
    tabIDs(c(tabIDs(), new_tab_titleSelec))
    
  } 
  

  # Display the selected ultimate values
  output$selectedUltimates <- renderTable({

    for (i in valeurs$TRInc[,1]) {
      valeurs$selectmethod[i]<-input[[paste0("methodSelect_", i)]]
    }

    selectedUltimates()
})
  
  output$selectedCoV <- renderTable({
    selectedCoV()
  })
  
})







