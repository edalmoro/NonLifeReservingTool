observeEvent(input$save_results, {

  # Create tables to store the data if they don't exist
  dbExecute(db, "CREATE TABLE IF NOT EXISTS triangle_results (
  UWY DATE,
  Dev DATE,
  AggClass TEXT,
  LoB TEXT,
  DataType TEXT,
  Version DATETIME,
  Value REAL
)")
  
  # Create tables to store the data if they don't exist
  dbExecute(db, "CREATE TABLE IF NOT EXISTS BF_Results (
  UWY DATE,
  AggClass TEXT,
  LoB TEXT,
  DataType TEXT,
  Version DATETIME,
  Value REAL
)")
  
  # Create tables to store the data if they don't exist
  dbExecute(db, "CREATE TABLE IF NOT EXISTS FinalSelection (
  UWY DATE,
  AggClass TEXT,
  LoB TEXT,
  DataType TEXT,
  Version DATETIME,
  Value TEXT
)")
  
  TriangleInc<-valeurs$TRInc
  TrianglePaid<-valeurs$TRPaid
  
  BF_LR<-valeurs$lossRatios
  BF_cvu<-valeurs$cvu
  
  FinalSelect<-valeurs$selectmethod
  
  Version <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  UWYear <- as.Date(paste(TriangleInc$annee, "01", "01", sep = "-"), format = "%Y-%m-%d")
  
  LoB<-input$dataSelect
  AggClass<-input$QuarterSelect
  
  BF_LossRatios<-cbind(UWY=UWYear, AggClass, LoB, DataType="BF LR", Version, Value=BF_LR)
  BF_LossRatios_df<-as.data.frame(BF_LossRatios)
  
  BF_cvus<-cbind(UWY=UWYear, AggClass, LoB, DataType="BF cvu", Version, Value=BF_cvu)
  BF_cvus_df<-as.data.frame(BF_cvus)
  
  if (!is.null(BF_LR)) {
    dbWriteTable(db, "BF_Results", BF_LossRatios_df, append=TRUE)
    dbWriteTable(db, "BF_Results", BF_cvus_df, append=TRUE)
  }
  
  FinalSelect<-cbind(UWY=UWYear,AggClass,LoB,DataType="FinalSelection", Version, Value=FinalSelect)
  FinalSelect_df<-as.data.frame(FinalSelect)
  
  dbWriteTable(db, "FinalSelection", FinalSelect_df, append=TRUE)
  
  TRInc_fr <- gather(TriangleInc, key = "anneedevN", value = "Value", -annee)
  TRInc_fr_t<-na.omit(TRInc_fr)
  
  UWYear <- as.Date(paste(TRInc_fr_t$annee, "01", "01", sep = "-"), format = "%Y-%m-%d")
  DevYear<- as.Date(paste((as.numeric(TRInc_fr_t$annee) + as.numeric(TRInc_fr_t$anneedevN)), "01", "01", sep = "-"), format = "%Y-%m-%d")
  
  TriangleInc_load<-cbind(UWY=UWYear, Dev=DevYear, LoB, AggClass, DataType="Incurred Claims", Version, Value=TRInc_fr_t$Value)
  TriangleInc_load_df<-as.data.frame(TriangleInc_load)
  
  dbWriteTable(db, "triangle_results", TriangleInc_load_df, append=TRUE)
  
  if (!is.null(TrianglePaid)) {
    TRPaid_fr <- gather(TrianglePaid, key = "anneedevN", value = "Value", -annee)
    TRPaid_fr_t<-na.omit(TRPaid_fr)
    
    UWYear <- as.Date(paste(TRPaid_fr_t$annee, "01", "01", sep = "-"), format = "%Y-%m-%d")
    DevYear<- as.Date(paste((as.numeric(TRPaid_fr_t$annee) + as.numeric(TRPaid_fr_t$anneedevN)), "01", "01", sep = "-"), format = "%Y-%m-%d")
    
    TrianglePaid_load<-cbind(UWY=UWYear, Dev=DevYear, LoB, AggClass, DataType="Paid Claims", Version, Value=TRPaid_fr_t$Value)
    TrianglePaid_load_df<-as.data.frame(TrianglePaid_load)
    
    dbWriteTable(db, "triangle_results", TrianglePaid_load_df, append=TRUE)
  }
  
 
})