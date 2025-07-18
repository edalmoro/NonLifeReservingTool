# Chain Ladder calculation

apply_mack_chain_ladder <- function(triangle) {
  # Ensure the input is a triangle object
  if (!inherits(triangle, "triangle")) {
    stop("Input must be a triangle object.")
  }
  
  #Avoid 0 values in the triangle
  triangle[triangle == 0]<-0.0001
  NbLignes<-nrow(triangle)
  
  for (i in 1:NbLignes) {
    for (j in 1:(NbLignes-i+1)){
      if (is.na(triangle[i,j])) { 
        if (j>1) {
          triangle[i,j]<-0.5*(triangle[i,j-1]+triangle[i,j+1])
        }
        else {
          triangle[i,j]<-(triangle[i,j+1])/summary(chainladder(triangle)$Models[[1]])$coef[1]
         }
        }
    }
  }
  
  # Apply the MackChainLadder function
  mack_result <- MackChainLadder(triangle)
  
  # Return the result
  return(mack_result)
}

# Chain Ladder calculation

apply_GLM <- function(triangle) {
  # Ensure the input is a triangle object
  if (!inherits(triangle, "triangle")) {
    stop("Input must be a triangle object.")
  }
  
  #Avoid 0 values in the triangle
  triangle[triangle <= 0]<-0.01
  
  # compound Poisson GLM (variance function estimated from the data):
  fit3 <- glmReserve(triangle, var.power = 1, cum=FALSE)
  
  # Return the result
  return(fit3)
}


##############################################################################
## Calculation of the skewness (Eric Dal Moro - added 31 July 2018)
## 

Asymetrie<- function(x) {
  
  if(! ("MackChainLadder" %in% class(x))){
    stop("x is not a MackChainLadder output")
  }
  
  
  Triangle <- x$Triangle
  FullTriangle <- x$FullTriangle
  
  n <- dim(Triangle)[2]
  
  Skewnes <- c(n-1)
  Skewnesi <- c(n)
  Sk3ki <- c(n)
  Inter <- c(n-1)
  OverSkew <- c(1)
  Variance <- c(n-1)
  Correlation <- matrix(nrow=n-2,ncol=n-2) 
  
  MackModel <- x$Models
  Sigma2<-x$sigma^2
  
  f <- x$f
  f.se <- x$f.se
  sigma <- x$sigma

  #Calculation of the difference between individual chain-ladder coefficients and the chain-ladder coef 
  CLRatio <- function(i, Triangle, f){
    y=Triangle[,i+1]/Triangle[,i] - f[i]
  } 
  
  myModel <- sapply(c(1:(n-1)), CLRatio, Triangle, f)

  #intermediary sums calculation as in the formula of skewness in https://ssrn.com/abstract=2344297  
  Interm1<-function(i, yData){
    Interm1 <- sum(yData[c(1:(n-i)),i]^1.5)
  }
  
  Interm2<-function(i, yData){
    Interm2 <- sum(yData[c(1:(n-i)),i])
  }
  
  #Calculation of Sk3k (voir article SSRN above of Dal Moro for formula)
  
  Skew<- function(i, yModel, yData, Interme1, Interme2){
    #    yModel <- yModel[!is.na(yModel)]
    y=1/(n-i-Interme1[i]^2/Interme2[i]^3)*sum(yData[c(1:(n-i)),i]^1.5*(yModel[c(1:(n-i)),i]^3))
  } 
  
  Triangle[is.na(Triangle)]<-0
  FullTriangle[is.na(FullTriangle)]<-1.0001
  
  Interme1 <- sapply(c(1:(n-1)), Interm1, Triangle)
  Interme2 <- sapply(c(1:(n-1)), Interm2, Triangle)
  Interme2<- as.numeric(Interme2)
  Sk3k <- sapply(c(1:(n-1)), Skew, myModel, Triangle, Interme1, Interme2)
  
  for (k in c(1:(n-1)))
  {
    if ((is.infinite(Sk3k[k])) | (is.nan(Sk3k[k])))
    {Sk3k[k]=0}
  }
  
  Interme2[is.na(Interme2)]<-0.0001
  Interme1[is.na(Interme1)]<-0.0001
  Sk3k[is.na(Sk3k)]<-0

  # Calculation of Skewness per accident year
  for (k in c(1:(n-1))) {
    
    if (Interme2[k]==0.0001) {
     Variance[k]<-0 
    }
    else {
      Variance[k] <- Triangle[n+1-k,k]*Sigma2[k]*(Triangle[n+1-k,k]/Interme2[k]+1)
    } 
    
    if (k<n-1 & Interme2[k]>0.0001) { Skewnes[k] <- Triangle[n+1-k,k]^1.5*Sk3k[k]+Triangle[n+1-k,k]^3*Sk3k[k]*Interme1[k]/Interme2[k]^3 }
    
    for (j in c((k+1):(n-1))) {
      intermediaire <- 0
      intermediaire1 <- 0
      for (v in c(1:(n-j))) {
        intermediaire <- intermediaire + FullTriangle[v,j]
      }
      
      for (v in c(1:(n-j))) {
        intermediaire1 <- intermediaire1 + FullTriangle[v,j]^1.5
      }
      
      if ((k<n-1) && (FullTriangle[n+1-k,j]>1) && (intermediaire>1)) {
        Skewnes[k] <- Skewnes[k]*f[j]^3+FullTriangle[n+1-k,j]^1.5*Sk3k[j]*(1+Variance[k]/FullTriangle[n+1-k,j]^2)^(3/8)+3*Sigma2[j]*f[j]*Variance[k]+FullTriangle[n+1-k,j]^3*intermediaire1/intermediaire^3*Sk3k[j]
      }
      if ((k<n-1) && (FullTriangle[n+1-k,j]>1) && (intermediaire>1)) {
        Variance[k] <- Variance[k]*f[j]^2+FullTriangle[n+1-k,j]^2*Sigma2[j]*(1/intermediaire+1/FullTriangle[n+1-k,j])
      }
      
    }
  }

  #Calculation of Mack correlation between accident years (Mack article 1993)
  for (k in c(1:(n-1))) {
    
    if (Interme2[k]==0.0001) {
      Inter[n-k]<-0 
    }
    else {
      Inter[n-k]<-Sigma2[k]/f[k]^2/Interme2[k]
    } 
    
  }
  
  for (k in c(2:(n-2))) {
    Inter[k]<-Inter[k-1]+Inter[k]
  }
  
  for (k in c(1:(n-2))) {
    for (l in c((k+1):(n-1))) {
      if (Variance[n-k]>0 & Variance[n-l]>0) {
            Correlation[k,l-1]<-Inter[k]*FullTriangle[k+1,n]*FullTriangle[l+1,n]/Variance[n-k]^0.5/Variance[n-l]^0.5
      } else {
        Correlation[k,l-1]<-0
      }
    }
  }
  
  #Calculation of overall Skewness across all accident years
  OverSkew<-sum(Skewnes)

  for (o in c(1:(n-2))) {
    for (p in c((o+1):(n-1))) {
      
      if (FullTriangle[o+1,n]>1 & FullTriangle[p+1,n]>1) {
      OverSkew=OverSkew + 3*Correlation[o,p-1]*(Variance[n-o]*Variance[n-p])^0.5*(Variance[n-o]/FullTriangle[o+1,n]+Variance[n-p]/FullTriangle[p+1,n])*(2+Correlation[o,p-1]*(Variance[n-o]*Variance[n-p])^0.5/(FullTriangle[p+1,n]*FullTriangle[o+1,n]))
      OverSkew=OverSkew + 3*Correlation[o,p-1]^2*(Variance[n-o]*Variance[n-p])*(FullTriangle[o+1,n]+FullTriangle[p+1,n])/(FullTriangle[o+1,n]*FullTriangle[p+1,n])
      }
      
    }
  }
  
  for (o in c(1:(n-3))) {
    for (p in c((o+1):(n-2))) {
      for (q in c((p+1):((n-1)))) {
        
        if (FullTriangle[o+1,n]>1 & FullTriangle[p+1,n]>1 & FullTriangle[q+1,n]>1) {
        OverSkew=OverSkew + 6*Correlation[o,p-1]*Correlation[p,q-1]*Correlation[o,q-1]*(Variance[n-o]*Variance[n-p]*Variance[n-q])^0.5*((Variance[n-o]*Variance[n-p]*Variance[n-q])^0.5/(FullTriangle[o+1,n]*FullTriangle[p+1,n]*FullTriangle[q+1,n])+Variance[n-o]^0.5/(Correlation[p,q-1]*FullTriangle[o+1,n])+Variance[n-p]^0.5/(Correlation[o,q-1]*FullTriangle[p+1,n])+Variance[n-q]^0.5/(Correlation[o,p-1]*FullTriangle[q+1,n]))
        }
        
      }
    }
  }
  

  
  Skewnesi[1]<-0
  Skewnesi[2]<-0
  Sk3ki[1]<-0
  Sk3ki[2]<-0
  
  for (k in c(3:n)) {
    
    Skewnesi[k]<-0
 
    if (Variance[n-k+1]>0) {
      Skewnesi[k]<-Skewnes[n-k+1]/Variance[n-k+1]^1.5
    }
    
    Sk3ki[k]<-Sk3k[n-k+1]
  }  
  
  output <- list()
  output[["Skewnes"]]<-Skewnesi
  output[["Correlation"]]<-Correlation
  output[["OverSkew"]]<-OverSkew
  output[["Sk3k"]]<-Sk3ki
  
  return(output)
}

## End addition Eric Dal Moro 31 July 2018
