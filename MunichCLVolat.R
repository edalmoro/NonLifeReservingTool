library(ChainLadder)

MackChainLadder1 <- function(
  Triangle1,
  weights=1,
  alpha=1,
  est.sigma=sigma,
  tail=FALSE,
  tail.se=NULL,
  tail.sigma=NULL,
  mse.method = "Mack",
  Quantil=FALSE,
  type)
{
    ## idea: have a list for tail factor
    ## tail=list(f=FALSE, f.se=NULL, sigma=NULL, F.se=NULL)
    ##
    # 2013-02-25 Parameter risk recursive formula may have a third term per
    #   Murphy and BBMW
    if (! mse.method %in% c("Mack", "Independence")) stop("mse.method must be 'Mack' or 'Independence'")
    
  if (type=="incurred") {
    Triangle<-Triangle1$Incurred
  }
  if (type=="paid") {
    Triangle<-Triangle1$Paid
  }
     
  
#    Triangle <- checkTriangle(Triangle)
    m <- dim(Triangle)[1]
    n <- dim(Triangle)[2]
    
    ## Create chain ladder models
    
    ## Mack uses alpha between 0 and 2 to distinguish
    ## alpha = 0 straight averages
    ## alpha = 1 historical chain ladder age-to-age factors
    ## alpha = 2 ordinary regression with intercept 0
    
    ## However, in Zehnwirth & Barnett they use the notation of delta, whereby delta = 2 - alpha
    ## the delta is than used in a linear modelling context.
    delta <- 2-alpha
    #  CL <- chainladder1(Triangle, weights=weights, delta=delta)
   
    CL<-Triangle1
    if (type=="incurred") {
      Models<-CL$MackIncurred$Models
    }
    if (type=="paid") {
      Models<-CL$MackPaid$Models
    }
    


    alpha<-1
    
    # Estimate expected values and standard errors in four steps:
    # 1) Squaring the Triangle: Expected values and f/F SE's from the data in the triangle
    # 2) Expected values and f/F SE's from the tail factor specifications
    # 3) Process Risk and Parameter Risk estimates of the squared triangle (incl tail column)
    # 3) Expected values and SE's for the totals-across-origin-periods of the predicted values
    
    
 
    ## EXPECTED VALUES: Predict the chain ladder models

    if (type=="incurred") {
      FullTriangle<-Triangle1$MCLIncurred
    }
    if (type=="paid") {
      FullTriangle<-Triangle1$MCLPaid
    }
    

    ## f/F SE's

    
    if (type=="incurred") {
      StdErr <- Mack.S.E1(Models, FullTriangle, est.sigma = est.sigma,
                          weights = Triangle1$MackIncurred$weights, alpha = alpha)
    }
    if (type=="paid") {
      StdErr <- Mack.S.E1(Models, FullTriangle, est.sigma = est.sigma,
                          weights = Triangle1$MackPaid$weights, alpha = alpha)
    }
    


    ## 3) Calculate process and parameter risks of the predicted loss amounts
    StdErr <- c(StdErr, MackRecursive.S.E1(FullTriangle, StdErr$f, StdErr$f.se, StdErr$F.se, mse.method = mse.method))
    
    
    
    ## 4) Total-across-origin-periods by development period
    ## EXPECTED VALUES
    ##   Not complicated. Not required at this time.
    ## STANDARD ERRORS
    ## Calculate process and parameter risk for the sum of the predicted loss amounts
    Total.SE <- TotalMack.S.E1(FullTriangle, StdErr$f, StdErr$sigma, StdErr$FullTriangle.procrisk, StdErr$FullTriangle.paramrisk, mse.method = mse.method)
    
    ## Collect the output
    output <- list()
    output[["call"]] <-  match.call(expand.dots = FALSE)
    output[["Triangle"]] <- Triangle
    output[["FullTriangle"]] <- FullTriangle
    output[["Models"]] <- CL[["Models"]]
    output[["f"]] <- StdErr$f
    output[["f.se"]] <- StdErr$f.se
    output[["F.se"]] <- StdErr$F.se
    output[["sigma"]] <- StdErr$sigma
    output[["Mack.ProcessRisk"]]   <- StdErr$FullTriangle.procrisk  # new dmm
    output[["Mack.ParameterRisk"]] <- StdErr$FullTriangle.paramrisk  # new dmm
    output[["Mack.S.E"]] <- sqrt(StdErr$FullTriangle.procrisk^2 + StdErr$FullTriangle.paramrisk^2)
    output[["weights"]] <- CL$weights
    output[["alpha"]] <- alpha
    output[["Total.Mack.S.E"]] <- Total.SE
    output[["tail"]] <- tail
    

    class(output) <- c("MackChainLadder", "TriangleModel", "list")
    return(output)
  }




##############################################################################
## Calculation of the mean squared error and standard error
## mean squared error = stochastic error (process variance) + estimation error
## standard error = sqrt(mean squared error)
approx.equal <- function (x, y, tol=.Machine$double.eps^0.5) abs(x-y)<tol
Mack.S.E1 <- function(MackModel, FullTriangle, est.sigma=est.sigma, weights, alpha) {
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  f <- rep(1, n - 1)
  f.se <- rep(0, n - 1)
#  sigma <- rep(0, n - 1)
 sigma<-est.sigma 

  ## Extract estimated slopes, std. error and sigmas
  ## 2015-10-9 Replace lm's warning with more appropriate message
  smmry <- suppressWarnings(lapply(MackModel, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
#  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
#  sigma <- sapply(smmry, function(x) x$sigma)
  
  sigma<-est.sigma
  
  df <- sapply(smmry, function(x) x$df[2L])
  tolerance <- .Machine$double.eps
  perfect.fit <- (df > 0) & (f.se < tolerance)
  w <- which(perfect.fit)
  if (length(w)) {
    warn <- "Information: essentially no variation in development data for period(s):\n"
    nms <- colnames(FullTriangle)
    periods <- paste0("'", paste(nms[w], nms[w+1], sep = "-"), "'")
    warn <- c(warn, paste(periods, collapse = ", "))
    # Print warning message
    warning(warn)
  }
  #   

  
  isna <- is.na(sigma)

  for (i in 1:(n-1)) {
    f.se[i]<-sigma[i]/sqrt(sum(FullTriangle[1:(n-i),i]))
  }
  
  f.se[length(sigma)]<-0

  
  W <- weights
  W[is.na(W)] <- 1
  F.se <- t(sigma/t(sqrt(W[,-n]*t(t(FullTriangle[,-n])^alpha[-n]))))
  
  return(list(sigma = sigma,
              f = f,
              f.se = f.se,
              F.se = F.se)
  )
}
################################################################
MackRecursive.S.E1 <- function(FullTriangle, f, f.se, F.se, mse.method = "Mack"){
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  
  FullTriangle.procrisk <- rep(0,n)
  FullTriangle.paramrisk <- rep(0,n)
  
  
  for (j in 2:n) {
    for (k in (n+1-j):(n-1)) {
      FullTriangle.paramrisk[j]<-FullTriangle.paramrisk[j]+f.se[k]^2/f[k]^2
      FullTriangle.procrisk[j]<-FullTriangle.procrisk[j]+F.se[j,k]^2/f[k]^2
    }
    FullTriangle.paramrisk[j]<-sqrt(FullTriangle.paramrisk[j])*(FullTriangle[j,n])
    FullTriangle.procrisk[j]<-sqrt(FullTriangle.procrisk[j])*(FullTriangle[j,n])
  }
  
  return(list(FullTriangle.procrisk=FullTriangle.procrisk,
              FullTriangle.paramrisk=FullTriangle.paramrisk))
}

################################################################################
## Total reserve SE

TotalMack.S.E1 <- function(FullTriangle, f, sigma, FullTriangle.procrisk, FullTriangle.paramrisk, mse.method = "Mack") {

  C <- FullTriangle
  n <- ncol(C)
  m <- nrow(C)
  total.seR<-0
  
  for (j in 2:n) {
    int2<-0
    int1<-1
    if (j<n) {
      int1<-sum(FullTriangle[(j+1):n,n])
    }
    for (k in (n+1-j):(n-1)) {
      int3<-sum(FullTriangle[1:(n-k),k])
      int2<-int2+2*sigma[k]^2/(int3*f[k]^2)
    }
    total.seR<-total.seR+FullTriangle.procrisk[j]^2+FullTriangle.paramrisk[j]^2+FullTriangle[j,n]*int1*int2
  }
    
  total.seR<-total.seR^0.5
  
  return(total.seR)
}




