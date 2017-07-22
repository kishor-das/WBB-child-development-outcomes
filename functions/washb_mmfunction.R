# hazard ratio estimation
whomm_gam <- function(Y,childage,tr,contrast, W=NULL, pval=0.1){
  require(scam)
  
  if(is.null(W)){
    gamdat <- data.frame(Y, childage, tr)      
  } else {
    gamdat <- data.frame(Y, childage, tr, W)      
  }
  gamdat <- subset(gamdat, tr == contrast[1] | tr == contrast[2])
  gamdat <- gamdat[!is.na(gamdat$Y),]
  n_in <- nrow(gamdat)
  
  # Set binary treatment indicator
  gamdat$tr <- ifelse(gamdat$tr==contrast[2],1,0)
  
  # Covariate selection
  if(!is.null(W)){
    selectedCovs <- washb_prescreen(Y=gamdat[,"Y"], family="binomial", 
                                    Ws=gamdat[,colnames(W)], pval=pval, print=F)
    if(length(selectedCovs)==0){
      selectedCovs <- NULL
    }
  } else{
    selectedCovs <- NULL
  }
  
  # create final dataset
  gamdat <- gamdat[,c("Y","childage","tr",selectedCovs)]
  gamdat <- gamdat[complete.cases(gamdat),]
  n_out <- nrow(gamdat)
  
  # Fit model and extract estimates
  model <- "s(childage, bs='mpi') + tr"
  if(!is.null(selectedCovs)){
    model <- paste(model,paste(selectedCovs, collapse=" + "),sep=" + ")
  }    
  model <- reformulate(model,response="Y")
  
  scamfit <- scam(model, family=binomial(link='cloglog'),data=gamdat)
  hr    <- exp(scamfit$coefficients[2])
  hr_se <- sqrt(diag(scamfit$Ve))[2]
  hr_ci <- exp(c(scamfit$coefficients[2] - 1.96*hr_se, scamfit$coefficients[2] + 1.96*hr_se ))
  
  return(list(c(hr=hr,hr_se=hr_se,hr_ci_lower=hr_ci[1],hr_ci_upper=hr_ci[2],n_in=n_in,n_out=n_out),
              fit=scamfit,
              selectedCovs=selectedCovs))
}  
