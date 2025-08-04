library(MASS)
library(tidyverse)
library(BuyseTest)

exposure_fact<- function(n.sample, correlation, exposure_shape, exposure_rate, fact_threshold, exposure_threshold){
  M<- matrix(nrow=n.sample, ncol=2)
  gauss<-mvrnorm(n.sample, rep(0, times=2), Sigma=matrix(c(1, correlation,correlation, 1), nrow=2))
  exposure<-qbeta(pnorm(gauss[,1]), exposure_shape, exposure_rate)
  scores<-pnorm(gauss[,2])
  ordinal<-findInterval(scores, fact_threshold, rightmost.closed = TRUE, all.inside = TRUE)
  M[,1]<- ifelse(exposure<=exposure_threshold, 0, 1)
  M[,2]<- ordinal
  colnames(M)<- c("sufficient_exposure", "ordinal")
  return(M)
}

dlt<- function(n.sample, dlt_rate){
  return(rbinom(n.sample, 1, dlt_rate))
}

efficacy<- function(n.sample, response_rate){
  return(rbinom(n.sample, 1, response_rate))
}

endpoints<- function(n.sample, dlt_rate, response_rate,correlation, exposure_shape, exposure_rate, fact_threshold, exposure_threshold){
  toxicity<- dlt(n.sample, dlt_rate)
  response<- efficacy(n.sample, response_rate)
  id<- 1:n.sample
  M<- cbind(id, toxicity, response, 
            exposure_fact(n.sample, correlation, exposure_shape, exposure_rate, fact_threshold, exposure_threshold))
  return(M)
}

#Assume for now we have the same correlation between PROs and dose-discontinuation for both doses
comparison_dataset<- function(n.sample, vec_dlt_rate, vec_response_rate, correlation, vec_exposure_shape, vec_exposure_rate, mat_fact_threshold, exposure_threshold){
  doseA<- endpoints(n.sample, vec_dlt_rate[1], vec_response_rate[1],correlation, vec_exposure_shape[1], vec_exposure_rate[1], mat_fact_threshold[1,], exposure_threshold)
  doseB<- endpoints(n.sample, vec_dlt_rate[2], vec_response_rate[2],correlation, vec_exposure_shape[2], vec_exposure_rate[2], mat_fact_threshold[2,], exposure_threshold)
  doseA<- cbind(doseA, dose=rep(1, times=n.sample))
  doseB<- cbind(doseB, dose=rep(2, times=n.sample))
  M<- rbind(doseA, doseB)
  return(M)
}