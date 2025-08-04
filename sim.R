library(MASS)
library(tidyverse)
library(BuyseTest)
#Gaussian space 

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

thresholdA<- c(0, 0.059, 0.088, 0.353, 0.735, 1)
thresholdB<- c(0, 0.03, 0.09, 0.50, 0.79, 1)

dataset<-comparison_dataset(60, c(0.25, 0.4), c(0.7, 0.6), -0.6, c(9,8), c(1,3),t(matrix(c(thresholdA, thresholdB), ncol=2)), 0.8 )

GPC_analysis<-BuyseTest(treatment = "dose", endpoint = c("toxicity", "response", "sufficient_exposure", "ordinal"), threshold=c(NA, NA, NA, 0.5), 
          operator = c("<0", ">0", ">0", "<0" ),type=c("b", "b", "b", "c"), data=dataset)
summary<- data.frame(summary(GPC_analysis))
win<-summary$favorable...
loss<-summary$unfavorable...
tie<-summary$neutral...[length(summary$neutral...)]
sum(win, loss, tie)

#TO DO: check happy with how it is calculating comparisons 
#collect win, loss, tie info and then can run some simulation studies  
