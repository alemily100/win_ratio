library(MASS)
library(tidyverse)
library(BuyseTest)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)
library(abind)

dlt_exposure_fact_eff<- function(n.sample, correlation_pro_exp,correlation_pro_dlt ,correlation_dlt_eff, exposure_shape, exposure_rate, fact_threshold, exposure_threshold, dlt_rate, eff_rate){
  M<- matrix(nrow=n.sample, ncol=4)
  gauss<-mvrnorm(n.sample, rep(0, times=4), Sigma=matrix(c(1, correlation_pro_exp,0,0,correlation_pro_exp, 1,correlation_pro_dlt,0,0,correlation_pro_dlt,1,
                                                           correlation_dlt_eff,0,0,correlation_dlt_eff,1), nrow=4))
  exposure<-qbeta(pnorm(gauss[,1]), exposure_shape, exposure_rate)
  scores<-pnorm(gauss[,2])
  dlt<- pnorm(gauss[,3])
  eff<-pnorm(gauss[,4])
  ordinal<-findInterval(scores, fact_threshold, rightmost.closed = TRUE, all.inside = TRUE)
  dlt_obs<-findInterval(dlt, c(0, 1-dlt_rate, 1), rightmost.closed = TRUE, all.inside = TRUE)-1
  eff_obs<-findInterval(eff, c(0, 1-eff_rate, 1), rightmost.closed = TRUE, all.inside = TRUE)-1
  M[,1]<- dlt_obs
  M[,2]<- ifelse(exposure<=exposure_threshold, 0, 1)
  M[,3]<- ordinal
  M[,4]<- eff_obs
  colnames(M)<- c("dlt", "sufficient_exposure", "ordinal","eff")
  return(M)
}


#vector of response rates: [1]: no DLT, [2] DLT
#mat_fact: matrix [1,] no DLT [2,] DLT
endpoints<- function(n.sample, dlt_rate, eff_rate, correlation_pro_exp,correlation_pro_dlt, correlation_dlt_eff, exposure_shape, exposure_rate, vec_fact, exposure_threshold){
  fact_threshold<- c(0, cumsum(vec_fact))
  M<-dlt_exposure_fact_eff(n.sample,correlation_pro_exp,correlation_pro_dlt, correlation_dlt_eff,exposure_shape, exposure_rate, fact_threshold, exposure_threshold, dlt_rate, eff_rate)
  #response<- sapply(M[,1], function (k) rbinom(1, 1, vec_response_rate[k+1]))
  id<- 1:n.sample
  M<- cbind(id, M)
  M[which(M[,1]==1),3]<- FALSE
  return(M)
}

#Assume for now we have the same correlation between PROs and dose-discontinuation for both doses
#n.sample_vec: [1] sample size for dose A and [2] sample size for dose B
#vec_overall_response_rate: [1] overall response rates for dose A and [2] overall response rates for dose B. 
#vec_dlt_response_rate: [1] dlt response rates for dose A and [2] dlt response rates for dose B. 
#mat_overall_ordinal: [1,] overall fact for dose A and [2,] overall fact for dose B

comparison_dataset<- function(n.sample_vec, vec_dlt_rate, vec_response_rate,correlation_pro_exp, correlation_pro_dlt, correlation_dlt_eff,
                              vec_exposure_shape, vec_exposure_rate, mat_fact, exposure_threshold){
  #creating response matrices
  #response_no_dlt_A<- prob_no_dlt(vec_overall_response_rate[1], vec_dlt_response_rate[1], vec_dlt_rate[1])
  #response_no_dlt_B<- prob_no_dlt(vec_overall_response_rate[2], vec_dlt_response_rate[2], vec_dlt_rate[2])
  #mat_response_rate<- matrix(
  #  c(response_no_dlt_A, vec_dlt_response_rate[1], response_no_dlt_B, vec_dlt_response_rate[2]), nrow = 2, byrow = TRUE)
  
  doseA<- endpoints(n.sample_vec[1], vec_dlt_rate[1], vec_response_rate[1], correlation_pro_exp, correlation_pro_dlt, correlation_dlt_eff,
                    vec_exposure_shape[1], vec_exposure_rate[1], mat_fact[1,], exposure_threshold)
  doseB<- endpoints(n.sample_vec[2], vec_dlt_rate[2], vec_response_rate[2], correlation_pro_exp, correlation_pro_dlt, correlation_dlt_eff,
                    vec_exposure_shape[2], vec_exposure_rate[2], mat_fact[2,], exposure_threshold)
  doseA<- cbind(doseA, dose=rep(1, times=n.sample_vec[1]))
  doseB<- cbind(doseB, dose=rep(2, times=n.sample_vec[2]))
  M<- rbind(doseA, doseB)
  return(M)
}

analysis<- function(n.sample_vec, vec_dlt_rate, vec_response_rate, correlation_pro_exp, correlation_pro_dlt, correlation_dlt_eff,
                    vec_exposure_shape, vec_exposure_rate, mat_fact, exposure_threshold){
  dataset<- comparison_dataset(n.sample_vec, vec_dlt_rate, vec_response_rate, correlation_pro_exp, correlation_pro_dlt, correlation_dlt_eff,
                               vec_exposure_shape, vec_exposure_rate, mat_fact, exposure_threshold)
  dataset_utility<-data.frame(dataset)%>%
    mutate(utility = case_when(
      dlt == 1 & eff == 0 ~ 0,
      dlt == 0 & eff == 0 ~ 30,
      dlt == 1 & eff == 1 ~ 60,
      dlt == 0 & eff == 1 ~ 100
    ))
  GPC_analysis_response<-BuyseTest(treatment = "dose", endpoint = c("utility", "sufficient_exposure", "ordinal"), threshold=c(0.1, NA, 0.1),
                                  operator = c(">0", ">0", "<0" ),type=c("c", "b", "c"), data=dataset_utility)
  summary_response<- data.frame(summary(GPC_analysis_response,percentage = FALSE))
  win_response<-sum(summary_response$favorable)
  loss_response<-sum(summary_response$unfavorable)
  tie_response<-sum(summary_response$neutral[length(summary_response$neutral)])
  
  dataset_utility<-data.frame(dataset)%>%
    mutate(utility = case_when(
      dlt == 1 & eff == 0 ~ 0,
      dlt == 0 & eff == 0 ~ 60,
      dlt == 1 & eff == 1 ~ 30,
      dlt == 0 & eff == 1 ~ 100
    ))
  GPC_analysis_dlt<-BuyseTest(treatment = "dose", endpoint = c("utility", "sufficient_exposure", "ordinal"), threshold=c(0.1, NA, 0.1),
                                   operator = c(">0", ">0", "<0" ),type=c("c", "b", "c"), data=dataset_utility)
  summary_dlt<- data.frame(summary(GPC_analysis_dlt,percentage = FALSE))
  win_dlt<-sum(summary_dlt$favorable)
  loss_dlt<-sum(summary_dlt$unfavorable)
  tie_dlt<-sum(summary_dlt$neutral[length(summary_dlt$neutral)])
  
  dataset_utility<-data.frame(dataset)%>%
    mutate(utility = case_when(
      dlt == 1 & eff == 0 ~ 0,
      dlt == 0 & eff == 0 ~ 50,
      dlt == 1 & eff == 1 ~ 50,
      dlt == 0 & eff == 1 ~ 100
    ))
  GPC_analysis_tie<-BuyseTest(treatment = "dose", endpoint = c("utility", "sufficient_exposure", "ordinal"), threshold=c(0.1, NA, 0.1),
                              operator = c(">0", ">0", "<0" ),type=c("c", "b", "c"), data=dataset_utility)
  summary_tie<- data.frame(summary(GPC_analysis_tie,percentage = FALSE))
  win_tie<-sum(summary_tie$favorable)
  loss_tie<-sum(summary_tie$unfavorable)
  tie_tie<-sum(summary_tie$neutral[length(summary_tie$neutral)])

  return(list(response = c(win_response, loss_response, tie_response), dlt = c(win_dlt, loss_dlt, tie_dlt), tie = c(win_tie, loss_tie, tie_tie)))
} 