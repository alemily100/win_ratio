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

prob_no_dlt<- function(overall_prob, prob_for_dlt, dlt_rate){
  val<- (overall_prob - (prob_for_dlt*dlt_rate))/(1-dlt_rate)
  return(val)
}

#vector of response rates: [1]: no DLT, [2] DLT
#mat_fact: matrix [1,] no DLT [2,] DLT
endpoints<- function(n.sample, dlt_rate, vec_response_rate,correlation, exposure_shape, exposure_rate, mat_fact, exposure_threshold){
  fact_threshold<- c(0, cumsum(mat_fact[1,]))
  toxicity<- dlt(n.sample, dlt_rate)
  response<- sapply(toxicity, function (k) rbinom(1, 1, vec_response_rate[k+1]))
  exp_pro<-exposure_fact(n.sample, correlation, exposure_shape, exposure_rate, fact_threshold, exposure_threshold)
  id<- 1:n.sample
  M<- cbind(id, toxicity, response, 
            exp_pro)
  M[which(toxicity==1),4]<- FALSE
  M[which(toxicity==1),5]<- sample(1:5, size=sum(toxicity), replace=TRUE, prob=mat_fact[2,])
  return(M)
}

#Assume for now we have the same correlation between PROs and dose-discontinuation for both doses
#n.sample_vec: [1] sample size for dose A and [2] sample size for dose B
#vec_overall_response_rate: [1] overall response rates for dose A and [2] overall response rates for dose B. 
#vec_dlt_response_rate: [1] dlt response rates for dose A and [2] dlt response rates for dose B. 
#mat_overall_ordinal: [1,] overall fact for dose A and [2,] overall fact for dose B

comparison_dataset<- function(n.sample_vec, vec_dlt_rate, vec_overall_response_rate, vec_dlt_response_rate, 
                              correlation, vec_exposure_shape, vec_exposure_rate, mat_overall_ordinal, vec_dlt_ordinal, exposure_threshold){
  #creating response matrices
  response_no_dlt_A<- prob_no_dlt(vec_overall_response_rate[1], vec_dlt_response_rate[1], vec_dlt_rate[1])
  response_no_dlt_B<- prob_no_dlt(vec_overall_response_rate[2], vec_dlt_response_rate[2], vec_dlt_rate[2])
  mat_response_rate<- matrix(
    c(response_no_dlt_A, vec_dlt_response_rate[1], response_no_dlt_B, vec_dlt_response_rate[2]), nrow = 2, byrow = TRUE)
  
  #creating PRO matrices
  fact_no_dlt_A<- sapply(1:5, function (k) prob_no_dlt((mat_overall_ordinal[1,])[k], (vec_dlt_ordinal)[k], vec_dlt_rate[1]))
  fact_no_dlt_B<- sapply(1:5, function (k) prob_no_dlt((mat_overall_ordinal[2,])[k], (vec_dlt_ordinal)[k], vec_dlt_rate[2]))
  
  list_fact<- list()
  ordinal_mat_A <- matrix(
    c(fact_no_dlt_A,
      vec_dlt_ordinal),
    nrow = 2, byrow = TRUE)
  ordinal_mat_B <- matrix(
    c(fact_no_dlt_B,
      vec_dlt_ordinal),
    nrow = 2, byrow = TRUE)
  list_fact[[1]]<- ordinal_mat_A
  list_fact[[2]]<- ordinal_mat_B
  
  doseA<- endpoints(n.sample_vec[1], vec_dlt_rate[1], mat_response_rate[1,], correlation, vec_exposure_shape[1], vec_exposure_rate[1], list_fact[[1]], exposure_threshold)
  doseB<- endpoints(n.sample_vec[2], vec_dlt_rate[2], mat_response_rate[2,], correlation, vec_exposure_shape[2], vec_exposure_rate[2], list_fact[[2]], exposure_threshold)
  doseA<- cbind(doseA, dose=rep(1, times=n.sample_vec[1]))
  doseB<- cbind(doseB, dose=rep(2, times=n.sample_vec[2]))
  M<- rbind(doseA, doseB)
  return(M)
}