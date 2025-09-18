library(ggflowchart)
setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio")
source("functions.R")


analysis_cs<- function(n.sample_vec, vec_dlt_rate, vec_overall_response_rate, vec_dlt_response_rate, 
                       correlation_pro_exp, correlation_pro_dlt, vec_exposure_shape, vec_exposure_rate, mat_fact, exposure_threshold){
  dataset<- comparison_dataset(n.sample_vec, vec_dlt_rate, vec_overall_response_rate, vec_dlt_response_rate, 
                               correlation_pro_exp, correlation_pro_dlt, vec_exposure_shape, vec_exposure_rate, mat_fact, exposure_threshold)
  GPC_analysis_4<-BuyseTest(treatment = "dose", endpoint = c("dlt", "response", "sufficient_exposure", "ordinal"), threshold=c(NA, NA, NA, 0.1),
                            operator = c("<0", ">0", ">0", "<0" ),type=c("b", "b", "b", "c"), data=dataset)
  return(GPC_analysis_4)
} 


n.sample_vec<- c(30,30)
vec_dlt_rate<- c(0.267, 0.417)
overall_efficacy<- c(0.8, 0.875)
dlt_efficacy<- c(0.75, 0.77)
correlation_pro_exp<- -0.6
correlation_pro_dlt<- 0.5
vec_exposure_shape<- c(9,9)
vec_exposure_rate<- c(1,2)
mat_fact<- matrix(c(0.159, 0.229, 0.365, 0.182, 0.065, 0.059, 0.029, 0.265, 0.382, 0.265), nrow=2, byrow=TRUE) 

old<-c(0.159, 0.129, 0.365, 0.182, 0.165)

exposure_threshold<- 0.8
#set.seed(11600)
set.seed(11600)
case_study<-
  analysis_cs(n.sample_vec, vec_dlt_rate, overall_efficacy, dlt_efficacy , correlation_pro_exp, correlation_pro_dlt,
              vec_exposure_shape, vec_exposure_rate, mat_fact,  exposure_threshold)

doseA<- case_study@count.unfavorable
neutral<-case_study@count.neutral
doseB<-case_study@count.favorable

summary_4<- data.frame(summary(case_study,percentage = FALSE))
win_4<-sum(summary_4$favorable)
loss_4<-sum(summary_4$unfavorable)
tie_4<-sum(summary_4$neutral[length(summary_4$neutral)])
win_4/loss_4

#Estimated WR
confint(case_study, statistic = "winRatio")