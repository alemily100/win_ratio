setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio")
source("functions.R")

overall_ordinal<- matrix(c(0.059, 0.029, 0.265, 0.382, 0.265, 0.05, 0.1, 0.2, 0.3, 0.35), nrow = 2, byrow = TRUE)
dlt_ordinal<- c(0.05, 0.1, 0.2, 0.3, 0.35)

n.sample_vec<- c(30,35)
vec_dlt_rate<- c(0.25, 0.25)

correlation<- 0.6
vec_exposure_shape<- c(9,8)
vec_exposure_rate<- c(1,3)
exposure_threshold<- 0.8

dataset<-comparison_dataset(n.sample_vec, vec_dlt_rate, c(0.6, 0.3), c(0.4, 0.2), correlation, vec_exposure_shape, vec_exposure_rate, overall_ordinal, dlt_ordinal,  exposure_threshold)


GPC_analysis<-BuyseTest(treatment = "dose", endpoint = c("toxicity", "response", "sufficient_exposure", "ordinal"), threshold=c(NA, NA, NA, 0.5), 
          operator = c("<0", ">0", ">0", "<0" ),type=c("b", "b", "b", "c"), data=dataset)
summary<- data.frame(summary(GPC_analysis,percentage = FALSE))
win<-sum(summary$favorable)
loss<-sum(summary$unfavorable)
tie<-sum(summary$neutral[length(summary$neutral)])

c(win, loss, tie)
 
