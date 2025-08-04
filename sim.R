setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio")
source("functions.R")
set.seed(11600)

n.sim<-1000

overall_ordinal<- matrix(c(0.059, 0.029, 0.265, 0.382, 0.265, 0.05, 0.1, 0.2, 0.3, 0.35), nrow = 2, byrow = TRUE)
dlt_ordinal<- c(0.05, 0.1, 0.2, 0.3, 0.35)

n.sample_vec<- c(30,30)
vec_dlt_rate<- c(0.25, 0.35)

correlation<- 0.6
vec_exposure_shape<- c(9,9)
vec_exposure_rate<- c(1,1.5)

#probability not sufficient intensity
pbeta(0.8, 9, 1)
pbeta(0.8, 9, 1.5)

exposure_threshold<- 0.8
overall_efficacy<-c(0.6, 0.6)
dlt_efficacy<- c(0.4, 0.4)

test<-sapply(1:n.sim, function (k) 
  analysis(n.sample_vec, vec_dlt_rate, overall_efficacy, dlt_efficacy , correlation, vec_exposure_shape, vec_exposure_rate, overall_ordinal, dlt_ordinal,  exposure_threshold))

wr<-sapply(1:n.sim, function (k) test[1,k]/test[2,k])
plot(hist(wr))
