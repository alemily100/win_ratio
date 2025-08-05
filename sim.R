setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio")
source("functions.R")
set.seed(11600)

n.sim<-1000

n.sample_vec<- c(30,30)
vec_dlt_rate<- c(0.267, 0.417)
overall_efficacy<- c(0.8, 0.875)
dlt_efficacy<- c(0.1, 0.6)
correlation_pro_exp<- -0.6
correlation_pro_dlt<- 0.5
vec_exposure_shape<- c(9,9)
vec_exposure_rate<- c(1,2)
mat_fact<- matrix(c(0.059, 0.029, 0.265, 0.382, 0.265, 0.02, 0.03, 0.10, 0.45, 0.4), nrow=2, byrow=TRUE) 
exposure_threshold<- 0.8


test<-replicate(n.sim,
  analysis(n.sample_vec, vec_dlt_rate, overall_efficacy, dlt_efficacy , correlation_pro_exp, correlation_pro_dlt,
           vec_exposure_shape, vec_exposure_rate, mat_fact,  exposure_threshold),
  simplify = FALSE)

four_endpoints <- do.call(rbind, lapply(test, `[[`, "four"))
utility_endpoints <- do.call(rbind, lapply(test, `[[`, "utility"))

wr_four<-sapply(1:n.sim, function (k) four_endpoints[k,1]/four_endpoints[k,2])
wr_utility<-sapply(1:n.sim, function (k) utility_endpoints[k,1]/utility_endpoints[k,2])

hist(wr_four, freq=FALSE, xlim=c(0,4))
lines(density(wr_utility))
