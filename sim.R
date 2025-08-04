setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio")
source("functions.R")

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
