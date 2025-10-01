setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results")

#scenario1

dlt_priority_scenario1 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario1/wins_dlt_priority_scenario1.csv")[,-1]
response_priority_scenario1 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario1/wins_response_priority_scenario1.csv")[,-1]
tie_priority_scenario1 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario1/wins_tie_priority_scenario1.csv")[,-1]

sc1<-rbind(c("DLT", colMeans(dlt_priority_scenario1)),
c("ties", colMeans(tie_priority_scenario1)),
c("response",colMeans(response_priority_scenario1)))

#scenario2
dlt_priority_scenario2 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario2/wins_dlt_priority_scenario2.csv")[,-1]
response_priority_scenario2 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario2/wins_response_priority_scenario2.csv")[,-1]
tie_priority_scenario2 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario2/wins_tie_priority_scenario2.csv")[,-1]

sc2<-rbind(c("DLT", colMeans(dlt_priority_scenario2)),
           c("ties", colMeans(tie_priority_scenario2)),
           c("response",colMeans(response_priority_scenario2)))

#scenario3
dlt_priority_scenario3 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario3/wins_dlt_priority_scenario3.csv")[,-1]
response_priority_scenario3 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario3/wins_response_priority_scenario3.csv")[,-1]
tie_priority_scenario3 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario3/wins_tie_priority_scenario3.csv")[,-1]

sc3<-rbind(c("DLT", colMeans(dlt_priority_scenario3)),
           c("ties", colMeans(tie_priority_scenario3)),
           c("response",colMeans(response_priority_scenario3)))

#scenario4
dlt_priority_scenario4 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario4/wins_dlt_priority_scenario4.csv")[,-1]
response_priority_scenario4 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario4/wins_response_priority_scenario4.csv")[,-1]
tie_priority_scenario4 <- read.csv("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio/sim_results/scenario4/wins_tie_priority_scenario4.csv")[,-1]

sc4<-rbind(c("DLT", colMeans(dlt_priority_scenario4)),
           c("ties", colMeans(tie_priority_scenario4)),
           c("response",colMeans(response_priority_scenario4)))


write.csv(rbind(sc1, sc2, sc3, sc4), "mean_winlosstie.csv")
