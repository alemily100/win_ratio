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

####### create flow diagram 

edges <- tibble(
  from = c("A", "A", "A", "C", "C", "C", "F", "F", "F", "I", "I", "I"),
  to   = c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
)

# Define explicit node coordinates
node_data <- tibble::tribble(
  ~name, ~x, ~y, ~count,
  "A", 3, 12, 900,
  "B", 1, 11, doseA[1],
  "C", 3, 11, neutral[1],
  "D", 5, 11, doseB[1],
  "E", 1, 10, doseA[2],
  "F", 3, 10, neutral[2],
  "G", 5, 10, doseB[2],
  "H", 1, 9, doseA[3],
  "I", 3, 9, neutral[3],
  "J", 5, 9, doseB[3],
  "K", 1, 8, doseA[4],
  "L", 3, 8, neutral[4],
  "M", 5, 8, doseB[4],
  "N", 1, 7, loss_4,
  "O", 3, 7, tie_4,
  "P", 5, 7, win_4 
) %>%
  mutate(
    label = count,
    fill = case_when(
      name %in% c("B","E","H","K", "N") ~ "pink",
      name %in% c("D","G","J","M", "P") ~ "green",
      TRUE ~ "lightblue"  # default color
    )
  )

flow<-ggflowchart(edges, node_data = node_data, layout="custom",text_colour = "black", fill = "fill")+
  ggplot2::scale_fill_identity()


final_plot <- ggdraw() +
  draw_plot(flow, x = 0.2, y = 0, width = 0.6, height = 1) +  # shift 20% from left
  draw_label("Text here\nnext to flowchart", x = 0.85, y = 0.8, size = 12, hjust = 0) +
  draw_label("Another label\nlower down", x = 0.85, y = 0.4, size = 12, hjust = 0)
final_plot


###other flow diagram option

edges <- tibble(
  from = c("A", "A", "A", "C", "C", "C", "F", "F", "F", "I", "I", "I"),
  to   = c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
)

# Define explicit node coordinates
node_data <- tibble::tibble(name=c("A","B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P"), 
                            label = c(900,doseA[1], neutral[1], doseB[1],
                                      doseA[2], neutral[2], doseB[2],
                                      doseA[3], neutral[3], doseB[3],
                                      doseA[4], neutral[4], doseB[4],
                                      loss_4, tie_4, win_4))

ggflowchart(edges, node_data)

flow<-ggflowchart(edges, node_data = node_data, layout="custom",text_colour = "black", fill = "fill")+
  ggplot2::scale_fill_identity()
